"""
Localize the `CUDA error: invalid configuration argument` seen during UNet3+
training under mixed precision.

Two modes:

  # (A) Full model step (mirrors HPC config). Crashes in backward.
  CUDA_LAUNCH_BLOCKING=1 python debug_amp_repro.py --precision bf16
  CUDA_LAUNCH_BLOCKING=1 python debug_amp_repro.py --precision fp32   # control

  # (B) Op-isolation: run each suspect op's backward on its own and report
  #     exactly which one launches the bad kernel.
  CUDA_LAUNCH_BLOCKING=1 python debug_amp_repro.py --op-test --precision bf16
"""
import argparse
import torch
import torch.nn.functional as F

p = argparse.ArgumentParser()
p.add_argument("--precision", choices=["fp16", "bf16", "fp32"], default="bf16")
p.add_argument("--op-test", action="store_true", help="run per-op isolation instead of full model")
p.add_argument("--no-ds", action="store_true", help="disable deep supervision (full-model mode)")
p.add_argument("--anomaly", action="store_true",
               help="enable autograd anomaly detection: names the forward line that created the failing backward node")
p.add_argument("--batch-size", type=int, default=32)
args = p.parse_args()

assert torch.cuda.is_available(), "no CUDA device"
dev = torch.device("cuda")
amp_dtype = {"fp16": torch.float16, "bf16": torch.bfloat16, "fp32": torch.float32}[args.precision]
autocast = args.precision != "fp32"
print(f"device: {torch.cuda.get_device_name(0)}  torch: {torch.__version__}  precision: {args.precision}")


def trial(name, fn):
    """Run fn() under autocast, do a backward, sync, report pass/fail per op."""
    try:
        with torch.autocast(device_type="cuda", dtype=amp_dtype, enabled=autocast):
            out = fn()
        out.float().sum().backward()
        torch.cuda.synchronize()
        print(f"  PASS  {name}")
    except RuntimeError as e:
        print(f"  FAIL  {name}: {str(e).splitlines()[0]}")
    torch.cuda.synchronize() if torch.cuda.is_available() else None


if args.op_test:
    B = args.batch_size
    print(f"\nop-isolation (batch={B}):")

    # 1. bilinear upsample backward at every deep-supervision / UnifyBranch scale
    for src, ch in [(16, 320), (32, 320), (64, 320), (128, 320), (16, 4), (32, 4), (64, 4), (128, 4)]:
        x = torch.randn(B, ch, src, src, device=dev, requires_grad=True)
        trial(f"interpolate bilinear {src}->256 ch={ch}",
              lambda x=x: F.interpolate(x, size=(256, 256), mode="bilinear", align_corners=False))

    # 2. nearest upsample backward (candidate replacement)
    x = torch.randn(B, 320, 16, 16, device=dev, requires_grad=True)
    trial("interpolate nearest 16->256 ch=320",
          lambda x=x: F.interpolate(x, size=(256, 256), mode="nearest"))

    # 3. strided max_pool2d backward (our downsample path)
    x = torch.randn(B, 512, 32, 32, device=dev, requires_grad=True)
    trial("max_pool2d 32->16 ch=512", lambda x=x: F.max_pool2d(x, kernel_size=2))

    # 4. adaptive_avg_pool2d -> 1 backward (SqueezeExcitation squeeze)
    x = torch.randn(B, 320, 256, 256, device=dev, requires_grad=True)
    trial("adaptive_avg_pool2d->1 ch=320 @256", lambda x=x: F.adaptive_avg_pool2d(x, 1))

    # 5. big conv backward at full res (decoder fuse)
    conv = torch.nn.Conv2d(320, 320, 3, padding=1).to(dev)
    x = torch.randn(B, 320, 256, 256, device=dev, requires_grad=True)
    trial("conv2d 320->320 @256", lambda x=x: conv(x))
    print("\nop-isolation done")
else:
    from dl_03_unet3plus_model import create_unet3plus
    model = create_unet3plus(
        in_channels=26, num_classes=4, base_filters=64, depth=4,
        cat_channels=64, deep_supervision=not args.no_ds, device=dev,
    )
    model.train()
    opt = torch.optim.AdamW(model.parameters(), lr=1e-4)
    x = torch.randn(B := args.batch_size, 26, 256, 256, device=dev)
    y = torch.randint(0, 4, (B, 256, 256), device=dev)
    scaler = torch.amp.GradScaler("cuda", enabled=args.precision == "fp16")
    for step in range(2):
        opt.zero_grad(set_to_none=True)
        with torch.autocast(device_type="cuda", dtype=amp_dtype, enabled=autocast):
            outs = model(x)
            outs = outs if isinstance(outs, list) else [outs]
            loss = sum(F.cross_entropy(o, y) for o in outs)
        scaler.scale(loss).backward()
        scaler.step(opt)
        scaler.update()
        torch.cuda.synchronize()
        print(f"step {step} OK  loss={loss.item():.4f}")
    print("DONE — no crash")
