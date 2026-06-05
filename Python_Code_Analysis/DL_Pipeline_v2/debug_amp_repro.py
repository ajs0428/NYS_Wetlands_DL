"""
Minimal GPU repro to localize the `CUDA error: invalid configuration argument`
seen during UNet3+ training under 16-mixed.

Run inside the container with launch blocking so the traceback points at the
REAL failing kernel (not the async backward() frame):

    CUDA_LAUNCH_BLOCKING=1 python debug_amp_repro.py
    CUDA_LAUNCH_BLOCKING=1 python debug_amp_repro.py --precision bf16   # compare
    CUDA_LAUNCH_BLOCKING=1 python debug_amp_repro.py --precision fp32   # compare

Mirrors the HPC config: bf=64, depth=4, cat_channels=64, deep supervision,
batch 32, 26 input channels, 256x256.
"""
import argparse
import torch
import torch.nn.functional as F

from dl_03_unet3plus_model import create_unet3plus

p = argparse.ArgumentParser()
p.add_argument("--precision", choices=["fp16", "bf16", "fp32"], default="fp16")
p.add_argument("--batch-size", type=int, default=32)
p.add_argument("--base-filters", type=int, default=64)
p.add_argument("--depth", type=int, default=4)
p.add_argument("--cat-channels", type=int, default=64)
args = p.parse_args()

assert torch.cuda.is_available(), "no CUDA device"
dev = torch.device("cuda")
print(f"device: {torch.cuda.get_device_name(0)}  torch: {torch.__version__}  precision: {args.precision}")

model = create_unet3plus(
    in_channels=26, num_classes=4,
    base_filters=args.base_filters, depth=args.depth,
    cat_channels=args.cat_channels, deep_supervision=True, device=dev,
)
model.train()
opt = torch.optim.AdamW(model.parameters(), lr=1e-4)

x = torch.randn(args.batch_size, 26, 256, 256, device=dev)
y = torch.randint(0, 4, (args.batch_size, 256, 256), device=dev)

amp_dtype = {"fp16": torch.float16, "bf16": torch.bfloat16, "fp32": torch.float32}[args.precision]
use_scaler = args.precision == "fp16"
scaler = torch.cuda.amp.GradScaler(enabled=use_scaler)

for step in range(3):
    opt.zero_grad(set_to_none=True)
    with torch.autocast(device_type="cuda", dtype=amp_dtype, enabled=args.precision != "fp32"):
        outs = model(x)                      # list of full-res heads (deep supervision)
        loss = sum(F.cross_entropy(o, y) for o in outs)
    scaler.scale(loss).backward()
    scaler.step(opt)
    scaler.update()
    torch.cuda.synchronize()
    print(f"step {step} OK  loss={loss.item():.4f}")

print("DONE — no crash")
