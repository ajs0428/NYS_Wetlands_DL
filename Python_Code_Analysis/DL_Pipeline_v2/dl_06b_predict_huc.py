"""
dl_06b_predict_huc.py

Mapped predictions for a whole HUC WITHOUT building a *_stack.tif on disk.

This is dl_06_predict.py's sibling: instead of reading one pre-stacked GeoTIFF,
it assembles the predictor stack in memory from the canonical source rasters
(dl_huc_stack.VirtualHucStack, the Python port of huc_stack.R) and runs the same
sliding-window inference loop (predict_from_source). Source rasters are read
window-by-window and aligned to the DEM grid on the fly, so nothing heavy is
duplicated -- you sync the source datasets to the GPU server once and reuse them
for every HUC and every model.

Example:
    python dl_06b_predict_huc.py --huc 041402011002 --cluster 208 \
        --data-root /scratch/NYS_Wetlands_Data \
        --model Models/best_model.safetensors \
        --stats Data/Training_Data/binary_normalization_stats.json \
        --out-dir Data/HUC_DL_Predictions

Inspect the resolved sources / band contract first (no model needed):
    python dl_huc_stack.py --huc 041402011002 --cluster 208 \
        --data-root /scratch/NYS_Wetlands_Data --inspect
"""

import argparse
from pathlib import Path

from dl_03_unet_model import get_device
from dl_band_utils import default_stats_path
from dl_model_utils import assert_mode_matches, load_model
from dl_06_predict import load_normalization_stats, predict_from_source
from dl_huc_stack import open_huc_stack


def main(
    huc: str,
    cluster: str,
    model_path: Path,
    stats_path: Path,
    out_dir: Path,
    data_root: Path = None,
    patch_size: int = 128,
    overlap: int = 64,
    base_filters: int = 32,
    depth: int = 4,
    save_probabilities: bool = False,
    use_aspp: bool = False,
    aspp_rates: tuple = (6, 12, 18),
):
    device = get_device()
    print(f"Using device: {device}")

    # Load normalization stats (defines predictor-name contract + in_channels)
    stats = load_normalization_stats(stats_path)
    in_channels = stats["in_channels"]
    num_classes = len(stats["class_names"])
    mode = stats.get("classification_mode", "multiclass")

    # Output name mirrors the R prediction convention, with mode suffix.
    out_dir.mkdir(parents=True, exist_ok=True)
    output_path = out_dir / f"DLpred_{mode}_cluster_{cluster}_huc_{huc}.tif"

    # Verify model and stats agree on classification mode before loading.
    assert_mode_matches(model_path, mode)

    print(f"\nLoading model from {model_path}")
    model = load_model(model_path, device, in_channels, num_classes, base_filters, depth,
                       use_aspp=use_aspp, aspp_rates=aspp_rates)

    print(f"\nAssembling virtual stack for HUC {huc} (cluster {cluster}) ...")
    with open_huc_stack(huc, cluster, data_root=data_root) as stack:
        print(f"Virtual stack: {len(stack.descriptions)} bands, "
              f"{stack.width} x {stack.height}")
        predict_from_source(
            model=model,
            src=stack,
            output_path=output_path,
            stats=stats,
            device=device,
            patch_size=patch_size,
            overlap=overlap,
            save_probabilities=save_probabilities,
        )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Mapped wetland predictions for a HUC, stack assembled in memory.")
    parser.add_argument("--huc", required=True, help="HUC id (e.g. 041402011002)")
    parser.add_argument("--cluster", required=True, help="cluster number (e.g. 208)")
    parser.add_argument("--data-root", type=Path, default=None,
                        help="Root of the NYS_Wetlands_Data project "
                             "(default: $NYS_WETLANDS_DATA_ROOT or sibling project)")
    parser.add_argument("--model", type=Path, default=Path("Models/best_model.safetensors"))
    parser.add_argument("--stats", type=Path, default=default_stats_path())
    parser.add_argument("--out-dir", type=Path, default=Path("Data/HUC_DL_Predictions"),
                        help="Directory for the output classification GeoTIFF")
    parser.add_argument("--patch-size", type=int, default=128)
    parser.add_argument("--overlap", type=int, default=64)
    parser.add_argument("--base-filters", type=int, default=32,
                        help="Model base filters (auto-detected from checkpoint if available)")
    parser.add_argument("--depth", type=int, default=4,
                        help="Model depth (auto-detected from checkpoint if available)")
    parser.add_argument("--probs", action="store_true", help="Save probability maps")
    parser.add_argument("--use-aspp", action="store_true",
                        help="Model uses ASPP (auto-detected from checkpoint if available)")
    parser.add_argument("--aspp-rates", type=int, nargs="+", default=[6, 12, 18],
                        help="ASPP dilation rates (auto-detected from checkpoint if available)")

    args = parser.parse_args()

    # Resolve project-relative model/stats paths against the DL project root.
    project_root = Path(__file__).parent.parent.parent
    model_path = project_root / args.model if not args.model.is_absolute() else args.model
    stats_path = project_root / args.stats if not args.stats.is_absolute() else args.stats

    main(
        huc=args.huc,
        cluster=args.cluster,
        model_path=model_path,
        stats_path=stats_path,
        out_dir=args.out_dir,
        data_root=args.data_root,
        patch_size=args.patch_size,
        overlap=args.overlap,
        base_filters=args.base_filters,
        depth=args.depth,
        save_probabilities=args.probs,
        use_aspp=args.use_aspp,
        aspp_rates=tuple(args.aspp_rates),
    )
