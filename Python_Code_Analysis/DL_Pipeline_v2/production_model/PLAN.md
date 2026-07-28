# Production Wetland Model — Plan

**Status:** scaffolding built (2026-07-27); no production run executed yet.

## 1. Why this exists (and why it is not a branch)

The factorial answers *which inputs and labels matter*. It deliberately trains 48
short, comparable models and throws them away — none is a deliverable. This
workstream answers the next question: **given the factorial's answer, what do we
ship?**

It shares 100% of its machinery with the factorial (`dl_01`–`dl_06`,
`dl_02_dataset.py`, `dl_03_unet_model.py`, `dl_losses.py`, `dl_model_factory.py`).
So it is *not* a git branch — a long-lived branch over shared core would mean every
`dl_02`/`dl_03` fix had to be applied and reconciled twice. It is separated the way
every other workstream here is separated: **its own directory, its own results
root, its own config module**, on the same trunk. See CLAUDE.md § "Repo & branching".

## 2. The recipe

Single source of truth: `dl_prod_config.py` (`RECIPE`). Run it with no arguments to
print and self-check the resolved recipe.

| Knob | Value | Why |
|---|---|---|
| config | `nwifield_chmret_leafoff` | Best factorial-v2 arm on every headline wetland metric |
| mode | `multiclass` | Ship 4-class; binary is derivable by collapsing EMW/FSW/SSW |
| arch | `unet`, bf64, d5 | Held at the factorial's values |
| loss | weighted CE (`ce=1.0, dice=0.0, gamma=0`) | Held (set in `run_config.sh`) |
| epochs | 100 | The **one** deliberate departure from the factorial's 50 |
| batch / precision | 16 / `16-mixed` | Held |
| leakage guard | `huc12` | Held |
| seeds | 0, 1, 2 | Ship the best by field-test macro F1 (see §4) |

**Why `nwifield_chmret_leafoff`** — from `Models/factorial_results_v2/analysis/cross_mode_summary.csv`:

| config | WET IoU (mc-collapsed) | WET IoU (binary) | WET recall (mc) |
|---|---|---|---|
| **nwifield_chmret_leafoff** | **0.659** | **0.668** | **0.870** |
| fld_chmret_leafoff | 0.654 | 0.667 | 0.848 |
| flddeg_chmret_leafoff | 0.634 | 0.674 | 0.827 |
| nwiextra_chmret_leafoff | 0.626 | 0.637 | 0.835 |
| nwi_chmret_leafoff | 0.586 | 0.613 | 0.739 |

It is the hybrid pool — field labels on the field footprints **union** the NWIextra
patches outside the test HUC12s — on the full 26-channel feature set. The margin
over pure-field is small in IoU (+0.005) but clearer in recall (+0.022), which is
the metric that matters for a mapping product that must not miss wetlands.

**Why 100 epochs is the only change.** The grid capped epochs to fit 48 cells in a
reservation, not because 50 was optimal. Everything else is held so the benchmark's
ranking remains valid evidence for the shipped model — change more knobs and the
factorial stops justifying the choice.

## 3. What it produces

```
Models/production_model/multiclass/production/seed<k>/
  best_*.safetensors + .meta.json    <- the deployable artifact
  best_*.ckpt
  metrics.json, confusion_matrix.csv <- field-test scores (same held-out field patches)
  manifest.json                      <- full provenance incl. git_commit
  train.log, training_log.json
```

Evaluation is unchanged from the factorial: **always against field labels** on the
seed's held-out field patches, so production scores are directly comparable to the
benchmark table above.

## 4. Open decisions (deliberately NOT pre-empted)

1. **Ship one seed or ensemble three?** Current scaffolding trains three and leaves
   the choice open. Best-single is simpler to deploy and matches how
   `run_predict_factorial.sh` already picks a checkpoint; a 3-model softmax average
   would likely gain a little accuracy for 3× inference cost.
2. **Train on the full pool for the final artifact?** Standard practice once a
   recipe is locked: refit on train+val+test to use every labelled pixel. This is
   *not* implemented — it needs a trainer flag, and it forfeits the held-out score
   that makes the model auditable. Recommend shipping the held-out-validated model
   first, and treating a full-pool refit as a separate, clearly-labelled artifact.
3. **Retraining cadence** as new annotated patches land. The factorial's patch-count
   learning curve (`Models/results_patchcurve_v2`) is the evidence base for whether
   more patches are still buying accuracy.

## 5. Not in scope here

Inference over HUCs, COG generation, and the web map are separate concerns that
already have their own paths (`run_predict_factorial.sh`, `python_make_cogs.py`,
`webmap/`). This workstream ends at a validated checkpoint.
