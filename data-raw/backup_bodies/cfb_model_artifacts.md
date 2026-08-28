Canonical CFB EPA/WPA + decision-surface model artifacts, read by **both**
`sportsdataverse-py` and `cfbfastR`. Sibling of `nfl_model_artifacts`.

Publishing here is the single change that updates both libraries — see
[cfbfastR#138](https://github.com/sportsdataverse/cfbfastR/issues/138).

## Contents

| Asset | Features | Objective |
|---|---:|---|
| `ep_model.ubj` | 8 | `multi:softprob` (7 next-score classes) |
| `wp_naive.ubj` | 12 | `binary:logistic` |
| `wp_spread.ubj` | 13 | `binary:logistic` |
| `fg_model.ubj` | 5 | `binary:logistic` |
| `cfb_cp_model.ubj` | 8 | `binary:logistic` |
| `qbr_model.ubj` | 10 | `reg:squarederror` |
| `fd_model.ubj` | 9 | `multi:softprob` (76-class yardage) |
| `two_pt_model.ubj` | 4 | `binary:logistic` |
| `xpass_model.ubj` | 7 | `binary:logistic` |
| `punt_distribution.parquet`, `cfb_field_position_ep.parquet` | — | lookup tables |

Plus `*.card.json` model cards and **`MANIFEST.json`**.

## Read MANIFEST.json before consuming

It carries `model_version`, per-asset `sha256`, the introspected feature
contract for every booster, and — critically — the **EP class contract**.

`ep_model` emits 7 softmax columns in the order
`TD, Opp_TD, FG, Opp_FG, Safety, Opp_Safety, No_Score` with point values
`7, -7, 3, -3, 2, -2, 0`; `EP = sum(prob * point_value)`.

⚠️ **cfbfastR's historical `ep_model$lev` uses a different order**
(`No_Score, FG, Opp_FG, Opp_Safety, Opp_TD, Safety, TD`) **with no fixed point
against this one.** A consumer that assumes its own ordering produces EP that is
wrong but sits in a plausible range — it will not trip a range check. The
manifest publishes `permutation_to_cfbfastR_lev_1based = [7,3,4,6,2,5,1]` for
consumers keeping cfbfastR's positional weights `c(0,3,-3,-2,-7,2,7)`.

## Verifying

`verify_manifest.R` (in the spec repo) checks every checksum, loads every
booster in R, asserts feature-name agreement, and asserts the EP permutation
reproduces cfbfastR's weights. Requires R `xgboost` >= 3.x — the older
`xgb_*.model` binary artifacts in `cfbfastR-data/models/` use a format removed
in xgboost 3.1 and are superseded by this bundle.