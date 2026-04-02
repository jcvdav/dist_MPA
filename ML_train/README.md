# ML Classifier — Fishing / Not Fishing

This folder contains scripts for training a Random Forest classifier that predicts fishing vs. non-fishing activity from VMS tracks, using onboard observer data as ground truth.

It will be ported to its own repository. When splitting, the following data files should move with it:

## Scripts

| Script | Description |
|---|---|
| `01_clean_onboard_observer_data.R` | Clean raw observer data from the Tuna Trust project |
| `02_build_training_data.R` | Match VMS pings to observed fishing sets to build training dataset |
| `03_train_classifier.R` | Tune and train the Random Forest model (tidymodels / ranger) |

## Data files to migrate

**Raw:**
- `data/raw/tuna_trust/` — Observer fishing set data (2013–2014)

**Processed:**
- `data/processed/labeled_tracks.rds` — VMS pings with fishing/not-fishing labels
- `data/processed/onboard_observer_data.rds` — Cleaned observer data

**Models:**
- `results/models/final_rf_model.rds` — Trained Random Forest model
- `results/models/tune_results.rds` — Hyperparameter tuning results
