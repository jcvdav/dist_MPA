# dist_MPA

Analyzes changes in the spatial distribution and inequality of fishing effort in response to the 2017 expansion of the Revillagigedo Islands MPA in Mexico. Fishing effort is characterized using VMS (Vessel Monitoring System) tracks, and spatial inequality is measured via Gini coefficients and Lorenz curves.

## Pipeline

```
scripts/
├── 01_collection/      # Acquire raw data (EEZ, MPA boundaries, VMS tracks, landings)
├── 02_processing/      # Clean and prepare data for analysis
└── 03_analysis/        # Gini, Lorenz curves, and changes in effort
```

### 01_collection

| Script | Description |
|---|---|
| `01_get_mex_eez.R` | Download Mexico's EEZ boundary |
| `02_get_revilla_polygons.R` | Get old and new Revillagigedo MPA boundaries from WDPA |
| `03_make_distance_to_MPA.R` | Build distance-to-MPA raster |
| `04_make_port_locations.R` | Define port locations |
| `05_get_tracks.R` | Query VMS tracks from BigQuery |
| `06_get_landings_data.R` | Process tuna landings data |

### 02_processing

| Script | Description |
|---|---|
| `01_clean_vessel_info.R` | Clean vessel metadata |
| `02_score_tracks.R` | Classify VMS pings as fishing / not fishing (k-means); derive year, inside, after flags |
| `03_process_tracks.R` | Assign treatment groups (displaced vs. not displaced) based on pre-expansion fishing inside the MPA; exports `processed_tracks.rds` |

### 03_analysis

| Script | Description |
|---|---|
| `01_lorenz_curves.R` | Lorenz curves and Gini coefficients for fishing effort |
| `02_lorenz_curves_revilla.R` | Lorenz curves specific to the Revillagigedo region |
| `03_spatial_gini_vs_resolution.R` | Sensitivity of Gini to spatial resolution |
| `04_spatial_gini.R` | Spatial Gini analysis at chosen resolution |
| `05_changes_in_effort.R` | Event study of effort changes within the spillover ring; concave hulls for displaced vs. non-displaced fishing grounds; summary stats for abstract |

## ML Classifier (to be ported)

The `ML_train/` folder contains scripts for a separate project: training a Random Forest classifier to predict fishing behavior using onboard observer data. See [`ML_train/README.md`](ML_train/README.md) for details.
