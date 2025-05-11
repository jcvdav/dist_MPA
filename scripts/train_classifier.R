################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################
## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  ranger,
  future,
  tidymodels
)

# Load data --------------------------------------------------------------------
data <- readRDS(here("data", "processed", "labeled_tracks.rds")) |> 
  mutate(fishing = ifelse(likely_fishing, "fishing", "not_fishing"),
         fishing = factor(fishing)) |> 
  select(-c(likely_fishing, hours))

## PROCESSING ##################################################################

# Split sample -----------------------------------------------------------------
set.seed(1)
data_split <- initial_split(data, prop = 3/4, strata = fishing)

# Create data frames for the two sets:
# Training
train_data <- training(data_split)
# Testing
test_data  <- testing(data_split)

# 10-fold CV
folds <- vfold_cv(train_data, v = 10, strata = fishing)

# Preprocessing ----------------------------------------------------------------
rec <- recipe(fishing ~ ., #lat + lon + implied_speed_knots + course + distance_to_last_m,
              data = train_data) |> 
  update_role(seg_id, point_in_seg, vessel_rnpa, eu_rnpa, trip, set_id, new_role = "ID") |> 
  step_date(date, features = c("dow", "month"), keep_original_cols = F) |> 
  step_time(datetime, features = c("hour"), keep_original_cols = F) |> 
  step_zv(all_predictors())

# Define model -----------------------------------------------------------------
lr_mod <-
  logistic_reg() %>%
  set_engine("glm")

rf_mod <- rand_forest() |> 
  set_mode("classification") |> 
  set_engine("ranger",
             num.threads = 1,
             importance = "impurity") |> 
  set_args(mtry = tune(),
           trees = tune(),
           min_n = tune())

# Define workflows -------------------------------------------------------------
lr_wf <- workflow() |>
  add_recipe(rec) |>
  add_model(lr_mod)

rf_wf <- workflow() |> 
  add_recipe(rec) |> 
  add_model(rf_mod)

# Hyperparameter tuning --------------------------------------------------------
# Define grid for hyperparameter tuning
rf_grid <- 
  grid_space_filling(
    min_n(range = c(5, 30)), 
    mtry(range = c(3, 9)), 
    trees(range = c(10, 1000)), 
    size = 30)

# plot(rf_grid)
metrics <- metric_set(accuracy, sens, spec, precision, recall, f_meas)

plan(multisession, workers = 15)
set.seed(2)
tune_res <- 
  rf_wf %>% 
  tune_grid(resamples = folds,
            grid = rf_grid, 
            metrics = metrics,
            control = control_grid(verbose = TRUE))

# Find best combination of parameters ------------------------------------------
collect_metrics(tune_res)
autoplot(tune_res)
final_wf <- rf_wf |> 
  finalize_workflow(show_best(x = tune_res,
                              metric = "accuracy",
                              n = 1))

# Fit best combination of parameters -------------------------------------------
lr_fit <- lr_wf |>
  fit(data = train_data)

rf_fit <- final_wf |> 
  fit(data = train_data)


## EVALUATE PERFORMANCE ########################################################
# Augment 
lr_aug <-
  augment(lr_fit, test_data)

rf_aug <- 
  augment(rf_fit, test_data)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
metrics(lr_aug, truth = fishing, estimate = .pred_class)
lr_aug |>
  roc_auc(truth = fishing, .pred_fishing)
lr_aug |>
  roc_curve(truth = fishing, .pred_fishing) %>%
  autoplot()

# For the random forest
# Metrics before optimizing thershold for F1
metrics(rf_aug, truth = fishing, estimate = .pred_class)

# Based on pred_fishing
rf_aug |> 
  roc_auc(truth = fishing, .pred_fishing)
rf_aug |>  
  roc_curve(truth = fishing, .pred_fishing) %>% 
  autoplot()

## optimize threshold ---------------------------------------------------------
prob_preds <- predict(rf_fit, new_data = test_data, type = "prob") %>%
  bind_cols(test_data)  # attach true labels

# Calculate F1 score for each threshold
threshold_metrics <-  seq(0, 1, by = 0.01) %>%
  map_dfr(~{
    prob_preds %>%
      mutate(.pred_class = if_else(.pred_fishing >= .x, "fishing", "not_fishing")) %>%
      summarise(f1 = f_meas_vec(truth = fishing, estimate = factor(.pred_class, levels = levels(fishing))),
                threshold = .x)
  })

best_threshold <- threshold_metrics %>%
  filter(f1 == max(f1, na.rm = TRUE))

ggplot(threshold_metrics, aes(x = threshold, y = f1)) +
  geom_point() +
  geom_point(data = best_threshold, aes(x = threshold, y = f1), color = "red", size = 3) +
  labs(title = "F1 Score vs Threshold", x = "Threshold", y = "F1 Score")


# Print the best threshold and corresponding F1 score
print(best_threshold)

mod_aug <- rf_aug |> 
  mutate(.pred_class = ifelse(.pred_fishing >= 0.28, "fishing", "not_fishing"),
         .pred_class = factor(.pred_class, levels = levels(fishing)))

# Metrics after optimizing thershold for F1
metrics(mod_aug, truth = fishing, estimate = .pred_class)

## EXPORT ######################################################################
saveRDS(rf_fit, file = here("results", "models", "final_rf_model.rds"))
saveRDS(tune_res, file = here("results", "models", "tune_results.rds"))
