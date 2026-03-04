#!/usr/bin/env Rscript

args = c(
    "Data/Training_Data/HUC_Extracted_Training_Data/", #1
    "MOD_CLASS", #2
    "Models/RF_model_output" #3
)
args = commandArgs(trailingOnly = TRUE) # arguments are passed from terminal to here

cat("these are the arguments: \n", 
    "- Path to training data files (should be multiple):", args[1], "\n",
    "- Class to predict on (should be MOD_CLASS or COARSE_CLASS)", args[2], "\n",
    "- Path to model output", args[3]
)


# Random Forest Training Pipeline with tidymodels
# Features: Train/Val/Test split, RFE, Hyperparameter Tuning, Parallel Processing

# === Setup ===
suppressPackageStartupMessages({
    library(tidymodels)
    library(ranger)
    library(future)
    library(doFuture)
    library(vip)
    library(sf)
    library(here)
})

#tidymodels_prefer()
set.seed(11)

# Enable parallel processing with future (SLURM-aware)
# Detect cores: SLURM env var > detectCores
if(future::availableCores() > 16){
    corenum <-  8
} else {
    corenum <-  (future::availableCores())
}
corenum <- max(1, corenum)

# plan(multisession, workers = corenum)
plan(future.callr::callr, workers = corenum)

# Set future options
options(future.globals.maxSize= 16 * 1e9)  


cat(sprintf("Using %d workers with plan: %s\n\n", corenum, class(plan())[1]))

###################################################################################################
# === Load Data ===
list_of_pts_extracted_locs <- list.files(here(args[1]), pattern = ".gpkg$", full.names = TRUE, recursive = FALSE)
data <- lapply(list_of_pts_extracted_locs, st_read, quiet = TRUE) |> 
    bind_rows() |> 
    as_tibble() |> 
    dplyr::mutate(across(where(is.character), as.factor),
                  twi = case_when(is.infinite(twi) ~ NA,
                                  .default = twi)
    ) |> 
    dplyr::select(-geom) |> 
    drop_na()

target_var <- args[2]
if(target_var == "MOD_CLASS"){
    data <- data |> select(-COARSE_CLASS)
} else if(target_var == "COARSE_CLASS") {
    data <- data |> select(-MOD_CLASS)
}

# data_sf <- lapply(list_of_pts_extracted_locs, st_read, quiet = TRUE) |>
#     bind_rows() |>
#     dplyr::mutate(across(where(is.character), as.factor),
#                   twi = case_when(is.infinite(twi) ~ NA,
#                                   .default = twi)
    # ) 
# st_write(data_sf, paste0("Data/Training_Data/Combined_Training_Datasets/", args[2], "_sf_points_for_RFM_FieldLocs.gpkg"))

cat("Target variable:", target_var, "\n")
cat("Dataset dimensions:", nrow(data), "x", ncol(data), "\n")
cat("Features:", paste(names(data), collapse = ", "), "\n\n")

# Check number of classes for later metric handling
n_classes <- length(levels(data[[target_var]]))
cat(sprintf("Number of classes: %d\n\n", n_classes))
###################################################################################################

# === Step 1: Split Data (60% train, 20% validation, 20% test) ===
cat("=== Step 1: Data Splitting ===\n")

initial_split <- initial_split(data, prop = 0.70, strata = "MOD_CLASS")

train_data <- training(initial_split)
test_data  <- testing(initial_split)

cat(sprintf("Training:   %d samples\n", nrow(train_data)))
cat(sprintf("Testing:    %d samples\n\n", nrow(test_data)))

readr::write_csv(train_data, here("Data/Dataframes/TrainingPoints.csv"))
readr::write_csv(test_data, here("Data/Dataframes/TestPoints.csv"))

###################################################################################################

# === Step 3: Hyperparameter Tuning ===
cat("=== Step 3: Hyperparameter Tuning ===\n")

# Recipe with selected features (using step_rm instead of deprecated step_select)
#features_to_remove <- setdiff(all_features, best_features)

start_recipe <- recipe(as.formula(paste0(target_var, "~ .")), 
                       data = train_data) %>%
    update_role(huc, cluster, new_role = "ID") %>%
    step_dummy(all_nominal_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_zv(all_predictors())
start_prep <- prep(start_recipe)
start_juice <- juice(start_prep)

start_spec <- 
    rand_forest(
        trees = 10,
        mtry = tune(),
        min_n = tune()
        ) %>% 
    set_mode("classification") %>%
    set_engine("ranger")

start_workflow <- 
    workflow() %>% 
    add_recipe(start_recipe) %>%
    add_model(start_spec) # %>% 
    # add_formula(as.formula(paste0(target_var, "~ .")))

folds <- vfold_cv(train_data, v = 10)
folds

rf_grid <- grid_regular(
    mtry(range = c(10, 12)),
    min_n(range = c(2, 3)),
    levels = 1
)

rf_grid

regular_res <- tune_grid(
    start_workflow,
    resamples = folds,
    grid = rf_grid
)

regular_res

best_auc <- select_best(regular_res, metric = "roc_auc")

################################################################################################### 

final_rf <- finalize_model(
    start_spec,
    best_auc
)

final_rf %>%
    set_engine("ranger", importance = "permutation") %>%
    fit(as.formula(paste0(target_var, "~ .")),
        data = start_juice
    ) %>%
    vip(geom = "point")

final_wf <- workflow() %>%
    add_recipe(start_recipe) %>%
    add_model(final_rf)

final_res <- final_wf %>%
    last_fit(trees_split)

final_res %>%
    collect_metrics()

############ 
# Ended Here 
##########



################################################################################################### 
# === Step 5: Final Test Set Evaluation ===
cat("=== Step 5: Test Set Performance ===\n")

# Predict on test set
test_predictions <- predict(final_fit, test_data) %>%
    bind_cols(predict(final_fit, test_data, type = "prob")) %>%
    bind_cols(test_data %>% select(all_of(target_var)))

# Calculate metrics (handle both binary and multiclass)
test_accuracy <- accuracy(test_predictions, truth = !!sym(target_var), estimate = .pred_class)

# Use same probability columns as validation
test_roc_auc <- tryCatch({
    roc_auc(test_predictions, truth = !!sym(target_var), all_of(prob_cols))
}, error = function(e) {
    cat("Note: ROC AUC calculation failed -", conditionMessage(e), "\n")
    tibble(.metric = "roc_auc", .estimator = "multiclass", .estimate = NA_real_)
})

test_metrics <- bind_rows(test_accuracy, test_roc_auc)

cat("Test metrics:\n")
print(test_metrics)

test_acc_value <- test_accuracy$.estimate
cat(sprintf("\nFinal Test Accuracy: %.4f\n\n", test_acc_value))

# Confusion matrix
cat("Test Set Confusion Matrix:\n")
test_predictions %>%
    conf_mat(truth = !!sym(target_var), estimate = .pred_class) %>%
    print()

###################################################################################################
# === Save Model and Artifacts ===
cat("\n=== Saving Model ===\n")

output_dir <- args[3]
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save the fitted workflow (includes preprocessing + model)
model_path <- file.path(output_dir, paste0(target_var, "_rf_model.rds"))
saveRDS(final_fit, model_path)
cat(sprintf("Model saved: %s\n", model_path))

# Save selected features for reference
features_path <- file.path(output_dir, paste0(target_var, "_selected_features.rds"))
saveRDS(best_features, features_path)
cat(sprintf("Features saved: %s\n", features_path))

# Save variable importance scores
importance_path <- file.path(output_dir, paste0(target_var, "_variable_importance.rds"))
saveRDS(importance_scores, importance_path)
cat(sprintf("Importance scores saved: %s\n", importance_path))

# Also save as CSV for easy viewing
importance_csv_path <- file.path(output_dir, paste0(target_var, "_variable_importance.csv"))
write.csv(importance_scores, importance_csv_path, row.names = FALSE)
cat(sprintf("Importance scores (CSV): %s\n", importance_csv_path))

# Save best hyperparameters
params_path <- file.path(output_dir, paste0(target_var, "_best_params.rds"))
saveRDS(best_params, params_path)
cat(sprintf("Parameters saved: %s\n", params_path))

# Save metrics summary
metrics_summary <- list(
    validation = val_metrics,
    test = test_metrics,
    n_features_selected = length(best_features),
    n_features_original = n_features,
    best_params = best_params
)
metrics_path <- file.path(output_dir, paste0(target_var, "_metrics_summary.rds"))
saveRDS(metrics_summary, metrics_path)
cat(sprintf("Metrics summary saved: %s\n", metrics_path))

###################################################################################################
# === Cleanup ===
plan(sequential)  # Reset to sequential processing

cat("\n=== Pipeline Complete ===\n")
cat(sprintf("Features used: %d of %d original\n", length(best_features), n_features))
cat(sprintf("Final model: Random Forest with %d trees\n", best_params$trees))
cat(sprintf("Validation Accuracy: %.4f\n", val_acc_value))
cat(sprintf("Test Accuracy: %.4f\n", test_acc_value))

