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

tidymodels_prefer()
set.seed(11)

# Enable parallel processing with future (SLURM-aware)
# Detect cores: SLURM env var > detectCores
n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = parallel::detectCores() - 1))
n_cores <- max(1, n_cores)

# Set future plan - use multicore on Linux (fork-based, more efficient)
# Falls back to multisession on Windows/RStudio
if (.Platform$OS.type == "unix" && !identical(Sys.getenv("RSTUDIO"), "1")) {
    plan(multicore, workers = n_cores)
} else {
    plan(multisession, workers = n_cores)
}

# Register future backend for foreach (used by tune_grid)
registerDoFuture()

# Set future options
options(future.globals.maxSize = 10 * 1024^3)  # 10GB limit for exported objects
options(future.rng.onMisuse = "ignore")       # Suppress RNG warnings

cat(sprintf("SLURM Job ID: %s\n", Sys.getenv("SLURM_JOB_ID", "N/A (local)")))
cat(sprintf("Using %d workers with plan: %s\n\n", n_cores, class(plan())[1]))

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
    dplyr::select(-huc, -cluster, -geom) |> 
    drop_na()

target_var <- args[2]
if(target_var == "MOD_CLASS"){
    data <- data |> select(-COARSE_CLASS)
} else if(target_var == "COARSE_CLASS") {
    data <- data |> select(-MOD_CLASS)
}
cat("Target variable:", target_var, "\n")
cat("Dataset dimensions:", nrow(data), "x", ncol(data), "\n")
cat("Features:", paste(names(data), collapse = ", "), "\n\n")

# Check number of classes for later metric handling
n_classes <- length(levels(data[[target_var]]))
cat(sprintf("Number of classes: %d\n\n", n_classes))
###################################################################################################

# === Step 1: Split Data (60% train, 20% validation, 20% test) ===
cat("=== Step 1: Data Splitting ===\n")

initial_split <- initial_validation_split(data, prop = c(0.6, 0.2), strata = all_of(target_var))

train_data <- training(initial_split)
val_data   <- validation(initial_split)
test_data  <- testing(initial_split)

cat(sprintf("Training:   %d samples\n", nrow(train_data)))
cat(sprintf("Validation: %d samples\n", nrow(val_data)))
cat(sprintf("Testing:    %d samples\n\n", nrow(test_data)))

# Create validation set for tuning
val_set <- validation_set(initial_split)

readr::write_csv(train_data, here("Data/Dataframes/TrainingPoints.csv"))
readr::write_csv(val_data, here("Data/Dataframes/ValidationPoints.csv"))
readr::write_csv(test_data, here("Data/Dataframes/TestPoints.csv"))

###################################################################################################
# === Step 2: Recursive Feature Elimination (RFE) ===
cat("=== Step 2: Feature Selection (RFE) ===\n")

rfe_recipe <- recipe(as.formula(paste(target_var, "~ .")), data = train_data) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_zv(all_predictors())

# Initial model for variable importance
rfe_model <- rand_forest(trees = 500) %>%
    set_engine("ranger", importance = "permutation") %>%
    set_mode("classification")

rfe_workflow <- workflow() %>%
    add_recipe(rfe_recipe) %>%
    add_model(rfe_model)

rfe_fit <- fit(rfe_workflow, data = train_data)

# Get feature importance and select top features
importance_scores <- rfe_fit %>%
    extract_fit_parsnip() %>%
    vip::vi()

cat("Feature importance:\n")
print(importance_scores)

# RFE: Iteratively remove lowest importance features
all_features <- importance_scores$Variable
n_features <- length(all_features)
min_features <- max(2, floor(n_features / 3))

best_accuracy <- 0
best_features <- all_features

for (k in seq(n_features, min_features, by = -1)) {
    current_features <- importance_scores$Variable[1:k]
    features_to_remove <- setdiff(all_features, current_features)
    
    temp_recipe <- recipe(as.formula(paste(target_var, "~ .")), data = train_data) %>%
        step_rm(all_of(features_to_remove)) %>%
        step_normalize(all_numeric_predictors())
    
    temp_wf <- workflow() %>%
        add_recipe(temp_recipe) %>%
        add_model(rfe_model)
    
    temp_fit <- fit(temp_wf, data = train_data)
    temp_pred <- predict(temp_fit, val_data) %>%
        bind_cols(val_data %>% select(all_of(target_var)))
    
    temp_acc <- accuracy(temp_pred, truth = !!sym(target_var), estimate = .pred_class)$.estimate
    
    if (temp_acc >= best_accuracy) {
        best_accuracy <- temp_acc
        best_features <- current_features
    }
}

cat(sprintf("\nSelected %d features: %s\n", length(best_features), paste(best_features, collapse = ", ")))
cat(sprintf("RFE validation accuracy: %.4f\n\n", best_accuracy))

###################################################################################################
# === Step 3: Hyperparameter Tuning ===
cat("=== Step 3: Hyperparameter Tuning ===\n")

# Recipe with selected features (using step_rm instead of deprecated step_select)
features_to_remove <- setdiff(all_features, best_features)

final_recipe <- recipe(as.formula(paste(target_var, "~ .")), data = train_data) %>%
    step_rm(all_of(features_to_remove)) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_zv(all_predictors())

# Model specification with tunable parameters
rf_spec <- rand_forest(
    trees     = tune(),
    mtry      = tune(),
    min_n     = tune()
) %>%
    set_engine("ranger", max.depth = tune("max_depth")) %>%
    set_mode("classification")

# Tuning workflow
tune_workflow <- workflow() %>%
    add_recipe(final_recipe) %>%
    add_model(rf_spec)

# Define parameter grid
# Create custom max_depth parameter (ranger-specific)
max_depth_param <- new_quant_param(
    type = "integer",
    range = c(5L, 30L),
    inclusive = c(TRUE, TRUE),
    label = c(max_depth = "Max Tree Depth")
)

rf_params <- tune_workflow %>%
    extract_parameter_set_dials() %>%
    recipes::update(
        trees = trees(range = c(100, 1000)),
        mtry = mtry(range = c(1, min(length(best_features), 5))),
        min_n = min_n(range = c(2, 20)),
        max_depth = max_depth_param
    )

# Create grid with specific tree values (100, 500, 1000) combined with Latin hypercube for others
base_grid <- grid_space_filling(
    rf_params %>% recipes::update(trees = trees(range = c(100, 100))),  # placeholder
    size = 20
)

# Replace trees with specific values
tree_values <- c(100L, 500L, 1000L)
tune_grid <- base_grid %>%
    select(-trees) %>%
    crossing(trees = tree_values) %>%
    select(trees, everything())

cat("Tuning grid size:", nrow(tune_grid), "\n")
cat("Parameters being tuned: trees, mtry, min_n, max_depth\n")
cat("Tree values:", paste(tree_values, collapse = ", "), "\n\n")

# Perform tuning
tune_results <- tune_grid(
    tune_workflow,
    resamples = val_set,
    grid = tune_grid,
    metrics = metric_set(accuracy, roc_auc),
    control = control_grid(verbose = FALSE, parallel_over = "everything")
)

# Best hyperparameters
best_params <- select_best(tune_results, metric = "accuracy")
cat("Best hyperparameters:\n")
print(best_params %>% select(-`.config`))

###################################################################################################
# === Step 4: Validation Accuracy ===
cat("\n=== Step 4: Validation Set Performance ===\n")

# Finalize workflow with best parameters
final_workflow <- tune_workflow %>%
    finalize_workflow(best_params)

# Fit on training data
final_fit <- fit(final_workflow, data = train_data)

# Predict on validation set
val_predictions <- predict(final_fit, val_data) %>%
    bind_cols(predict(final_fit, val_data, type = "prob")) %>%
    bind_cols(val_data %>% select(all_of(target_var)))

# Calculate metrics (handle both binary and multiclass)
val_accuracy <- accuracy(val_predictions, truth = !!sym(target_var), estimate = .pred_class)

# Get probability columns that match actual class levels
class_levels <- levels(val_predictions[[target_var]])
prob_cols <- paste0(".pred_", class_levels)
# Keep only columns that exist in predictions
prob_cols <- prob_cols[prob_cols %in% names(val_predictions)]

val_roc_auc <- tryCatch({
    roc_auc(val_predictions, truth = !!sym(target_var), all_of(prob_cols))
}, error = function(e) {
    cat("Note: ROC AUC calculation failed -", conditionMessage(e), "\n")
    tibble(.metric = "roc_auc", .estimator = "multiclass", .estimate = NA_real_)
})

val_metrics <- bind_rows(val_accuracy, val_roc_auc)

cat("Validation metrics:\n")
print(val_metrics)

val_acc_value <- val_accuracy$.estimate
cat(sprintf("\nValidation Accuracy: %.4f\n\n", val_acc_value))

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

