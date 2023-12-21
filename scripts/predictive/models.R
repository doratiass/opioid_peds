# packages ####
library(tidyverse)
library(tidymodels)
library(parallel)
library(doParallel)
library(probably)
tidymodels_prefer()

source("funcs.R", encoding = "utf8")
load("op_models_data.RData")
set.seed(45)
cat("\f")

cor_thres <- 0.9

# create ML data & general variables####
selected_predictors <- c("RE_age", "gender", "SES", "sector", "bmi","district",  
                         "generic", "outcome", "n_visits","diff_profession",
                         "proffesion_primary", "n_diagnosis",
                         "malignancy", "pain", "psychiatric_not_pci", "pci",
                         "sum_drug", "drug_nervous", "drug_muscle",
                         "drug_antineoplastic", "lab_n", "img_n")

df_split_1 <- initial_split(model_data_1 %>% 
                              select(all_of(selected_predictors)), 
                            strata = outcome)
df_train_1 <- training(df_split_1)
df_test_1 <- testing(df_split_1)
df_train_cv_1 <- vfold_cv(df_train_1, v = 10, strata = outcome)

df_split_2 <- initial_split(model_data_2 %>% 
                              select(all_of(selected_predictors)), 
                            strata = outcome)
df_train_2 <- training(df_split_2)
df_test_2 <- testing(df_split_2)
df_train_cv_2 <- vfold_cv(df_train_2, v = 10, strata = outcome)

df_split_3 <- initial_split(model_data_3 %>% 
                              select(all_of(selected_predictors)), 
                            strata = outcome)
df_train_3 <- training(df_split_3)
df_test_3 <- testing(df_split_3)
df_train_cv_3 <- vfold_cv(df_train_3, v = 10, strata = outcome)

# XGB 1 ####
## prepare the data ####
xgb_rec_1 <- recipe(outcome ~ ., data = df_train_1) %>%
  step_zv(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = cor_thres) %>%
  step_dummy(all_nominal_predictors())

## setup the model & tune hyperparameters####
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(), mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf_1 <- workflow() %>%
  add_recipe(xgb_rec_1) %>% 
  add_model(xgb_spec)

xgb_grid_1 <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train_1),
  learn_rate(),
  size = 500
)

set.seed(2020)

cl <- makePSOCKcluster(96)
registerDoParallel(cl)

xgb_res_1 <- tune_grid(
  xgb_wf_1,
  resamples = df_train_cv_1,
  grid = xgb_grid_1,
  control = control_grid(save_pred = TRUE, 
                         save_workflow = TRUE,
                         parallel_over = 'everything'),
  metrics = metric_set(roc_auc)
)

stopCluster(cl)

xgb_res_1 %>% 
  autoplot()

## select best model ####
xgb_best_auc_1 <- xgb_res_1 %>%
  select_best("roc_auc")

final_xgb_1 <- finalize_workflow(
  xgb_wf_1,
  xgb_best_auc_1
)

## fit model ####
set.seed(2020)

cl <- makePSOCKcluster(96)
registerDoParallel(cl)
xgb_train_fit_1 <- fit_resamples(final_xgb_1,
                               resamples = df_train_cv_1,
                               metrics = metric_set(roc_auc),
                               control = control_resamples(save_pred = TRUE, 
                                                           save_workflow = TRUE,
                                                           parallel_over = 'everything'))

stopCluster(cl)

xgb_train_fit_1 %>%
  collect_predictions() %>%
  mutate(outcome = factor(outcome, levels = c("Proper","Misuser"))) %>%
  glm(outcome ~ .pred_Misuser, data = ., family = "binomial")

final_xgb_fit_1 <- final_xgb_1 %>%
  last_fit(df_split_1)

collect_metrics(final_xgb_fit_1)

gc()

# XGB 2 ####
## prepare the data ####
xgb_rec_2 <- recipe(outcome ~ ., data = df_train_2) %>%
  step_zv(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = cor_thres) %>%
  step_dummy(all_nominal_predictors())

## setup the model & tune hyperparameters####
xgb_wf_2 <- workflow() %>%
  add_recipe(xgb_rec_2) %>% 
  add_model(xgb_spec)

xgb_grid_2 <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train_2),
  learn_rate(),
  size = 500
)

set.seed(2020)

cl <- makePSOCKcluster(96)
registerDoParallel(cl)

xgb_res_2 <- tune_grid(
  xgb_wf_2,
  resamples = df_train_cv_2,
  grid = xgb_grid_2,
  control = control_grid(save_pred = TRUE, 
                         save_workflow = TRUE,
                         parallel_over = 'everything'),
  metrics = metric_set(roc_auc)
)

stopCluster(cl)

xgb_res_2 %>% 
  autoplot()

## select best model ####
xgb_best_auc_2 <- xgb_res_2 %>%
  select_best("roc_auc")

final_xgb_2 <- finalize_workflow(
  xgb_wf_2,
  xgb_best_auc_2
)

## fit model ####
set.seed(2020)

cl <- makePSOCKcluster(96)
registerDoParallel(cl)
xgb_train_fit_2 <- fit_resamples(final_xgb_2,
                                 resamples = df_train_cv_2,
                                 metrics = metric_set(roc_auc),
                                 control = control_resamples(save_pred = TRUE, 
                                                             save_workflow = TRUE,
                                                             parallel_over = 'everything'))

stopCluster(cl)

xgb_train_fit_2 %>%
  collect_predictions() %>%
  mutate(outcome = factor(outcome, levels = c("Proper","Misuser"))) %>%
  glm(outcome ~ .pred_Misuser, data = ., family = "binomial")

final_xgb_fit_2 <- final_xgb_2 %>%
  last_fit(df_split_2)

collect_metrics(final_xgb_fit_2)

gc()

# XGB 3 ####
## prepare the data ####
xgb_rec_3 <- recipe(outcome ~ ., data = df_train_3) %>%
  step_zv(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = cor_thres) %>%
  step_dummy(all_nominal_predictors())

## setup the model & tune hyperparameters####
xgb_wf_3 <- workflow() %>%
  add_recipe(xgb_rec_3) %>% 
  add_model(xgb_spec)

xgb_grid_3 <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train_3),
  learn_rate(),
  size = 500
)

set.seed(2020)

cl <- makePSOCKcluster(96)
registerDoParallel(cl)

xgb_res_3 <- tune_grid(
  xgb_wf_3,
  resamples = df_train_cv_3,
  grid = xgb_grid_3,
  control = control_grid(save_pred = TRUE, 
                         save_workflow = TRUE,
                         parallel_over = 'everything'),
  metrics = metric_set(roc_auc)
)

stopCluster(cl)

xgb_res_3 %>% 
  autoplot()

## select best model ####
xgb_best_auc_3 <- xgb_res_3 %>%
  select_best("roc_auc")

final_xgb_3 <- finalize_workflow(
  xgb_wf_3,
  xgb_best_auc_3
)

## fit model ####
set.seed(2020)

cl <- makePSOCKcluster(96)
registerDoParallel(cl)
xgb_train_fit_3 <- fit_resamples(final_xgb_3,
                                 resamples = df_train_cv_3,
                                 metrics = metric_set(roc_auc),
                                 control = control_resamples(save_pred = TRUE, 
                                                             save_workflow = TRUE,
                                                             parallel_over = 'everything'))

stopCluster(cl)

xgb_train_fit_3 %>%
  collect_predictions() %>%
  mutate(outcome = factor(outcome, levels = c("Proper","Misuser"))) %>%
  glm(outcome ~ .pred_Misuser, data = ., family = "binomial")

final_xgb_fit_3 <- final_xgb_3 %>%
  last_fit(df_split_3)

collect_metrics(final_xgb_fit_3)

gc()

save(xgb_rec_1, xgb_spec, xgb_wf_1, xgb_grid_1, xgb_res_1, xgb_best_auc_1,
     final_xgb_1, xgb_train_fit_1, final_xgb_fit_1,
     xgb_rec_2, xgb_wf_2, xgb_grid_2, xgb_res_2, xgb_best_auc_2,
     final_xgb_2, xgb_train_fit_2, final_xgb_fit_2,
     xgb_rec_3, xgb_wf_3, xgb_grid_3, xgb_res_3, xgb_best_auc_3,
     final_xgb_3, xgb_train_fit_3, final_xgb_fit_3,
     file = "models.RData")



