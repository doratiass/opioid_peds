# packages --------------------------------------------------------------------
library(tidyverse)
library(fastshap)
library(shapviz)
library(tidymodels)
library(parallel)
library(doParallel)
library(probably)
library(scam)
tidymodels_prefer()

source(file.path("Rprojects","funcs.R"), encoding = "utf8")
load("models.RData")

pfun <- function(model, newdata, prob = prob_df) {
  prob_raw <- predict(model, newdata, type = "response")
  model_prob <- sapply(prob_raw, function(x) {
    as.numeric(prob[which.min(abs(prob[["model_prob"]] - x)), "cal_prob", drop = T])
  })
  return(model_prob)
}

set.seed(4563)
cat("\f")

# XGB_1 ------------------------------------------------------------------------
## Calibration -----------------------------------------------------------------
scam_cal <- scam(outcome ~ s(.pred_Misuser,bs = "mpi", m = 2),
                 data = xgb_train_fit_1 %>% 
                   collect_predictions() %>%
                   mutate(outcome = as.numeric(outcome == "Misuser")),
                 Family = binomial)
df <- tibble(pred_Misuser = seq(0,1,length.out = 100000))
prob  <- predict.scam(scam_cal, df, type = "response")

prob_df <- df %>%
  rename(model_prob = .pred.Misuser) %>%
  mutate(cal_prob = ifelse(model_prob > 0.4,
                           as.numeric(exp(20.340*model_prob-6.313)/(1+exp(20.340*model_prob-6.313))), 
                           prob)) %>%
  tibble()

## SHAP values -----------------------------------------------------------------
xgb_prep_1 <- xgb_rec_1 %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

xgb_shap_1 <- bake(xgb_prep_1,
                   has_role("predictor"),
                   new_data = df_train_1,
                   composition = "matrix")

xgb_bg_1 <- bake(xgb_prep_1,
                 has_role("predictor"),
                 new_data = df_train_1[sample(1:nrow(df_train_1), 5000),],
                 composition = "matrix")

cl <- makepsockcluster(64)
registerDoParallel(cl)

shap_1 <- Fastshap::explain(extract_fit_engine(final_xgb_fit_1),
                            x = shap_bg_1,
                            pred_wrapper = pfun,
                            newdata = xgb_shap_1,
                            nsim = 100,
                            adjust = TRUE,
                            shap_only = FALSE,
                            parallel = TRUE,
                            .export = c("prob_df"))
stopcluster (cl)

shap_imp_1 <- sv_importance(shap_1, kind = "both", show_numbers = TRUE, max_display = 36) +
  theme_classic()

shap_imp_1

# XGB_2 ------------------------------------------------------------------------
## Calibration -----------------------------------------------------------------
scam_cal <- scam(outcome ~ s(.pred_Misuser,bs = "mpi", m = 2),
                 data = xgb_train_fit_2 %>% 
                   collect_predictions() %>%
                   mutate(outcome = as.numeric(outcome == "Misuser")),
                 Family = binomial)
df <- tibble(pred_Misuser = seq(0,1,length.out = 100000))
prob  <- predict.scam(scam_cal, df, type = "response")

prob_df <- df %>%
  rename(model_prob = .pred.Misuser) %>%
  mutate(cal_prob = ifelse(model_prob > 0.4,
                           as.numeric(exp(20.340*model_prob-6.313)/(1+exp(20.340*model_prob-6.313))), 
                           prob)) %>%
  tibble()

## SHAP values -----------------------------------------------------------------
xgb_prep_2 <- xgb_rec_2 %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

xgb_shap_2 <- bake(xgb_prep_2,
                   has_role("predictor"),
                   new_data = df_train_2,
                   composition = "matrix")

xgb_bg_2 <- bake(xgb_prep_2,
                 has_role("predictor"),
                 new_data = df_train_2[sample(1:nrow(df_train_2), 5000),],
                 composition = "matrix")

cl <- makepsockcluster(64)
registerDoParallel(cl)

shap_2 <- Fastshap::explain(extract_fit_engine(final_xgb_fit_2),
                            x = shap_bg_2,
                            pred_wrapper = pfun,
                            newdata = xgb_shap_2,
                            nsim = 100,
                            adjust = TRUE,
                            shap_only = FALSE,
                            parallel = TRUE,
                            .export = c("prob_df"))
stopcluster (cl)

shap_imp_2 <- sv_importance(shap_2, kind = "both", show_numbers = TRUE, max_display = 36) +
  theme_classic()

shap_imp_2

# XGB_3 ------------------------------------------------------------------------
## Calibration -----------------------------------------------------------------
scam_cal <- scam(outcome ~ s(.pred_Misuser,bs = "mpi", m = 2),
                 data = xgb_train_fit_3 %>% 
                   collect_predictions() %>%
                   mutate(outcome = as.numeric(outcome == "Misuser")),
                 Family = binomial)
df <- tibble(pred_Misuser = seq(0,1,length.out = 100000))
prob  <- predict.scam(scam_cal, df, type = "response")

prob_df <- df %>%
  rename(model_prob = .pred.Misuser) %>%
  mutate(cal_prob = ifelse(model_prob > 0.4,
                           as.numeric(exp(20.340*model_prob-6.313)/(1+exp(20.340*model_prob-6.313))), 
                           prob)) %>%
  tibble()

## SHAP values -----------------------------------------------------------------
xgb_prep_3 <- xgb_rec_3 %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

xgb_shap_3 <- bake(xgb_prep_3,
                   has_role("predictor"),
                   new_data = df_train_3,
                   composition = "matrix")

xgb_bg_3 <- bake(xgb_prep_3,
                 has_role("predictor"),
                 new_data = df_train_3[sample(1:nrow(df_train_3), 5000),],
                 composition = "matrix")

cl <- makepsockcluster(64)
registerDoParallel(cl)

shap_3 <- Fastshap::explain(extract_fit_engine(final_xgb_fit_3),
                            x = shap_bg_3,
                            pred_wrapper = pfun,
                            newdata = xgb_shap_3,
                            nsim = 100,
                            adjust = TRUE,
                            shap_only = FALSE,
                            parallel = TRUE,
                            .export = c("prob_df"))
stopcluster (cl)

shap_imp_3 <- sv_importance(shap_3, kind = "both", show_numbers = TRUE, max_display = 36) +
  theme_classic()

shap_imp_3
