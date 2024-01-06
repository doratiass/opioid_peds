# packages --------------------------------------------------------------------
library(tidyverse)
library(gtsummary)
library(shapviz)
library(tidymodels)
library(pdp)
library(parallel)
library(doParallel)
library(probably)
library(gridExtra)
tidymodels_prefer()

source(file.path("Rprojects","funcs.R"), encoding = "utf8")
load("models.RData")
set.seed(45)
cat("\f")

# XGB_1 ------------------------------------------------------------------------
## SHAP values -----------------------------------------------------------------
xgb_prep_1 <- xgb_rec_1 %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

xgb_shap_data_1 <- bake(xgb_prep_1,
                        has_role("predictor"),
                        new_data = df_test_1,
                        composition = "matrix")

shap_1 <- shapviz(extract_fit_engine(final_xgb_fit_1), X_pred = xgb_shap_data_1)

shap_imp_1 <- sv_importance(shap_1, kind = "both", show_numbers = TRUE, max_display = 36) +
  theme_classic()

shap_imp_1

## PDP -------------------------------------------------------------------------
deps_1 <- shap_imp_1$data %>%
  mutate(feature = as.character(feature)) %>%
  distinct(feature) %>%
  pull(feature)

dep_plot_list_1 <- lapply(deps_1, pdp::partial, 
                 object = extract_fit_engine(final_xgb_fit_1), 
                 train = xgb_shap_data_1)

for (i in 1:length(dep_plot_list_1)) {
  dep_plot_list_1[[i]][,"feature"] <- colnames(dep_plot_list_1[[i]])[1]
  names(dep_plot_list_1[[i]])[1] <- "shap"
}

dep_plot_1 <- bind_rows(dep_plot_list_1)

dep_plot_1 %>%
  ggplot(aes(shap, yhat)) +
  geom_path() + 
  facet_wrap(~feature, scales = "free") +
  theme_classic()

# XGB_2 ------------------------------------------------------------------------
## SHAP values -----------------------------------------------------------------
xgb_prep_2 <- xgb_rec_2 %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

xgb_shap_data_2 <- bake(xgb_prep_2,
                        has_role("predictor"),
                        new_data = df_test_2,
                        composition = "matrix")

shap_2 <- shapviz(extract_fit_engine(final_xgb_fit_2), X_pred = xgb_shap_data_2)

shap_imp_2 <- sv_importance(shap_2, kind = "both", show_numbers = TRUE, max_display = 36) +
  theme_classic()

## PDP -------------------------------------------------------------------------
deps_2 <- shap_imp_2$data %>%
  mutate(feature = as.character(feature)) %>%
  distinct(feature) %>%
  pull(feature)

dep_plot_list_2 <- lapply(deps_2, pdp::partial, 
                          object = extract_fit_engine(final_xgb_fit_2), 
                          train = xgb_shap_data_2)

for (i in 1:length(dep_plot_list_2)) {
  dep_plot_list_2[[i]][,"feature"] <- colnames(dep_plot_list_2[[i]])[1]
  names(dep_plot_list_2[[i]])[1] <- "shap"
}

dep_plot_2 <- bind_rows(dep_plot_list_2)

dep_plot_2 %>%
  ggplot(aes(shap, yhat)) +
  geom_path() + 
  facet_wrap(~feature, scales = "free") +
  theme_classic()

# XGB_3 ------------------------------------------------------------------------
## SHAP values -----------------------------------------------------------------
xgb_prep_3 <- xgb_rec_3 %>%
  prep(strings_as_factors = FALSE,
       log_changes = TRUE,
       verbose = TRUE)

xgb_shap_data_3 <- bake(xgb_prep_3,
                        has_role("predictor"),
                        new_data = df_test_3,
                        composition = "matrix")

shap_3 <- shapviz(extract_fit_engine(final_xgb_fit_3), X_pred = xgb_shap_data_3)

shap_imp_3 <- sv_importance(shap_3, kind = "both", show_numbers = TRUE, max_display = 36) +
  theme_classic()

## PDP -------------------------------------------------------------------------
deps_3 <- shap_imp_3$data %>%
  mutate(feature = as.character(feature)) %>%
  distinct(feature) %>%
  pull(feature)

dep_plot_list_3 <- lapply(deps_3, pdp::partial, 
                          object = extract_fit_engine(final_xgb_fit_3), 
                          train = xgb_shap_data_3)

for (i in 1:length(dep_plot_list_3)) {
  dep_plot_list_3[[i]][,"feature"] <- colnames(dep_plot_list_3[[i]])[1]
  names(dep_plot_list_3[[i]])[1] <- "shap"
}

dep_plot_3 <- bind_rows(dep_plot_list_3)

dep_plot_3 %>%
  ggplot(aes(shap, yhat)) +
  geom_path() + 
  facet_wrap(~feature, scales = "free") +
  theme_classic()
