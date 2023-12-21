# packages ####
library(tidyverse)
library(gtsummary)
library(tidymodels)
library(parallel)
library(doParallel)
library(probably)
library(ggpubr)
library(dcurves)
tidymodels_prefer()

source("funcs.R", encoding = "utf8")
load("op_models_data.RData")
set.seed(45)
cat("\f")

# funcs ####
bootstrap_prev <- function(splits) {
  analysis(splits) %>%
    group_by(outcome) %>%
    summarise(n = n()) %>%
    mutate(p = round(n/sum(n),4)) %>%
    filter(outcome == "Misuser") %>%
    pull(p)
}

bootstrap_pr <- function(splits) {
  x <- analysis(splits)
  pr_auc(x, outcome, .pred_Misuser)$.estimate
}

bootstrap_roc <- function(splits) {
  x <- analysis(splits)
  roc_auc(x, outcome, .pred_Misuser)$.estimate
}

bootstrap_sens <- function(splits) {
  x <- analysis(splits)
  sens(x, truth = outcome, estimate = perdiction)$.estimate
}

bootstrap_spec <- function(splits) {
  x <- analysis(splits)
  spec(x, truth = outcome, estimate = perdiction)$.estimate
}

bootstrap_ppv <- function(splits) {
  x <- analysis(splits)
  ppv(x, truth = outcome, estimate = perdiction)$.estimate
}

bootstrap_npv <- function(splits) {
  x <- analysis(splits)
  npv(x, truth = outcome, estimate = perdiction)$.estimate
}

bootstrap_accu <- function(splits) {
  x <- analysis(splits)
  accuracy(x, truth = outcome, estimate = perdiction)$.estimate
}

cal_plot <- function(final_fit, train_fit, split = c("train", "test") ,plat = FALSE) {
  if (split == "train") {
    if (plat) {
      df <- train_fit %>%
        collect_predictions() %>%
        cal_apply(cal_estimate_logistic(train_fit))
    } else {
      df <- train_fit %>%
        collect_predictions()
    }
  } else if (split == "test") {
    if (plat) {
      df <- final_fit %>%
        collect_predictions() %>%
        cal_apply(cal_estimate_logistic(train_fit))
    } else {
      df <- final_fit %>%
        collect_predictions()
    }
  }
  
  lm_cal <- lm(outcome ~ .pred_Misuser, data = df %>%
                 mutate(outcome = as.numeric(outcome == "Misuser")))
  summary(lm_cal)
  
  plot_txt <- tibble(
    txt = paste0("Intercept ", round(lm_cal$coefficients[1],2), 
                 "\nSlope ", round(lm_cal$coefficients[2],2)))
  
  title <- ifelse(split == "train",
                  "Training set",
                  "Testing set")
  
  df %>%
    mutate(outcome = as.numeric(outcome == "Misuser")) %>%
    ggplot(aes(.pred_Misuser, outcome)) +
    geom_rug(sides = "tb", color = "grey") +
    geom_smooth(method = "loess", color = "black", se = TRUE, fullrange = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "longdash") +
    geom_text(data = plot_txt, aes(x = 0.1, y = 0.95, label = txt)) +
    labs(x = "Predicted risk",
         y = "Observed proportion") +
    theme_bw() +
    coord_equal(xlim = c(0,1), ylim = c(0,1)) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    ) + ggtitle(title) -> plot 
  
  return(plot)
}

# summary preformance ####

## find best j ####
n_j <- 1000
xgb_j_1 <- 0
xgb_v_1 <- 0

for (i in 1:n_j) {
  value <- i/n_j_p
  xgb_train_fit_1 %>%
    collect_predictions() %>%
    mutate(perdiction = factor(ifelse(.pred_Misuser > value, 
                                      "Misuser", "Proper"), 
                               levels = c("Misuser", "Proper"))) %>%
    j_index(outcome, perdiction) %>%
    pull(.estimate) -> new_j
  if (new_j > xgb_j_1) {
    xgb_j_1 <- new_j
    xgb_v_1 <- value
  }
}

xgb_j_2 <- 0
xgb_v_2 <- 0

for (i in 1:n_j) {
  value <- i/n_j_p
  xgb_train_fit_2 %>%
    collect_predictions() %>%
    mutate(perdiction = factor(ifelse(.pred_Misuser > value, 
                                      "Misuser", "Proper"), 
                               levels = c("Misuser", "Proper"))) %>%
    j_index(outcome, perdiction) %>%
    pull(.estimate) -> new_j
  if (new_j > xgb_j_2) {
    xgb_j_2 <- new_j
    xgb_v_2 <- value
  }
}

xgb_j_3 <- 0
xgb_v_3 <- 0

for (i in 1:n_j) {
  value <- i/n_j_p
  xgb_train_fit_3 %>%
    collect_predictions() %>%
    mutate(perdiction = factor(ifelse(.pred_Misuser > value, 
                                      "Misuser", "Proper"), 
                               levels = c("Misuser", "Proper"))) %>%
    j_index(outcome, perdiction) %>%
    pull(.estimate) -> new_j
  if (new_j > xgb_j_3) {
    xgb_j_3 <- new_j
    xgb_v_3 <- value
  }
}

### sum - j ####
model_list <- list(final_xgb_fit_1,final_xgb_fit_2,final_xgb_fit_3)
train_model_list <- list(xgb_train_fit_1,xgb_train_fit_2,xgb_train_fit_3)

model_names <- c("Model 1","Model 2","Model 3")
v_list <- c(xgb_v_1,xgb_v_2,xgb_v_3)

sum_models_list <- list()

for (i in 1:length(model_list)) {
  bs_df <- bootstraps(model_list[[i]] %>%
                        collect_predictions() %>%
                        mutate(perdiction = factor(ifelse(.pred_Misuser > v_list[i], 
                                                          "Misuser", "Proper"), 
                                                   levels = c("Misuser", "Proper"))), 
                      times = 1000)
  
  
  par_sum <- bs_df %>% 
    mutate(
      n = nrow(model_list[[i]] %>% collect_predictions()),
      prev = map_dbl(splits, bootstrap_prev), 
      accu = map_dbl(splits, bootstrap_accu),
      pr = map_dbl(splits, bootstrap_pr), 
      roc = map_dbl(splits, bootstrap_roc), 
      sens = map_dbl(splits, bootstrap_sens), 
      spec = map_dbl(splits, bootstrap_spec), 
      ppv = map_dbl(splits, bootstrap_ppv), 
      npv = map_dbl(splits, bootstrap_npv)
    ) %>%
    pivot_longer(cols = !c(splits, id),
                 values_to = "val",
                 names_to = "par") %>%
    group_by(par) %>%
    mutate(model = model_names[i])
  
  sum_models_list <- append(sum_models_list, list(par_sum))
}

sum_models <- bind_rows(sum_models_list)

### table ####
style_number_digits <- purrr::partial(gtsummary::style_number, digits = 3)
ll <- function(x) {quantile(x, 0.025)}#{mean(x)-1.96*sd(x)}
ul <- function(x) {quantile(x, 0.975)}#{mean(x)+1.96*sd(x)}

sum_models %>%
  mutate(name = paste(model, id, sep = "_@_")) %>%
  pivot_wider(id_cols = "name",
              names_from = "par",
              values_from = "val") %>%
  mutate(model = str_split_i(name, "_@_", 1)) %>%
  tbl_summary(by = "model",
              statistic = list(all_continuous() ~ "{mean} ({ll}, {ul})"), # ({sd})
              include = -c(name,n,prev, pr, ppv,npv),
              label = list(
                accu ~ "Accuracy",
                roc ~ "AUC-ROC",
                sens ~ "Sensitivity",
                spec ~ "Specificity"
              ),
              digits = list(c(all_continuous()) ~ c(3, 3))) %>%
  modify_header(label ~ "**Parameter**",
                all_stat_cols() ~ "**{level}**") %>%
  modify_footnote(all_stat_cols() ~ "mean (95% CI)")

# ROC & PR ####
sum_models %>%
  filter(par == "roc") %>%
  group_by(model) %>%
  summarise(mean = mean(val),
            se = sd(val),
            ul = mean+1.96*se,
            ll = mean-1.96*se) -> auc_bootstrap


paste0(auc_bootstrap[1,1]," - ", round(auc_bootstrap[1,2],3), " (", 
       round(auc_bootstrap[2,5],3),"-",
       round(auc_bootstrap[2,4],3),")")

xgb_roc_1 <- final_xgb_fit_1 %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_Misuser) %>%
  mutate(model = paste0(auc_bootstrap[1,1]," - ", round(auc_bootstrap[1,2],3), " (", 
                        round(auc_bootstrap[1,5],3),"-",
                        round(auc_bootstrap[1,4],3),")"),
         inx = 1)

xgb_pr_1 <- final_xgb_fit_1 %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_Misuser) %>%
  mutate(model = paste("Model 1 -", round(pr_auc(data = final_xgb_fit_1 %>%
                                                   collect_predictions(),
                                                 truth = outcome,
                                                 .pred_Misuser)[1,3],3)),
         inx = 1)

xgb_roc_2 <- final_xgb_fit_2 %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_Misuser) %>%
  mutate(model = paste0(auc_bootstrap[2,1]," - ", round(auc_bootstrap[2,2],3), " (", 
                        round(auc_bootstrap[2,5],3),"-",
                        round(auc_bootstrap[2,4],3),")"),
         inx = 2)

xgb_pr_2 <- final_xgb_fit_2 %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_Misuser) %>%
  mutate(model = paste("Model 2 -", round(pr_auc(data = final_xgb_fit_2 %>%
                                                   collect_predictions(),
                                                 truth = outcome,
                                                 .pred_Misuser)[1,3],3)),
         inx = 2)

xgb_roc_3 <- final_xgb_fit_3 %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_Misuser) %>%
  mutate(model = paste0(auc_bootstrap[3,1]," - ", round(auc_bootstrap[3,2],3), " (", 
                        round(auc_bootstrap[3,5],3),"-",
                        round(auc_bootstrap[3,4],3),")"),
         inx = 3)

xgb_pr_3 <- final_xgb_fit_3 %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_Misuser) %>%
  mutate(model = paste("Model 3 -", round(pr_auc(data = final_xgb_fit_3 %>%
                                                   collect_predictions(),
                                                 truth = outcome,
                                                 .pred_Misuser)[1,3],3)),
         inx = 3)


# Calibration ####
## XGBoost 1 ####
xgb_tr_1_p <- cal_plot(final_xgb_fit_1, xgb_train_fit_1, split = "train", plat = TRUE)
xgb_ts_1_p <- cal_plot(final_xgb_fit_1, xgb_train_fit_1, split = "test", plat = TRUE)

cal_fig_xgb_1 <- ggarrange(xgb_tr_1_p + rremove("xylab"),
                           xgb_ts_1_p + rremove("xylab"), 
                           labels = "AUTO", label.y = 0.96,
                           ncol = 2)

annotate_figure(cal_fig_xgb_1,
                bottom = text_grob("Predicted risk", color = "black", vjust = -1),
                left = text_grob("Observed proportion", color = "black", rot = 90),
                fig.lab = "Figure 2 - Calibration Plot", 
                fig.lab.face = "bold", fig.lab.size = 14)

## XGBoost 2 ####
xgb_tr_2_p <- cal_plot(final_xgb_fit_2, xgb_train_fit_2, split = "train", plat = TRUE)
xgb_ts_2_p <- cal_plot(final_xgb_fit_2, xgb_train_fit_2, split = "test", plat = TRUE)

cal_fig_xgb_2 <- ggarrange(xgb_tr_2_p + rremove("xylab"),
                           xgb_ts_2_p + rremove("xylab"), 
                           labels = "AUTO", label.y = 0.96,
                           ncol = 2)

annotate_figure(cal_fig_xgb_2,
                bottom = text_grob("Predicted risk", color = "black", vjust = -1),
                left = text_grob("Observed proportion", color = "black", rot = 90),
                fig.lab = "Figure 2 - Calibration Plot", 
                fig.lab.face = "bold", fig.lab.size = 14)

## XGBoost 3 ####
xgb_tr_3_p <- cal_plot(final_xgb_fit_3, xgb_train_fit_3, split = "train", plat = TRUE)
xgb_ts_3_p <- cal_plot(final_xgb_fit_3, xgb_train_fit_3, split = "test", plat = TRUE)

cal_fig_xgb_3 <- ggarrange(xgb_tr_3_p + rremove("xylab"),
                           xgb_ts_3_p + rremove("xylab"), 
                           labels = "AUTO", label.y = 0.96,
                           ncol = 2)

annotate_figure(cal_fig_xgb_3,
                bottom = text_grob("Predicted risk", color = "black", vjust = -1),
                left = text_grob("Observed proportion", color = "black", rot = 90),
                fig.lab = "Figure 2 - Calibration Plot", 
                fig.lab.face = "bold", fig.lab.size = 14)
