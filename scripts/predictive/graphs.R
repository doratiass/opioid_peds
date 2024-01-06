# packages, defs and funcs ----------------------------------------------------
library(tidyverse)
library(gtsummary)
library(tidymodels)
library(RColorBrewer)
library(probably)
library(ggpubr)
library(shapviz)
library(dcurves)
library(ggbump)
library(rcartocolor)
tidymodels_prefer()

source("funcs.R", encoding = "utf8")
load("Rprojects","op_models_data.RData")
set.seed(45)
cat("\f")

cal_plot_three <- function(final_fit, train_fit, split = c("train", "test") ,plat = FALSE) {
  if (split == "train") {
    df <- bind_rows(
      train_fit[[1]] %>%
        collect_predictions() %>%
        cal_apply(cal_estimate_logistic(train_fit[[1]])) %>% 
        mutate(model = "Model 1"),
      train_fit[[2]] %>%
        collect_predictions() %>%
        cal_apply(cal_estimate_logistic(train_fit[[2]])) %>% 
        mutate(model = "Model 2"),
      train_fit[[3]] %>%
        collect_predictions() %>%
        cal_apply(cal_estimate_logistic(train_fit[[3]])) %>% 
        mutate(model = "Model 3"))
  } else if (split == "test") {
    df <- bind_rows(
      final_fit[[1]] %>%
        collect_predictions() %>%
        cal_apply(cal_estimate_logistic(train_fit[[1]])) %>% 
        mutate(model = "Model 1"),
      final_fit[[2]] %>%
        collect_predictions() %>%
        cal_apply(cal_estimate_logistic(train_fit[[2]])) %>% 
        mutate(model = "Model 2"),
      final_fit[[3]] %>%
        collect_predictions() %>%
        cal_apply(cal_estimate_logistic(train_fit[[3]])) %>% 
        mutate(model = "Model 3"))
  }
  
  coef_int <- function(y,x) {
    z = tibble(x = x, y = y)
    lm_cal <- lm(y ~ x, data = z)
    
    return(round(lm_cal$coefficients[1],2))
  }
  
  coef_slope <- function(y,x) {
    z = tibble(x = x, y = y)
    lm_cal <- lm(y ~ x, data = z)
    
    return(round(lm_cal$coefficients[2],2))
  }
  
  plot_txt <- df %>%
    mutate(outcome = as.numeric(outcome == "Misuser")) %>%
    group_by(model) %>%
    summarise(int = coef_int(outcome,`.pred_Misuser`),
              slope = coef_slope(outcome,`.pred_Misuser`)) %>%
    mutate(txt = paste0(model,", int: ", int,", slope ", slope),
           inx = c(1,2,3))
  
  title <- ifelse(plat,
                  paste("Calibration plot -",split,"set - platt scaling"),
                  paste("Calibration plot -",split,"set"))
  
  df %>%
    mutate(outcome = as.numeric(outcome == "Misuser")) %>%
    ggplot(aes(.pred_Misuser, outcome, color = model)) +
    geom_rug(aes(group=model),sides = "tb",alpha = 0.2) + #, color = "grey"
    geom_smooth(linewidth = line_size, method = "loess", se = TRUE, fullrange = TRUE) +
    geom_abline(intercept = 0, slope = 1, linetype = "longdash") +
    geom_point(data = plot_txt, aes(x = 0.5, y = 0.2-0.05*inx), shape = 15, size = 3) +
    geom_text(data = plot_txt, aes(x = 0.55, y = 0.2-0.05*inx, label = txt, size = 15),
              color = "black", hjust = 0) +
    labs(x = "Predicted risk",
         y = "Observed proportion") +
    theme_bw() +
    coord_equal(xlim = c(0,1), ylim = c(0,1)) +
    theme(
      legend.position = "none",
      legend.title = element_blank()
    ) -> plot #+ ggtitle(title)
  
  return(plot)
}

# Theme -----------------------------------------------------------------------
plot_theme <- theme(
  plot.title = element_text(size = 25, hjust = 0.5),
  axis.title = element_text(size = 17),
  axis.text = element_text(size = 15),
  legend.text = element_text(size = 15),
  strip.text = element_text(size = 15))

color_pal <- 'Dark2'

line_size <- 1.2

# fig 1 - model assess --------------------------------------------------------

## ROC ------------------------------------------------------------------------
roc_plot <- rbind(xgb_roc_1,xgb_roc_2,xgb_roc_3) %>% #
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    linewidth = line_size
  ) +
  geom_path(linewidth = line_size) +
  geom_point(aes(x = 0.5, y = 0.2-0.05*inx), shape = 15, size = 3) +
  geom_text(aes(x = 0.55, y = 0.2-0.05*inx, label = model, size = 15),
            color = "black", hjust = 0, check_overlap = T) +
  coord_equal() +
  theme_bw() +
  plot_theme +
  scale_color_brewer(palette=color_pal) +
  theme(legend.position = "none",
        legend.title = element_blank())

roc_plot

## PR -------------------------------------------------------------------------
pr_plot <- rbind(xgb_pr_1,xgb_pr_2,xgb_pr_3) %>% #
  filter(!is.infinite(.threshold)) %>%
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_path() +
  coord_equal() +
  geom_point(aes(x = 0, y = 1-0.05*inx), shape = 15) +
  geom_text(aes(x = 0.05, y = 1-0.05*inx, label = model),
            color = "black", hjust = 0, check_overlap = T) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank())

## Calibration ----------------------------------------------------------------
cal_test <- cal_plot_three(list(final_xgb_fit_1, final_xgb_fit_2, final_xgb_fit_3),
                           list(xgb_train_fit_1,xgb_train_fit_2,xgb_train_fit_3), 
                           split = "test") +
  plot_theme +
  scale_color_brewer(palette=color_pal)

cal_test

## All together ---------------------------------------------------------------
sums_plot <- ggarrange(roc_plot,  cal_test, #pr_plot, cal_train,
                       labels = "AUTO",# label.y = 0.96,
                       ncol = 2) #, nrow = 2)

sums_plot

ggsave(filename = file.path("graphs","fig1.pdf"), plot = ggplot2::last_plot(), 
       width = 40, height = 20, dpi = 300, units = "cm", bg = "white")

# fig 2 - Decision curve analysis ---------------------------------------------
dc_df <- final_xgb_fit_1 %>% augment() %>%
  cal_apply(cal_estimate_logistic(xgb_train_fit_1)) %>%
  mutate(outcome = ifelse(outcome == "Misuser",1,0),
         oxy = ifelse(generic == "Oxycodone",1,0))

dcurves::dca(outcome ~ .pred_Misuser + oxy, data = dc_df, 
             as_probability = c("oxy")) %>%
  standardized_net_benefit() %>%
  as_tibble() %>%
  mutate(label = factor(case_when(
    label == ".pred_Misuser" ~ "Model",
    label == "oxy" ~ "Oxycodone users", 
    label == "Treat All" ~ "Assume everyone is a sustained user",
    label == "Treat None" ~ "Assume no one is a sustained user",
    TRUE ~ label
  ), levels = c("Model","Oxycodone users",
                "Assume no one is a sustained user",
                "Assume everyone is a sustained user")),
  inx = as.numeric(label)) %>%
  dplyr::filter(!is.na(standardized_net_benefit)) -> dc_plot_df

pal_cols <- brewer.pal(n = 4, name = color_pal)[c(1,2)]

dc_plot_df %>%
  ggplot(aes(x = threshold, y = standardized_net_benefit, color = label)) +
  geom_area(data = dc_plot_df %>% filter(label %in% c("Oxycodone users","Model")),
            aes(fill = label, group = label),
            alpha = 0.5, position = 'identity') +
  geom_line(aes(linetype = label),linewidth = line_size) +
  scale_linetype_manual(values=c("solid","solid","longdash","solid"))+
  coord_equal(ylim = c(-0.05, 1.02), xlim = c(0,0.3), ratio = 0.3) +
  geom_point(aes(x = 0.17, y = 1.05-0.05*inx), shape = 15, size = 3) +
  geom_text(aes(x = 0.18, y = 1.05-0.05*inx, label = label, size = 15),
            color = "black", hjust = 0, check_overlap = T) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     sec.axis=sec_axis(~., name = "Cost:Benefit Ratio",
                                       breaks=c(0.05,0.1,0.15,0.2, 0.25), 
                                       labels=c('1/19','1/9','3/17','1/4', '1/3'))) +
  labs(x = "Threshold Probability", y = "Standardized Net Benefit", 
       color = "") +
  theme_bw() +
  plot_theme +
  scale_color_manual(values = c("black","gray30", pal_cols)) +
  scale_fill_manual(values = pal_cols) +
  theme(legend.position = "none")

ggsave(filename = file.path("graphs","fig2.pdf"), plot = ggplot2::last_plot(), 
       width = 25, height = 25, dpi = 300, units = "cm", bg = "white")

# fig 3 - SHAP ----------------------------------------------------------------
shap_imp_bar_1 <- sv_importance(shap_1, kind = "bar", show_numbers = TRUE, max_display = 10) +
  theme_classic() +
  labs(x = "Mean absolute SHAP value") +
  plot_theme

shap_imp_bee_1 <- sv_importance(shap_1, kind = "beeswarm", show_numbers = FALSE, max_display = 10) +
  theme_classic() +
  plot_theme

ggarrange(shap_imp_bar_1, 
          shap_imp_bee_1 + 
            rremove("y.axis") + 
            rremove("y.text") + 
            rremove("y.ticks"),
          labels = "AUTO",
          ncol = 2, nrow = 1)

ggsave(filename = file.path("graphs","fig3.pdf"), plot = ggplot2::last_plot(), 
       width = 40, height = 20, dpi = 300, units = "cm", bg = "white")

# fig 4 - change in shap ------------------------------------------------------
shap_rank_1 <- shap_imp_1$data %>%
  group_by(feature) %>%
  summarise(
    shap = mean(abs(value),na.rm = TRUE),
    year = 1
  ) %>%
  arrange(desc(shap)) %>%
  mutate(rank_1 = row_number(shap))

shap_rank_2 <- shap_imp_2$data %>%
  group_by(feature) %>%
  summarise(
    shap = mean(abs(value),na.rm = TRUE),
    year = 2
  ) %>%
  arrange(desc(shap)) %>%
  mutate(rank_2 = row_number(shap))


shap_rank_3 <- shap_imp_3$data %>%
  group_by(feature) %>%
  summarise(
    shap = mean(abs(value),na.rm = TRUE),
    year = 3
  ) %>%
  arrange(desc(shap)) %>%
  mutate(rank_3 = row_number(shap))

model_3_shap <- left_join(shap_rank_1 %>% select(var = feature,rank_1),
                       shap_rank_2 %>% select(var = feature,rank_2), by = "feature") %>%
  left_join(shap_rank_3 %>% select(var = feature,rank_3), by = "feature") %>%
  pivot_longer(
    cols = !var,
    names_to = "year",
    values_to = "rank"
  ) %>%
  mutate(
    year = as.numeric(str_remove(year, "rank_")),
    rank = case_when(
      #  is.na(rank) ~ 4,
      rank <= 26 ~ NA,
      TRUE ~ rank)
  )

# [1] "#5F4690" "#1D6996" "#38A6A5" "#0F8554" "#73AF48" "#EDAD08" "#E17C05" "#CC503E" "#94346E" "#6F4070"
# [11] "#994E95" "#666666"

cols_14 <- palette(c(rcartocolor::carto_pal(name = "Prism"),"#b75278","#0F8554"))

model_3_shap %>%
  filter(var %in% c(intersect(model_3_shap[model_3_shap$year == 1 &
                                             !is.na(model_3_shap$rank),]$var,
                              model_3_shap[model_3_shap$year == 2 &
                                             !is.na(model_3_shap$rank),]$var),
                    intersect(model_3_shap[model_3_shap$year == 3 &
                                             !is.na(model_3_shap$rank),]$var,
                              model_3_shap[model_3_shap$year == 2 &
                                             !is.na(model_3_shap$rank),]$var),
                    intersect(intersect(model_3_shap[model_3_shap$year == 1 &
                                                       !is.na(model_3_shap$rank),]$var,
                                        model_3_shap[model_3_shap$year == 3 &
                                                       !is.na(model_3_shap$rank),]$var),
                              model_3_shap[model_3_shap$year == 2 &
                                             is.na(model_3_shap$rank),]$var))) -> shap_plot_cont
model_3_shap %>%
  mutate(cont = case_when(
    var %in% c(intersect(model_3_shap[model_3_shap$year == 1 &
                                        !is.na(model_3_shap$rank),]$var,
                         model_3_shap[model_3_shap$year == 2 &
                                        !is.na(model_3_shap$rank),]$var)) ~ "cont",
    var %in% c(intersect(model_3_shap[model_3_shap$year == 3 &
                                        !is.na(model_3_shap$rank),]$var,
                         model_3_shap[model_3_shap$year == 2 &
                                        !is.na(model_3_shap$rank),]$var)) ~ "cont",
    var %in% c(intersect(intersect(model_3_shap[model_3_shap$year == 1 &
                                                  !is.na(model_3_shap$rank),]$var,
                                   model_3_shap[model_3_shap$year == 3 &
                                                  !is.na(model_3_shap$rank),]$var),
                         model_3_shap[model_3_shap$year == 2 &
                                        is.na(model_3_shap$rank),]$var)) ~ "not")) %>%
  filter(!is.na(cont))-> shap_plot_cont_2

model_3_shap %>%
  filter(var %in% intersect(intersect(model_3_shap[model_3_shap$year == 1 &
                                                     !is.na(model_3_shap$rank),]$var,
                                      model_3_shap[model_3_shap$year == 3 &
                                                     !is.na(model_3_shap$rank),]$var),
                            model_3_shap[model_3_shap$year == 2 &
                                           is.na(model_3_shap$rank),]$var)) -> shap_plot_1_3

shap_plot_anti <- anti_join(model_3_shap, shap_plot_cont)

shap_plot_cont_2 %>%
  ggplot(aes(x = year, y = rank, colour=var)) +
  geom_bump(data = shap_plot_cont %>% filter(!(var %in% shap_plot_1_3$var)),linewidth = 1) +
  geom_point(aes(shape = cont),size = 3.5) + 
  scale_shape_manual(values=c(16, 18))+
  geom_text(data = filter(shap_plot_cont, year == 1, rank >= 6), 
            mapping = aes(x = 0.8, y = rank, label = str_wrap(var, 20)), 
            hjust = 1) +
  geom_text(data = filter(shap_plot_cont, year == 3, rank >= 6),
            mapping = aes(x = 3.2, y = rank, label = str_wrap(var, 20)),
            hjust = 0) +
  geom_point(data = shap_plot_anti, 
             aes(x = year, y = rank), color = "grey",
             size = 3.5) + 
  geom_text(data = filter(shap_plot_anti, year == 1), 
            mapping = aes(x = 0.8, y = rank, label = str_wrap(var, 20)), 
            hjust = 1, color = "grey") +
  geom_text(data = filter(shap_plot_anti, year == 3),
            mapping = aes(x = 3.2, y = rank, label = str_wrap(var, 20)),
            hjust = 0, color = "grey") +
  geom_text(data = filter(shap_plot_anti, year == 2),
            mapping = aes(x = 1.9, y = rank, label = str_wrap(var, 20)),
            hjust = 1, color = "grey") +
  labs(x="", 
       y="")+
  scale_x_continuous(breaks = c(1:3), limits = c(0.5, 3.5), labels = c("Model 1",
                                                                       "Model 2",
                                                                       "Model 3")) +
  scale_colour_carto_d(palette = "Prism") + #, direction = 1, na.value = c("#b75278","#0F8554")) +
  theme(plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour="white"),
        legend.position = "none", 
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        #   plot.margin = unit(c(0.5, 6, 0.5, 1), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text =  element_text(colour = "black", size=10, hjust = 0.5),
        plot.tag.position = c(1.05, 0.35),
        plot.tag = element_text(colour = "black", size=12, hjust = 0.5))


