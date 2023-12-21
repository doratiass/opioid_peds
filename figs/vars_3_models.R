library(tidyverse)
library(ggbump)
library(rcartocolor)
#https://medialab.github.io/iwanthue/

model_3_shap <- read_csv("model_3_shap.csv", show_col_types = FALSE) %>%
  # pivot_wider(
  #   id_cols = "var",
  #   names_from = "year",
  #   names_glue = "{year}",
  #   values_from = "rank"
  # ) %>%
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

