library(tidyverse)
library(readxl)
library(gridExtra)
library(RColorBrewer)
library(sysfonts)
library(showtext)
library(patchwork)
library(ggtext)
library(glue)
library(charlatan)
library(magick)
library(ggimage)
library(ragg)

# def ####
bkg_col <- "white"
txt_col <- "black"
font_g <- "GillSans"
trait_size <- 12
title_size <- 18

non_sus_col <- "#1B9E77"
sus_col <- "#E6AB02"


# define coords
# https://stackoverflow.com/questions/36579767/add-unit-labels-to-radar-plot-and-remove-outer-ring-ggplot2-spider-web-plot-co
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  
  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }
  
  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }
  
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)
            
            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)
            
            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
            
            # This gets the proper theme element for theta and r grid lines:
            #   panel.grid.major.x or .y
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")
            
            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),
              
              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}


# tbl1 ####
tbl1 <- read_excel("tbl1.xlsx") %>%
#  janitor::clean_names() %>%
  pivot_longer(
    cols = c("Non-Sustained Users","Sustained Users"),
    names_to = "user",
    values_to = "n"
  )

tbl1 %>%
  filter(!(group %in% c("n", "Age", "SES"))) %>%
  arrange(user,group, desc(var)) %>%
  group_by(group, user) %>% 
#  summarise(count = sum(n)) %>% 
  mutate(perc = 100*n/sum(n)) %>%
  group_by(group, user) %>% 
  mutate(label_y = cumsum(perc) - 0.5 * perc) %>%
  ggplot(aes(user, perc, fill = var)) +
  geom_col(color = "black") +
  geom_text(aes(y = label_y, label = var), size = 4,
            family = font_g) +
  facet_wrap(. ~ group, scales = "free_x") +
  theme_bw(base_family =  font_g) +
  theme(
    axis.title = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Dark2") -> fig1

tbl1 %>%
  filter(group %in% c("Age", "SES")) %>%
  mutate(var = factor(var,
                      levels = c("13-19","6-12","Under 6","Low","Medium","High"))) %>%
  arrange(user,group, desc(var)) %>%
  group_by(group, user) %>% 
  #  summarise(count = sum(n)) %>% 
  mutate(perc = 100*n/sum(n)) %>%
  group_by(group, user) %>% 
  mutate(label_y = cumsum(perc) - 0.5 * perc) %>%
  ggplot(aes(user, perc, fill = var)) +
  geom_col(position = "dodge",color = "black") +
  geom_text(
    aes(label = var), size = 3,
    family = font_g,
    vjust = -1, position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(0,100)) +
  facet_wrap(. ~ group, scales = "free_x") +
  theme_bw(base_family =  font_g) +
  theme(
    axis.title = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Dark2")-> fig2

fig_1 <- grid.arrange(fig2,fig1, ncol = 1)
ggsave("fig1.png", fig_1, width = 9.97, height = 6.62,
       device = png, type = "quartz")
# tbl1_1 ####
tbl1_1 <- read_excel("tbl1_1.xlsx") %>%
  mutate(trait = str_to_upper(group)) %>% 
  mutate(trait = paste0(str_to_upper(group), "\n", label)) %>% 
  arrange(trait)

p_data_1 <- tbl1_1 %>%
  filter(user == "Sustained Users")
p_data_2 <- tbl1_1 %>%
  filter(user == "Non-Sustained Users")
p_1 <- ggplot(p_data_1, aes(x = trait,
                            y = total,
                            group = user))+
  geom_polygon(fill = sus_col, #"#b70102",
               colour = sus_col, 
               alpha = 0.4) +
  geom_point(colour = sus_col,
             size = 4) +
  geom_line(data = data.frame(x = rep(p_data_1$trait, 2),
                              y = c(rep(0, length(p_data_1$trait)), rep(100, length(p_data_1$trait)))),
            mapping = aes(x = x,
                          y = y,
                          group = x),
            colour = txt_col,
            alpha = 0.5)+
  geom_point(data = data.frame(x=p_data_1$trait, y=rep(100, length(p_data_1$trait))),
             inherit.aes = FALSE, 
             mapping = aes(x = x,
                           y = y),
             colour = txt_col,
             size = 4) +
  scale_y_continuous(limits = c(-40, 120),
                     breaks = seq(0, 100, 33.3))+
  coord_radar() +
  labs(x = "", 
       y = "",
       title = str_to_upper(p_data_1 %>% pull(user)),
       #caption = "N. Rennie | Data: Open-Source Psychometrics Project"
  ) +
  theme(plot.background = element_rect(fill = bkg_col, colour = bkg_col),
        panel.background = element_rect(fill = bkg_col, colour = bkg_col),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(colour = txt_col, family = font_g, size = trait_size),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(colour = txt_col),
        #     plot.margin=margin(t = 30, b = 30, r = 30, l = 20),
        plot.title = element_text(colour = txt_col, hjust = 0.5, family = font_g, size = title_size),
        plot.caption = element_text(colour=txt_col, size=10, hjust = 0.5, family = font_g),
  )

p_2 <- ggplot(p_data_2, aes(x = trait,
                            y = total,
                            group = user))+
  geom_polygon(fill = non_sus_col,
               colour = non_sus_col, 
               alpha = 0.4) +
  geom_point(colour = non_sus_col,
             size = 4) +
  geom_line(data = data.frame(x = rep(p_data_2$trait, 2),
                              y = c(rep(0, length(p_data_2$trait)), rep(100, length(p_data_2$trait)))),
            mapping = aes(x = x,
                          y = y,
                          group = x),
            colour = txt_col,
            alpha = 0.5)+
  geom_point(data = data.frame(x=p_data_2$trait, y=rep(100, length(p_data_2$trait))),
             inherit.aes = FALSE, 
             mapping = aes(x = x,
                           y = y),
             colour = txt_col,
             size = 4) +
  scale_y_continuous(limits = c(-40, 120),
                     breaks = seq(0, 100, 33.3))+
  coord_radar() +
  labs(x = "", 
       y = "",
       title = str_to_upper(p_data_2 %>% pull(user)),
       #caption = "N. Rennie | Data: Open-Source Psychometrics Project"
  ) +
  theme(plot.background = element_rect(fill = bkg_col, colour = bkg_col),
        panel.background = element_rect(fill = bkg_col, colour = bkg_col),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(colour = txt_col, family = font_g, size = trait_size),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(colour = txt_col),
        #   plot.margin=margin(t = 30, b = 30, r = 30, l = 20),
        plot.title = element_text(colour = txt_col, hjust = 0.5, family = font_g, size = title_size),
        plot.caption = element_text(colour=txt_col, size=10, hjust = 0.5, family = font_g),
  )

fig_1_1<- grid.arrange(p_1,p_2, ncol = 2)
ggsave("fig1_1.png", fig_1_1, width = 11.97, height = 6.62,
       device = png, type = "quartz")

# tbl2 ####
tbl2 <- read_excel("tbl2.xlsx") %>%
  pivot_longer(
    cols = c("Non-Sustained Users","Sustained Users"),
    names_to = "user",
    values_to = "p"
  ) %>%
  arrange(p) %>%
  mutate(var = factor(var, levels = unique(var)),
         p = ifelse(user == "Sustained Users", p, -p))

ggplot() +  
  geom_bar(data=tbl2, aes(x = var, y = p, fill = user), stat = "identity", width = .8) +
  coord_flip() +
  scale_fill_manual("", values=c("Non-Sustained Users"=non_sus_col, "Sustained Users"=sus_col), 
                    breaks=c("Non-Sustained Users","Sustained Users")) +
  # scale_x_reverse() +
  scale_y_continuous(limits=c(-30, 55), breaks=c(-30,-25,0,25,50), labels=c("", "25", "0", "25", "50")) +
  labs(#title="Survivor: Viewership", 
   # caption="\nbased on Pediatric comorbidity score",
    #   subtitle="On average, more people watch the finale\nthan the premier.",
    y="\nPercent (%)", x="") +
  theme(panel.background = element_rect(fill = bkg_col, colour=bkg_col),
        plot.background = element_rect(fill = bkg_col, colour=bkg_col),
        legend.background = element_rect(fill = bkg_col),
        plot.title = element_text(colour = txt_col, size=20, face="bold", hjust = 0, family=font_g),
        plot.subtitle = element_text(colour = txt_col, size=10, hjust = 0, family=font_g),
        plot.caption = element_text(colour = txt_col, size=10, hjust = 0, family=font_g),
        legend.position=c(0.8,0.15),
        legend.key = element_rect(colour = bkg_col, fill=bkg_col),
        plot.margin = unit(c(0.5, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.spacing.x = unit(0.5,"cm"),
        legend.title = element_text(colour = txt_col, size=14, hjust = 0.5, family=font_g),
        legend.text = element_text(colour=txt_col, size=14, family=font_g, hjust = 0),
        axis.title.y= element_text(colour = txt_col, size=14, family=font_g),
        axis.title.x= element_text(colour = txt_col, size=14, family=font_g),
        axis.text.y=element_text(colour = txt_col, size=14, hjust = 1, family=font_g),
        axis.text.x=element_text(colour = txt_col, size=14, hjust = 1, family=font_g),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) -> fig_2
fig_2
ggsave("fig2.png", fig_2, width = 11.97, height = 6.62,
       device = png, type = "quartz")

# tbl2_2 ####
tbl2_1 <- read_excel("tbl2.xlsx") %>%
  pivot_longer(
    cols = c("Non-Sustained Users","Sustained Users"),
    names_to = "user",
    values_to = "value"
  ) %>%
  mutate(total = value,
         label = paste0(value, "%"),
         trait = paste0(str_to_upper(var), "\n", label)) %>%
  arrange(trait) 

p_data_1_2 <- tbl2_1 %>%
  filter(user == "Sustained Users")
p_data_2_2 <- tbl2_1 %>%
  filter(user == "Non-Sustained Users")

p_1_2 <- ggplot(p_data_1_2, aes(x = trait,
                            y = total,
                            group = user))+
  geom_polygon(fill = sus_col,
               colour = sus_col, 
               alpha = 0.4) +
  geom_point(colour = sus_col,
             size = 4) +
  geom_line(data = data.frame(x = rep(p_data_1_2$trait, 2),
                              y = c(rep(0, length(p_data_1_2$trait)), rep(50, length(p_data_1_2$trait)))),
            mapping = aes(x = x,
                          y = y,
                          group = x),
            colour = txt_col,
            alpha = 0.5)+
  geom_point(data = data.frame(x=p_data_1_2$trait, y=rep(50, length(p_data_1_2$trait))),
             inherit.aes = FALSE, 
             mapping = aes(x = x,
                           y = y),
             colour = txt_col,
             size = 4) +
  scale_y_continuous(limits = c(-40, 50),
                     breaks = seq(0, 50, 12.5))+
  coord_radar() +
  labs(x = "", 
       y = "",
       title = str_to_upper(p_data_1_2 %>% pull(user)),
       #caption = "N. Rennie | Data: Open-Source Psychometrics Project"
  ) +
  theme(plot.background = element_rect(fill = bkg_col, colour = bkg_col),
        panel.background = element_rect(fill = bkg_col, colour = bkg_col),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(colour = txt_col, family = font_g, size = trait_size),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(colour = txt_col),
        #     plot.margin=margin(t = 30, b = 30, r = 30, l = 20),
        plot.title = element_text(colour = txt_col, hjust = 0.5, family = font_g, size = title_size),
        plot.caption = element_text(colour=txt_col, size=10, hjust = 0.5, family = font_g),
  )

p_2_2 <- ggplot(p_data_2_2, aes(x = trait,
                            y = total,
                            group = user))+
  geom_polygon(fill = non_sus_col,
               colour = non_sus_col, 
               alpha = 0.4) +
  geom_point(colour = non_sus_col,
             size = 4) +
  geom_line(data = data.frame(x = rep(p_data_2_2$trait, 2),
                              y = c(rep(0, length(p_data_2_2$trait)), rep(50, length(p_data_2_2$trait)))),
            mapping = aes(x = x,
                          y = y,
                          group = x),
            colour = txt_col,
            alpha = 0.5)+
  geom_point(data = data.frame(x=p_data_2_2$trait, y=rep(50, length(p_data_2_2$trait))),
             inherit.aes = FALSE, 
             mapping = aes(x = x,
                           y = y),
             colour = txt_col,
             size = 4) +
  scale_y_continuous(limits = c(-40, 50),
                     breaks = seq(0, 50, 12.5))+
  coord_radar() +
  labs(x = "", 
       y = "",
       title = str_to_upper(p_data_2_2 %>% pull(user)),
       #caption = "N. Rennie | Data: Open-Source Psychometrics Project"
  ) +
  theme(plot.background = element_rect(fill = bkg_col, colour = bkg_col),
        panel.background = element_rect(fill = bkg_col, colour = bkg_col),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(colour = txt_col, family = font_g, size = trait_size),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(colour = txt_col),
        #   plot.margin=margin(t = 30, b = 30, r = 30, l = 20),
        plot.title = element_text(colour = txt_col, hjust = 0.5, family = font_g, size = title_size),
        plot.caption = element_text(colour=txt_col, size=10, hjust = 0.5, family = font_g),
  )


fig_2_2 <- grid.arrange(p_1_2,p_2_2, ncol = 2)
ggsave("fig2_2.png", fig_2_2, width = 11.97, height = 6.62,
       device = png, type = "quartz")

# Forest plot ####

dat <- tibble::tibble(Index = c(1, 2, 3, 4),
                      OR  = c(1, 2.38, 4.29, 6.05),
                      LL = c(NA, 1.73, 3.0, 3.59),
                      UL = c(NA, 3.27, 6.16, 10.2),
                      label = c("Codeine", "Tramadol", "Oxycodone", "Other"),
                      img = c("/Users/doratias/Documents/statistical analysis/opioid_thesis/codeine.jpeg",
                              "/Users/doratias/Documents/statistical analysis/opioid_thesis/tramadol.jpeg",
                              "/Users/doratias/Documents/statistical analysis/opioid_thesis/oxy.jpeg",
                              "/Users/doratias/Documents/statistical analysis/opioid_thesis/other.jpeg")) %>%
  mutate(CI = ifelse(is.na(LL), "",paste(LL,UL, sep = ", ")))

plot1 <- ggplot(dat, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:4, labels = dat$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black", family = font_g),
        axis.text.x.bottom = element_text(size = 14, colour = "black", family = font_g),
        axis.title.x = element_text(size = 16, colour = "black", family = font_g))

## Create the table-base pallete
table_base <- ggplot(dat, aes(y=label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(OR, digits = 1))), size = 6) + ## decimal places
  ggtitle("OR")+
  theme(plot.title = element_text(face="bold", size=16, family = font_g))

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = CI), size = 6) + 
  ggtitle("95% CI")+
  theme(plot.title = element_text(face="bold", size=16, family = font_g))

lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
fp <- grid.arrange(plot1, tab1, tab2, layout_matrix = lay)
ggsave("forest_plot.png", fp, width = 11.97, height = 6.62,
       device = png, type = "quartz")

dat <- tibble::tibble(Index = c(1, 2, 3, 4),
                      OR  = c(1, 2.38, 4.29, 6.05),
                      OR_l = c("Ref", "2.4", "4.3", "6"),
                      LL = c(NA, 1.73, 3.0, 3.59),
                      UL = c(NA, 3.27, 6.16, 10.2),
                      label = c("Codeine", "Tramadol", "Oxycodone", "Other"),
                      img = c("/Users/doratias/Documents/statistical analysis/opioid_thesis/codeine.jpeg",
                              "/Users/doratias/Documents/statistical analysis/opioid_thesis/tramadol.jpeg",
                              "/Users/doratias/Documents/statistical analysis/opioid_thesis/oxy.jpeg",
                              "/Users/doratias/Documents/statistical analysis/opioid_thesis/other.jpeg")) %>%
  mutate(CI = ifelse(is.na(LL), "",paste(LL,UL, sep = ", ")))

pil_bkg <- "#81B8E3"
ci_col <- "black"
dot_down <- 0.2
ggplot(dat, aes(y = OR, x = Index)) +
  geom_image(mapping = aes(image = img),
             asp = 4.5/6,
             size = 0.2) +
  geom_text(aes(x = Index + 0.35, y = OR, label = label),
    hjust = 0.5, size = 8, family = "chewy", angle = 270) +
  geom_text(aes(x = Index, y = OR-0.8, label = OR_l),
            hjust = 0.5, size = 7, family = "chewy") +
  geom_point(data = dat %>% filter(label != "Codeine"),
             aes(Index-0.3, OR-dot_down),shape = 16, size = 3,
             color = ci_col) + 
  geom_segment(aes(x = Index-0.3, y = LL-dot_down, xend = Index-0.3, 
                   yend = UL-dot_down), color = ci_col, size = 2) +
  geom_point(data = dat %>% filter(label != "Codeine"),
             aes(Index-0.3, UL-dot_down),shape = 16, size = 1.5,
             color = ci_col) + 
  geom_point(data = dat %>% filter(label != "Codeine"),
             aes(Index-0.3, LL-dot_down),shape = 16, size = 1.5,
             color = ci_col) + 
  # geom_segment(aes(x = Index-0.35, y = LL-dot_down, xend = Index-0.25, 
  #                  yend = LL-dot_down), color = ci_col, size = 1.5) +
  # geom_segment(aes(x = Index-0.35, y = UL-dot_down, xend = Index-0.25, 
  #                  yend = UL-dot_down), color = ci_col, size = 1.5) +
  labs(title = "",#"Risk for sustained opioid use\nby substances ",
       x = "",
       y = "",
       caption = "Images imported from Vecteezy.com") +
  scale_x_continuous(name = "",breaks=1:4, labels = dat$label) +
  coord_cartesian(xlim = c(0.8,4.5),
                  ylim = c(-1,10)) +
  theme(plot.background = element_rect(fill = pil_bkg, colour = pil_bkg),
        panel.background = element_rect(fill = pil_bkg, color = pil_bkg),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 36, family = font_g),
       # plot.title.position = "plot",
      #  plot.caption.position = "plot",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> cool_or

cool_or
ggsave("cool_or.png", cool_or, width = 33.87, height = 19.05, units = "cm",
       device = png, type = "quartz")

# ooo ####
img_url <-"~/Documents/stat_projects/opioid_peds/pain - confrence/pill.avif"
line_s <- 1.2
txt_s <- 12

tibble(
  dates = c(1:12),
  y = rep(1,12),
  pres = c(NA,NA,"1st",NA,NA,"2nd","3rd",NA,NA,NA,NA,NA),
  month = c("01","02","03", "04", "05", "06","07","08","09","10","11","12"),
  img = c(NA,NA,img_url,NA,NA,img_url,img_url,NA,NA,NA,NA,NA)
) %>%
  ggplot(aes(dates)) +
  geom_image(mapping = aes(y = y+0.15,
                           image = img),
             asp = 4.5/6,
             size = 1,
             angle = 45) +
  geom_text(aes(y = y+0.3,label = pres), 
            family = font_g, size = 10) +
  geom_text(aes(y = y-0.1,label = month), 
            family = font_g, size = 8) +
  geom_segment(aes(x = 1, xend = 12, y = y, yend = y),
               linewidth = line_s, lineend = "round") +
  geom_segment(aes(x = dates, xend = dates, y = y-0.05, yend = y+0.05),
               linewidth = line_s, lineend = "round") +
  labs(y = "", x = "") +
  coord_cartesian(xlim = c(0,13),
                  ylim = c(0.85,1.35)) +
  theme(
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) -> pill_a
pill_a
ggsave("/Users/doratias/Documents/stat_projects/opioid_peds/pain - confrence/pill_a.png", 
       pill_a, width = 35, height = 5, units = "cm",
       device = png, type = "quartz")

tibble(
  dates = c(1:12),
  y = rep(1,12),
  month = c("01","02","03", "04", "05", "06","07","08","09","10","11","12"),
  dos = c("10-MME",NA,NA,NA,"12.5-MME",NA,NA,NA,"10-MME",NA,NA,"20-MME"),
  img2 = c(img_url,NA,NA,NA,img_url,NA,NA,NA,img_url,NA,NA,img_url),
) %>%
  ggplot(aes(dates)) +
  geom_image(mapping = aes(y = y+0.15,
                           image = img2),
             asp = 4.5/6,
             size = 1,
             angle = 45) +
  geom_text(aes(y = y+0.3,label = dos), 
            family = font_g, size = 8) +
  geom_text(aes(y = y-0.1,label = month), 
            family = font_g, size = 8) +
  geom_segment(aes(x = 1, xend = 12, y = y, yend = y),
               size = line_s, lineend = "round") +
  geom_segment(aes(x = dates, xend = dates, y = y-0.05, yend = y+0.05),
               size = line_s, lineend = "round") +
  labs(y = "", x = "") +
  coord_cartesian(xlim = c(0,13),
                  ylim = c(0.85,1.35)) +
  theme(
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) + 
  annotate(
    geom = "curve", x = 1.9, y = 1.2, xend = 1.6, yend = 1.3, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 1.9, y = 1.15, label = "1st", hjust = "center",
           fontface =2, size = 8, family = font_g) + 
  annotate(
    geom = "curve", x = 4, y = 1.2, xend = 4.25, yend = 1.3, 
    curvature = -.3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 4, y = 1.15, label = "2nd", hjust = "center",
           fontface =2, size = 8, family = font_g) + 
  annotate(
    geom = "curve", x = 11, y = 1.2, xend = 11.3, yend = 1.3, 
    curvature = -.3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 11, y = 1.15, label = "3rd", hjust = "center",
           fontface =2, size = 8, family = font_g) -> pill_b
pill_b
ggsave("/Users/doratias/Documents/stat_projects/opioid_peds/pain - confrence/pill_b.png", 
       pill_b, width = 35, height = 5, units = "cm",
       device = png, type = "quartz")

