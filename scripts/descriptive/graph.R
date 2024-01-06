Sys.setlocale("LC_ALL", "Hebrew")
library(tidyverse)
library(lubridate)
library(survival)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(grid)
library(ggsci)
library(RColorBrewer)

source(file.path("funcs.R"), encoding = "utf8")

set.seed(45)
cat("\f")

# Create graphs setting -------------------------------------------------------
plot_theme <- theme(
  plot.title = element_text(size = 25, hjust = 0.5),
  axis.title = element_text(size = 17),
  axis.text = element_text(size = 15),
  legend.text = element_text(size = 15),
  strip.text = element_text(size = 15))

color_scale <- scale_fill_brewer(palette='Dark2')

one_color <- "skyblue3"

clalit_num <- read_csv("clalit_num.csv", show_col_types = FALSE)

# Fig2 -------------------------------------------------------------------------
## Purchase per 100,000 -------------------------------------------------------
max_y <- 1700
purch_refine %>%
  filter(!(generic %in% exclusion_drugs[!exclusion_drugs %in% "Propoxyphene"])) %>%
  mutate(generic = case_when(generic == "Codeine - CodAcamol" ~ "Codeine",
                             generic == "Codeine - Rokacet" ~ "Codeine",
                             TRUE ~ as.character(generic)),
         generic = factor(fct_collapse(generic,
                                       Other = c("Buprenorphine", "Fentanyl", "Morphine")),
                          levels = c( "Propoxyphene", "Codeine", "Tramadol", "Oxycodone","Other")),
         year = year(RE_date)) %>%
  filter(year < 2022,
         year > 2002) %>%
  group_by(generic, year) %>%
  summarise(n = n()) %>%
  mutate(n_5 = n / clalit_num[clalit_num$year %in% as.numeric(year), "num", drop = TRUE] * 100000) %>%
  ggplot(aes(year, n_5, fill = generic)) +
  geom_area(color="black")+
  labs(title = NULL,
       x = "Year",
       y = "Purchases per 100,000",
       fill=' ')+
  scale_x_continuous(breaks=seq(2003,2021,1)) +
  geom_vline(xintercept = 2008, color = "skyblue2", size = 1, linetype="dashed") +
  geom_vline(xintercept = 2009, color = "skyblue2", size = 1, linetype="dashed") +
  geom_vline(xintercept = 2012, color = "royalblue1", size = 1, linetype="dashed") +
  annotate(geom = "point", x = 2008, y = max_y, colour = "chocolate1", size = 12, alpha = 0.8) + 
  annotate(geom = "text", x = 2008, y = max_y, label = "1", size = 6) +
  annotate(geom = "point", x = 2009, y = max_y, colour = "chocolate1", size = 12, alpha = 0.8) + 
  annotate(geom = "text", x = 2009, y = max_y, label = "2", size = 6) +
  annotate(geom = "point", x = 2012, y = max_y, colour = "chocolate1", size = 12, alpha = 0.8) + 
  annotate(geom = "text", x = 2012, y = max_y, label = "3", size = 6) + 
  theme_classic()+
  plot_theme +
  theme(legend.position='top') +
  scale_fill_manual(values = c("#E6AB02", "#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) ->f21

zoomed_f21 <- f21+
  coord_cartesian(xlim = c(2011,2021), ylim = c(0,400)) +
  labs(x = "", y ="") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

f21 +
  annotation_custom(ggplotGrob(zoomed_f21), 
                    xmin = 2013, xmax = 2021, ymin = 700, ymax = 1800)

## MME per capita --------------------------------------------------------------
max_y_mme <- 0.5
purch_refine %>%
  filter(!(generic %in% exclusion_drugs[!exclusion_drugs %in% "Propoxyphene"])) %>%
  mutate(generic = case_when(generic == "Codeine - CodAcamol" ~ "Codeine",
                             generic == "Codeine - Rokacet" ~ "Codeine",
                             TRUE ~ as.character(generic)),
         generic = factor(fct_collapse(generic,
                                       Other = c("Buprenorphine", "Fentanyl", "Morphine")),
                          levels = c("Propoxyphene", "Codeine", "Tramadol", "Oxycodone","Other")),
         year = year(RE_date)) %>%
  filter(year < 2022,
         year > 2002) %>%
  group_by(generic, year) %>%
  summarise(n = sum(mme)) %>%
  mutate(n_5 = n / clalit_num[clalit_num$year %in% as.numeric(year), "num", drop = TRUE] * 1) %>%
  ggplot(aes(year, n_5, fill = generic)) +
  geom_area(color="black")+
  labs(title = NULL,
       x = "Year",
       y = "MME per capita",
       fill=' ')+
  scale_x_continuous(breaks=seq(2003,2021,1)) +
  geom_vline(xintercept = 2008, color = "skyblue2", size = 1, linetype="dashed") +
  geom_vline(xintercept = 2009, color = "skyblue2", size = 1, linetype="dashed") +
  geom_vline(xintercept = 2012, color = "royalblue1", size = 1, linetype="dashed") +
  theme_classic()+
  plot_theme +
  theme(legend.position='top') +
  scale_fill_manual(values = c("#E6AB02", "#1B9E77", "#D95F02", "#7570B3", "#E7298A")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) -> f22

ggarrange(f21 + rremove("x.axis") + 
            rremove("xlab") + 
            rremove("x.text") + 
            rremove("x.ticks"),
          f22, nrow = 2,
          align = "v",
          labels = c("A", "B"),
          common.legend = TRUE, legend = "top")

ggsave(filename = file.path("Opioid - descriptive","graphs","fig2.pdf"), plot = ggplot2::last_plot(), 
       width = 30, height = 20, dpi = 300, units = "cm", bg = "white")

# Fig3 -------------------------------------------------------------------------
misuser_prev <- final_cohort%>%
  select(RE_age, RE_date, time, outcome) %>%
  rowwise() %>%
  mutate(start_year = year(RE_date),
         end_year = year(RE_date + months(time)),
         time = end_year - start_year + 1)

pp <- data.frame(pyears(time ~ start_year + outcome, misuser_prev)$n)
fact <- 2
pp %>%
  mutate(year = row.names(pp),
         n = Proper.User + Misuser,
         mis_5 = Misuser / clalit_num[clalit_num$year %in% as.numeric(row.names(pp)), "num", drop = TRUE]  * 100000,
         p = round(100 * (Misuser/n),2)) %>%
  ggplot(aes(x = year, y = mis_5)) +
  geom_col(color = "black",fill = one_color) +
  geom_point(aes(y = p*fact), color = "chocolate1", size = 3) +
  geom_line(aes(y = p*fact, group = 1), linewidth = 0.5) +
  scale_y_continuous(
    sec.axis = sec_axis(~./fact, 
                        name="Percentage of new sustained users of overall opioid users <19 years old")
  ) + 
  labs(
    title = NULL,
    x = "Year",
    y = "Annual incidence of sustained opioid users <19 years old, per 100,000",
    fill='')+
  theme_classic()+
  plot_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size = 14))

ggsave(filename = file.path("Opioid - descriptive","graphs","fig3.pdf"), 
       plot = ggplot2::last_plot(), 
       width = 30, height = 20, dpi = 300, units = "cm", bg = "white")

# Fig4 -------------------------------------------------------------------------
## fig 4 - A -------------------------------------------------------------------
purch_refine %>%
  filter(id %in% cohort_id,
         !(generic %in% exclusion_drugs)) %>%
  mutate(generic = factor(case_when(generic == "Codeine - CodAcamol" ~ "Codeine",
                                    generic == "Codeine - Rokacet" ~ "Codeine",
                                    TRUE ~ as.character(generic)),
                          levels = c("Codeine","Tramadol","Oxycodone",  
                                     "Buprenorphine", "Fentanyl", "Morphine")),
         year = year(RE_date)) %>%
  filter(year < 2022) %>%
  group_by(generic, year) %>%
  summarise(n = sum(mme)) %>%
  mutate(n_5 = n / clalit_num[clalit_num$year %in% as.numeric(year), "num", drop = TRUE]) %>%
  ggplot(aes(year, n_5, group = generic)) +
  geom_point(aes(color = generic), size = 3) +
  geom_line(color = "black",linewidth = 1, alpha = 0.4) +
  labs(title = NULL,
       x = ' ',
       y = ' ',
       color=' ')+
  scale_x_continuous(breaks=seq(2003,2021,1)) +
  theme_classic()+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3",
                                "#66A61E", "#E7298A", "#A6761D")) +
  theme(legend.position='top',
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) -> f31

## fig 4 - B -------------------------------------------------------------------
purch_corrected %>%
  group_by(id, start_date,RE_date,generic, factor) %>%
  summarise(mme = sum(mme)) %>%
  left_join(membership_dates %>% select(id,member_start,member_end), by = "id") %>%
  filter(id %in% cohort_id,
         !(generic %in% exclusion_drugs),
         RE_date >= member_start,
         RE_date <= member_end,
         RE_date <= as.Date("2022-12-31")) %>%
  ungroup() %>%
  mutate(strength = case_when(
    factor >= 1 ~ "Strong Opioid", 
    generic  == "Fentanyl" ~ "Strong Opioid", 
    TRUE ~"Weak Opioid"),
    year = year(RE_date)) %>%
  filter(year < 2022,
         year > 2011) -> f32_data

f32_data %>%
  group_by(strength, year) %>%
  summarise(n = sum(mme)) %>%
  mutate(n_5 = n / clalit_num[clalit_num$year %in% as.numeric(year), "num", drop = TRUE]) %>%
  ggplot(aes(year, n_5, group = strength)) +
  geom_point(aes(color = strength), size = 3) +
  geom_line(color = "black",linewidth = 1, alpha = 0.4) +
  labs(title = NULL,
       x = ' ',
       y = ' ',
       color=' ')+
  scale_x_continuous(breaks=seq(2003,2021,1)) +
  theme_classic()+
  theme(legend.position='top',
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) -> f32

## fig 4 - C -------------------------------------------------------------------
purch_refine %>%
  left_join(any_cancer, by = "id") %>%
  mutate(cancer = factor(ifelse(cancer, "Cancer", "Non-Cancer"),
                         levels = c("Non-Cancer", "Cancer")),
         year = year(RE_date)) %>%
  filter(id %in% cohort_id,
         !is.na(cancer),
         !(generic %in% exclusion_drugs),
         year < 2022) %>% 
  group_by(cancer, year) %>%
  summarise(n = sum(mme)) %>%
  mutate(n_5 = n / 100000) %>%
  ggplot(aes(year, n_5, group = cancer)) +
  geom_point(aes(color = cancer), size = 3) +
  geom_line(color = "black",linewidth = 1, alpha = 0.4) +
  labs(title = NULL,
       x = ' ',
       y = ' ',
       color=' ')+
  scale_x_continuous(breaks=seq(2003,2021,1)) +
  theme_classic()+
  theme(legend.position='top',
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) -> f33

## fig 4 - D -------------------------------------------------------------------
purch_refine %>%
  filter(id %in% final_cohort$id) %>%
  left_join(final_cohort %>% select(id, periphery), by = "id") %>%
  mutate(year = year(RE_date)) %>%
  filter(!is.na(periphery),
         !(generic %in% exclusion_drugs),
         year < 2022) %>% 
  group_by(periphery, year) %>%
  summarise(n = sum(mme)) %>%
  mutate(n_5 = n / 100000,
         periphery = factor(periphery, levels = c("Periphery","Center"))) %>%
  ggplot(aes(year, n_5, group = periphery)) +
  geom_point(aes(color = periphery), size = 3) +
  geom_line(color = "black",linewidth = 1, alpha = 0.4) +
  labs(title = NULL,
       x = ' ',
       y = ' ',
       color=' ')+
  scale_x_continuous(breaks=seq(2003,2021,1)) +
  theme_classic()+
  theme(legend.position='top',
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) -> f34

## fig 4 - complete ------------------------------------------------------------
f3 <- ggarrange(f32, f31, f33, f34,
                align = "hv",
                labels = c("A", "B", "C", "D"))

annotate_figure(f3, left = textGrob("Total MME / 100,000                                MME per capita", 
                                    rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Year", gp = gpar(cex = 1.3)))

ggsave(filename = file.path("Opioid - descriptive","graphs","fig4.pdf"), plot = ggplot2::last_plot(), 
       width = 30, height = 20, dpi = 300, units = "cm", bg = "white")

# Fig2s - MME per capita -------------------------------------------------------
purch_corrected %>%
  filter(generic == "Fentanyl") %>%
  mutate(drug = case_when(
    str_detect(RE_med, "PAT") ~ "Fentanyl PAT",
    str_detect(RE_med, "LOZ") ~ "Fentanyl LOZ",
    str_detect(RE_med, "BUCCAL") ~ "Fentanyl LOZ",
    str_detect(RE_med, "ABSTRAL") ~ "Fentanyl LOZ",
    str_detect(RE_med, "PECFENT") ~ "Fentanyl LOZ",
    str_detect(RE_med, "INJ") ~ "Fentanyl INJ",
    TRUE ~ as.character(RE_med)
  )) %>%
  left_join(any_cancer, by = "id") %>%
  mutate(cancer = factor(ifelse(cancer, "Cancer", "Non-Cancer"),
                         levels = c("Non-Cancer", "Cancer")),
         year = year(RE_date)) %>%
  filter(id %in% cohort_id,
         !is.na(cancer),
         year < 2022,
         year > 2011) %>% 
  group_by(year, cancer, drug) %>%
  summarise(n = n()) %>%
  ggplot(aes(year, n, color = drug)) + 
  geom_line(linewidth = 1) + 
  facet_grid(cancer ~ .) +
  labs(title = NULL,
       x = "Year",
       y = "Purchases",
       color=' ')+
  scale_x_continuous(breaks=seq(2003,2021,1)) +
  theme_classic()+
  plot_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")


ggsave(filename = file.path("Opioid - descriptive","graphs","fig2s.pdf"), plot = ggplot2::last_plot(), 
       width = 30, height = 20, dpi = 300, units = "cm", bg = "white")

