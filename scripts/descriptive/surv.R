Sys.setlocale("LC_ALL", "Hebrew")
library(tidyverse)
library(survival)
library(ranger)
library(survminer)
library(janitor)
library(survMisc)
library(gtsummary)
library(lubridate)
library(gtools)
library(randomForest)
library(gridExtra)
library(showtext)
library(extrafont)
font_import()

source(file.path("funcs.R"), encoding = "utf8")
load("op_data_big_files.RData")
load("op_data.RData")
set.seed(45)
cat("\f")

# prepare data for survival analysis ####
surv <- final_cohort %>%
  filter(!(generic %in% c("Methadone","Pethidine", "Propoxyphene"))) %>%
  mutate(year = factor(year(RE_date)),
         covid = RE_date >= as.Date("2020-03-01"),
         generic = factor(case_when(
           generic %in% c("Buprenorphine", "Fentanyl", "Morphine") ~ "Other",
           TRUE ~ as.character(generic)),
           levels = c("Codeine","Tramadol", "Oxycodone", "Other")),
         sector = factor(case_when(
           sector == "Bedouin" ~ "Arab", 
           sector == "Cherkess" ~ "Arab", 
           sector == "Arab - others" ~ "Arab", 
           sector == "Religious mixed" ~ "General",
           TRUE ~ as.character(sector)
         ), levels = c("General", "Arab")),
         periphery = factor(periphery, levels = c("Center","Periphery"))
  ) %>%
  select(id, age_group, year, covid, gender, SES, sector, periphery,
         generic, outcome, time, n_visits, drug_sum, 
         alcohol_abuse,anemia,anxiety,malignancy,asthma,cardiovascular,chromosomal_anomalies,
         condact_disorder,congenital_malformations, depression,developmental_delay,diabetes_melitus,
         drug_abuse,eating_disorders,epilepsy,GI,joints,menstrual,nausa_vomit,
         pain,psychotic,sleep,smoking,weight_loss,pci) %>%
  mutate(n_visits = ifelse(is.na(n_visits),FALSE,n_visits > 12),
         drug_sum = ifelse(is.na(drug_sum),FALSE,drug_sum > 12))

surv_id <- surv %>% pull(id)

## impute missing data ####
imp_data <- surv %>% 
  mutate_if(is.logical, function(x) {ifelse(is.na(x), FALSE, x)}) %>%
  mutate_if(is.logical, function(x) {ifelse(x, 1, 0)})

surv_imputed <- rfImpute(outcome ~ .-id, 
                         data = imp_data)

surv_imputed <- surv_imputed %>%
  mutate(time = ifelse(outcome == "Misuser", time + 1, time),
         outcome = ifelse(outcome == "Misuser", 1, 0))

# Kaplan Meier Analysis ####
km <- survfit(Surv(time, outcome) ~ generic, 
              data = surv_imputed %>%
                mutate(time = ifelse(time>120, 120, time))%>% 
                select(-id))

ggsurvplot(km,
           pval = FALSE, conf.int = TRUE,
           conf.int.alpha = 0.2,
           fun = "event",
           palette = 'Dark2',
           break.x.by = 24,
           risk.table = TRUE,
           cumcensor = TRUE,
           risk.table.y.text = FALSE,
           tables.y.text = FALSE,
           fontsize = 6,
           linetype = "strata", 
           xlab = "Follow-up time (Months)",
           ylab = "Comulative incidence\nof sustained opioid use (%)",
           legend = "bottom",
           legend.title = "",
           legend.labs = c("Codeine", "Tramadol", "Oxycodone", "Other"),
           ggtheme = theme_classic() + theme(
             plot.title = element_text(size = 16),
             axis.title = element_text(size = 16),
             axis.text = element_text(size = 16),
             legend.text = element_text(size = 16),
             strip.text = element_text(size = 16),
           )) -> f5
f5$plot + 
  geom_vline(color = "grey30",xintercept = 24, linetype = "longdash", linewidth = 1)

f5$plot <- f5$plot + 
  geom_vline(color = "grey30",xintercept = 24, linetype = "longdash", linewidth = 1)
f5

ggsave(filename = file.path("fig5.pdf"), plot = print(f5), 
       width = 30, height = 20, dpi = 300, units = "cm", bg = "white")

# Cox Model ####
cox <- coxph(Surv(time, outcome) ~ ., data = surv_imputed %>% select(-id))
summary(cox)
AIC(cox)
cz <- cox.zph(cox)
print(cz)
plot(cz)
print(ggcoxzph(cz), newpage = TRUE)

tbl_regression(cox, exp=TRUE)

# COX - Time varying covariate ####
## create df of all opioid use dates  ####
purch_surv <- purch %>%
  filter(id %in% surv_imputed$id) %>%
  left_join(surv_imputed %>% select(id,f_time=time), by = "id") %>%
  mutate(int = as.numeric(floor(difftime(RE_date, start_date,unit = "days")/30)),
         time = ifelse(int < 0, 0, int)) %>%
  filter(year(RE_date) > 2011,
         !(generic %in% c("Methadone","Pethidine", "Propoxyphene")),
         time <= f_time +1) %>%
  mutate(generic = case_when(generic == "Codeine - CodAcamol" ~ "Codeine",
                             generic == "Codeine - Rokacet" ~ "Codeine",
                             TRUE ~ as.character(generic)),
         generic = factor(fct_collapse(generic,
                                       Other = c("Buprenorphine", "Fentanyl", "Morphine")),
                          levels = c("Codeine", "No Use","Tramadol", "Oxycodone", "Other")),
         covid = ifelse(RE_date >= as.Date("2020-03-25"),1,0)) %>%
  group_by(id, time) %>%
  summarise(generic = generic[which(mme == max(mme))]) %>%
  ungroup()

## add "no use" time ####
new_times <- no_use(purch_surv) %>%
  mutate(time = as.numeric(time))

## create cancer dates df  ####
surv_cancer <- diagnosis %>%
  filter(id %in% surv_imputed$id) %>%
  left_join(membership_dates %>% select(id,member_start,member_end), by = "id") %>%
  filter(RE_date >= start_date,
         RE_date <= member_end,
         RE_date <= as.Date("2022-12-31")) %>%
  left_join(surv_imputed %>% select(id,f_time=time), by = "id") %>%
  mutate(int = as.numeric(floor(difftime(RE_date, start_date,unit = "days")/30)),
         time = ifelse(int < 0, 0, int),
         malignancy = ifelse(malignancy, 1, 0)) %>%
  select(id, time, malignancy) %>%
  group_by(id) %>% 
  summarise(time_0 = min(time[which(malignancy == 0)],na.rm = TRUE),
            time_1 = min(time[which(malignancy == 1)],na.rm = TRUE)) %>%
  mutate(time_1 = ifelse(is.infinite(time_1), NA, time_1),
         time_1 = ifelse(time_1 < 2, 2, time_1), 
         time_0 = ifelse(time_0 < 2, 2, time_0),
         time_0 = ifelse(time_0 >= time_1, NA, time_0)) %>% 
  pivot_longer(cols = c("time_1", "time_0"), names_to = "malignancy", values_to = "time",values_drop_na = TRUE) %>%
  mutate(malignancy = ifelse(malignancy == "time_1", 1,0))

## create COVID df ####
final_cohort %>%
  filter(id %in% surv_imputed$id) %>%
  select(id, RE_date) %>%
  left_join(surv_imputed %>% select(id,f_time = time), by = "id") %>%
  mutate(covid_1 = ifelse(RE_date >= as.Date("2020-03-01"),0,NA),
         date_2 = RE_date %m+% months(f_time),
         t = as.numeric(floor(difftime(as.Date("2020-03-25"), RE_date,unit = "days")/30)),
         covid_2 = ifelse(date_2 >= as.Date("2020-03-01"),t,NA)) %>%
  pivot_longer(!c(id,RE_date,f_time,date_2,t), names_to = "covid", values_to = "time") %>%
  filter(t < f_time) %>%
  mutate(covid = ifelse(is.na(time), 0, 1),
         time = ifelse(is.na(time) | time < 0, 0, time)) %>%
  select(id, covid, time)-> covid_time

## merge all data to cox tv analysis ####
surv_drugs_time <- tmerge(data1 = surv_imputed %>% 
                            mutate(outcome = ifelse(outcome == "Misuser", 1, 0),
                                   SES = factor(SES, ordered = FALSE, levels = c("Low", "Medium", "High"))),
                          data2 = surv_imputed %>% 
                            mutate(outcome = ifelse(outcome == "Misuser", 1, 0),
                                   SES = factor(SES, ordered = FALSE, levels = c("Low", "Medium", "High"))),
                          id = id,
                          outcome = event(time, outcome))

surv_drugs_time <- tmerge(data1 = surv_drugs_time,
                          data2 = new_times,
                          id = id,
                          generic_time = tdc(time, generic))

surv_drugs_time <- tmerge(data1 = surv_drugs_time,
                          data2 = surv_cancer,
                          id = id,
                          malignancy_time = tdc(time, malignancy))

surv_drugs_time <- tmerge(data1 = surv_drugs_time,
                          data2 = covid_time,
                          id = id,
                          covid_time = tdc(time, covid))
## cox TV analysis ####
cox_time <- coxph(Surv(tstart,tstop, outcome) ~ ., 
                  data = surv_drugs_time %>% 
                    select(-c(id, time, covid,malignancy)) %>%
                    mutate(tstart = as.numeric(ifelse(tstart < 2 & tstop > 2, 2, tstart)),
                           tstop = as.numeric(tstop),
                           malignancy_time = ifelse(is.na(malignancy_time), 0, malignancy_time),
                           covid_time = ifelse(is.na(covid_time), 0, covid_time)) %>%
                    filter(tstart >= 2))
summary(cox_time)
AIC(cox_time)
cz_time <- cox.zph(cox_time)
print(cz_time)
plot(cz_time)

srg <- ggcoxzph(cz_time)
ggsave(file.path("Opioid - descriptive","graphs","ggcoxzph.pdf"), arrangeGrob(grobs = srg),
       width = 50, height = 35, dpi = 300, units = "cm", bg = "white")


tbl_regression(cox_time, exp=TRUE,
               show_single_row = c(gender, sector, n_visits, drug_sum,
                                   malignancy_time, cardiovascular, pain),
               label = list(
                 age_group ~ "Age Group",
                 gender ~ "Sex (M)",
                 SES ~ "Socioeconomic status", 
                 sector ~ "Sector (Arab)",
                 periphery ~ "Resident in the Periphery",
                 covid_time ~ "During COVID",
                 n_visits ~ "Doctor visits (>12)",
                 drug_sum ~ "Total drugs purchased (>12)",
                 malignancy_time ~ "Any Malignancy diagnosis during the study", 
                 cardiovascular ~ "Cardiovascular conditions", 
                 pain ~ "Pain conditions", 
                 generic ~ "Opioid substance at first purchase",
                 generic_time ~ "Opioid substances use over Time"
               ),
               include = -year
) -> cox_tbl

gt::gtsave(as_gt(cox_tbl), file = file.path("Opioid - descriptive","tables","surv.html"))

## Sensitivity analysis ####
cox_no_cancer <- coxph(Surv(tstart,tstop, outcome) ~  ., 
                       data = surv_drugs_time %>% 
                         filter(!(id %in% any_cancer$id[any_cancer$cancer])) %>%
                         select(-c(id, time, covid,malignancy, malignancy_time)) %>%
                         mutate(tstart = as.numeric(ifelse(tstart < 2 & tstop > 2, 2, tstart)),
                                tstop = as.numeric(tstop),
                                covid_time = ifelse(is.na(covid_time), 0, covid_time)) %>%
                         filter(tstart >= 2))
summary(cox_no_cancer)
