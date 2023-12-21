Sys.setlocale("LC_ALL", "Hebrew")
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)
library(stringr)
library(labelled)
library(caret)
library(randomForest)
library(gtsummary)
library(smd)
library(flextable)

source(file.path("funcs.R"), encoding = "utf8")
load(file.path("Rprojects","op_data.RData"))
set.seed(45)
cat("\f")

# Table 1 - Patients Characteristics ####
## sociodemograph ####
dem_vars <- c("outcome", "gender", "age_group" ,"SES", "periphery", "sector")
final_cohort %>%
  mutate(outcome = ifelse(outcome == "Misuser", "Sustained Users", "Non-Sustained Users")) %>%
  select(all_of(dem_vars)) %>%
  tbl_summary(by = "outcome",
              label = list(
                gender ~ "Sex (M)"
              ),
              missing = "no",
              type = all_continuous() ~ 'continuous2',
              value = list(
                gender ~ "Male"
              )) %>%
  add_overall() %>%
  add_difference(everything() ~ "smd") %>%
  modify_column_hide(ci) %>%
  modify_header(estimate = "**SMD**") %>%
  add_p() -> t11

## Medical ####
diag_var <- c("n_visits","drug_sum","anemia","malignancy", "asthma", "cardiovascular", "diabetes_melitus",
              "epilepsy", "GI","pain","smoking", "psychiatric_not_pci")

final_cohort %>% 
  mutate(outcome = ifelse(outcome == "Misuser", "Sustained Users", "Non-Sustained Users")) %>%
  select(outcome, pci, bmi, all_of(diag_var)) %>%
  tbl_summary(by = "outcome",
              type = list(pci ~ 'continuous2'),
              statistic = c(pci) ~ c("{median} ({p25}, {p75})",
                                     "{mean} ({sd})"),
              missing = "no") %>%
  add_overall() %>%
  add_difference(everything() ~ "smd") %>%
  modify_column_hide(ci) %>%
  modify_header(label = "**Condition**",
                estimate = "**SMD**") %>%
  add_p()  -> t12

## all ####
(t1 <- tbl_stack(list(t11,t12), 
                 group_header = c("Sociodemographic Characteristics", "Medical History")))

gt::gtsave(as_gt(t1), file = file.path("Opioid - descriptive","tables","t1.html"))

# Table 2. Healthcare Usage ####
## Visits ####
t2_vars <- c("outcome", "n_visits", "clinic_sector_general_num", "proffesion_proffesional", "drug_sum")
final_cohort %>%
  mutate(outcome = ifelse(outcome == "Misuser", "Sustained Users", "Non-Sustained Users")) %>%
  select(all_of(t2_vars)) %>%
  tbl_summary(by = "outcome",
              type = list(all_continuous() ~ 'continuous'),
              statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"),
              missing = "no"
  ) %>%
  modify_caption("**Table 2. Healthcare Usage**") %>%
  add_difference(everything() ~ "smd") %>%
  modify_column_hide(ci) %>%
  modify_header(estimate = "**SMD**") %>%
  add_p()  -> t2

t2
gt::gtsave(as_gt(t2), file = file.path("Rprojects","Opioid - descriptive","tables","t3.html"))

#  Treatment metrics ####
purch_vars <- c("outcome", "n_purch", "n_drugs", "generic", "follow_up", "med_purch_int", "med_months_from_first", 
                "dist_num", "sub_dist_num", "quan", "mme_per_pres", "mme_per_day", "total_mme",
                "time_to_outcome")
final_cohort %>%
  mutate(outcome = ifelse(outcome == "Misuser", "Sustained Users", "Non-Sustained Users")) %>%
  select(all_of(purch_vars)) %>%
  mutate(
    dic_dist_num = ifelse(dist_num > 1, ">1", "1"),
    dic_sub_dist_num = ifelse(sub_dist_num > 1, ">1", "1"),
    .after = sub_dist_num
  ) %>%
  select(-dist_num, -sub_dist_num) %>%
  tbl_summary(by = "outcome",
              label = list(
                dic_dist_num ~ "Different prescribing districts", 
                dic_sub_dist_num ~ "Different prescribing sub-districts"
              ),
              type = list(all_continuous() ~ 'continuous2'),
              statistic = c(n_drugs, med_months_from_first) ~ c("{median} ({p25}, {p75})",
                                                                "{mean} ({sd})"),
              missing = "no",
              value = list(
                dic_dist_num ~ ">1",
                dic_sub_dist_num ~ ">1"
              )) %>%
  add_overall() %>%
  add_n(statistic = "{n_miss} ({p_miss})") %>%
  modify_header(n = "**Missing**") %>%
  modify_caption("**Table 2. Treatment metrics**") %>%
  add_difference(everything() ~ "smd") %>%
  modify_column_hide(ci) %>%
  add_p()  -> t2

t2
gt::gtsave(as_gt(t2), file = file.path("Rprojects","Opioid - descriptive","tables","t2.html"))

#Diagnosises ####
pci_vars <- c("alcohol_abuse", "anemia", "anxiety", "malignancy", "asthma", "cardiovascular", "chromosomal_anomalies",
              "condact_disorder", "congenital_malformations", "depression", "developmental_delay", "diabetes_melitus",
              "drug_abuse", "eating_disorders", "epilepsy", "GI", "joints", "menstrual", "nausa_vomit", "pain", 
              "psychotic", "sleep", "smoking", "weight_loss")

diagnosis_score %>%
  mutate(outcome = ifelse(outcome == "Misuser", "Sustained Users", "Non-Sustained Users")) %>%
  select(-c(id)) %>%
  tbl_summary(by = "outcome",
              
              type = list(pci ~ 'continuous2'),
              statistic = c(pci) ~ c("{median} ({p25}, {p75})",
                                     "{mean} ({sd})"),
              missing = "no") %>%
  modify_header(label = "**Condition**") %>%
  modify_caption("**Table 4. Pedictric Comirbidity Score**", text_interpret = "html") %>%
  add_overall() %>%
  add_difference(everything() ~ "smd") %>%
  modify_column_hide(ci) %>%
  add_p()  -> t4

t4
gt::gtsave(as_gt(t4), file = file.path("Rprojects","Opioid - descriptive","tables","t4.html"))


