Sys.setlocale("LC_ALL", "Hebrew")
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)
library(stringr)
library(labelled)
library(caret)
library(corrplot)
library(randomForest)
library(janitor)

source(file.path("funcs.R"), encoding = "utf8")
set.seed(45)
cat("\f")

# Cohort creation -------------------------------------------------------------

## important data files -------------------------------------------------------
load("op_data.RData")

re_date <- read_csv("FIRST_OPIOID_POP_PURCHASE_updated.csv",
                    show_col_types = FALSE) %>%
  clean_names()

op_meds_tbl <- read_csv("med_list_new.csv", show_col_types = FALSE)
membership_dates <- read_csv("cohort_entitlement_FIRST_OPIOID_POP_PURCHASE_updated_PatientID.csv",
                             show_col_types = FALSE) %>%
  left_join(re_date %>% select(patient_id, RE_date = reference_event_date_dispensed), 
            by = "patient_id") %>%
  mutate(days_from_start = difftime(RE_date, membership_start_date, units = "days"),
         days_from_end = difftime(RE_date, membership_end_date, units = "days")) %>%
  filter(days_from_start > 0,
         days_from_end < 0) %>%
  group_by(id = patient_id, RE_date) %>%
  summarise(member_start = min(membership_start_date),
            member_end = max(membership_end_date)) %>%
  ungroup() %>%
  mutate(days_from_start = difftime(RE_date, member_start, units = "days"),
         days_from_end = difftime(RE_date, member_end, units = "days"))

## import opioid drugs purchases -----------------------------------------------
purch_full <- drugs %>%
  filter(RE_ATC5 %in% op_meds_tbl$atc) %>%
  select(-c(birth_date,RE_age)) %>%
  mutate(RE_date = as.Date(RE_date)) %>%
  left_join(op_meds_tbl %>% select(med, generic, factor, dose),
            by = c("RE_med" = "med")) %>%
  group_by(id) %>%
  mutate(mme = (RE_quantity_disp * factor * dose),
         start_date = min(RE_date),
         time = as.numeric(floor(difftime(RE_date, start_date,unit = "days")/30))) %>%
  ungroup()

### Correct MME calculation ---------------------------------------------------
# If any MME NA look for conversion errors in med_list.csv
# If any MME == 0 look for conversion errors purch
purch_full %>%
  filter(or(is.na(mme),
            mme == 0)) %>%
  distinct(RE_med, .keep_all = TRUE) %>%
  select(RE_med, RE_subs, dose, factor, RE_quantity_disp, mme)

# check for data error: zero prescription zero purchase
zero_disp <- purch_full %>%
  filter(RE_quantity_disp == 0)

zero_pres <- drugs_pres %>%
  filter(patient_id %in% zero_disp$id,
         medications_generic_drug_name %in% zero_disp$RE_med) %>%
  left_join(zero_disp %>% select(id, RE_med, RE_date), by = c("patient_id" = "id")) %>%
  mutate(validity_start_date = as.Date(validity_start_date),
         validity_end_date = as.Date(validity_end_date),
         med_same = medications_generic_drug_name == RE_med,
         date_ok = and(RE_date >= validity_start_date,
                       RE_date <= validity_end_date),
         diff_days = difftime(RE_date, validity_start_date, units = "days")) %>%
  filter(med_same,
         date_ok) %>%
  group_by(id = patient_id, RE_med, RE_date) %>%
  slice_min(diff_days, n=1)

purch_full <- purch_full %>%
  left_join(zero_pres %>% select(id,RE_med,RE_date, RE_quantity_pres = prescribed_quantity), 
            by = c("id", "RE_med", "RE_date"))

# correct zero error by finding duplicates and taking the right value
purch_corrected <- purch_full %>%
  group_by(id, start_date, time, RE_med, RE_package_size) %>%
  summarise(pres_quant = sum(as.numeric(RE_quantity_pres), na.rm = TRUE),   # sum prescribed dose from same drug
            old_disp_quant = sum(RE_quantity_disp, na.rm = TRUE)) %>%       # sum purchased dose from same drug
  ungroup() %>%
  mutate(RE_date = make_date(year(start_date), month(start_date), 15) %m+% months(time),
         disp_quant = case_when(
           old_disp_quant > 0 ~ old_disp_quant, 
           old_disp_quant == 0 & pres_quant > 0 ~ pres_quant,
           old_disp_quant == 0 & pres_quant == 0 ~ RE_package_size)) %>%
  left_join(op_meds_tbl, by = c("RE_med" = "med")) %>% 
  mutate(mme = (disp_quant * factor * dose)) %>%
  ungroup()

purch_corrected %>%
  filter(or(is.na(mme),
            mme == 0)) %>%
  distinct(RE_med, .keep_all = TRUE) %>%
  select(RE_med, generic, dose, factor, mme)

purch_refine <- purch_corrected %>%
  group_by(id, start_date,time,generic) %>%
  summarise(mme = sum(mme),
            RE_date = start_date + months(time)) %>%
  ungroup()

### exclude years & drugs -----------------------------------------------------
exclusion_year <- 2012
exclusion_drugs <- c("Methadone","Pethidine", "Propoxyphene")

purch_refine %>%
  filter(!(generic %in% exclusion_drugs),
         year(RE_date) >= exclusion_year,
         year(start_date) >= exclusion_year) %>%
  pull(id) %>%
  unique() -> include_id

## Import & clean cohort data --------------------------------------------------
first_purch <- import_data("FIRST_OPIOID_POP_PURCHASE_updated.csv", "purch") %>%
  left_join(membership_dates %>% select(-RE_date), by = "id") %>%
  filter(id %in% include_id,              # exclude patients before 2012
         !(generic %in% exclusion_drugs), # exclude patients using excluded drugs
         days_from_start > 365,           # make sure 1 year of followup before
         days_from_end < -60,             # make sure two month followup after
         med_first_date >= "2003-01-01") %>%
  select("id", "birth_date", "RE_age", "gender", "SES",  "district", "sector", 
         "bmi", "RE_date", "generic", "generic_codacamol","mme", "RE_pres_sub_dist", 
         "RE_pres_dist", "member_end")

## add death dates ------------------------------------------------------------
death_dates <- read_csv("DeathDates.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  select(id = patient_id,death_date = death_deceased_date)

first_purch <- first_purch %>%
  left_join(death_dates, by = "id")

## Create outcome data ---------------------------------------------------------
purch <- purch_refine %>%
  left_join(membership_dates %>% select(id,member_start,member_end), by = "id") %>%
  filter(id %in% first_purch$id,
         RE_date >= member_start,
         RE_date <= member_end,
         RE_date <= as.Date("2022-12-31"))

outcome_data <- purch %>%
  create_outcome()

## create final df -------------------------------------------------------------
semi_final_cohort <- left_join(first_purch, outcome_data, 
                               by = "id", suffix = c("_coh", "_outcome"))  %>%
  mutate(age_group = factor(case_when(
    RE_age < 6 ~ "Under 6",
    RE_age >= 6 & RE_age < 13 ~ "6-12",
    RE_age >= 13 ~ "13-19"
  ),
  levels = c("Under 6","6-12","13-19")),
  .after = RE_age) %>%
  mutate(generic = factor(generic),
         outcome = factor(ifelse(is.na(outcome), "Proper User", outcome)),
         periphery = case_when(
           district %in% c("Center", "Dan PT", "Tel-Aviv Jaffa") ~ "Center",
           TRUE ~ "Periphery"
         )) %>%
  rowwise() %>%
  mutate(date_19 = as.numeric(floor(difftime(birth_date + years(19), RE_date,unit = "days")/30)),
         member_end_time = as.numeric(floor(difftime(member_end, RE_date,unit = "days")/30)),
         death_time = as.numeric(floor(difftime(death_date, RE_date,unit = "days")/30)),
         end_of_study = as.numeric(floor(difftime(as.Date("2022-12-31"), RE_date,unit = "days")/30)),
         time = min(outcome_date, date_19,   # correct the outcome and followup time
                    member_end_time, death_time,
                    end_of_study, na.rm = TRUE),
         end_reason = factor(case_when(
           time == date_19 ~ "19",
           time == member_end_time ~ "Left CHS",
           time == death_time ~ "Death",
           time == end_of_study ~ "End of study",
           time == outcome_date ~ "Outcome"
         ))) %>%
  ungroup() %>%
  select(-c(date_19, member_end_time, end_of_study))

semi_final_cohort <- semi_final_cohort %>% #remove all with less than 2 months followup
  filter(time >= 2)

semi_final_cohort %>%
  pull(id) -> cohort_id

# Visits data -----------------------------------------------------------------
#add doctor visits data
visits_prof <- visits %>%
  left_join(semi_final_cohort %>%
              select(id, start_date = RE_date, district), by = "id") %>%
  filter(id %in% cohort_id,
         RE_date >= start_date - 365,
         RE_date <= start_date) %>%
  group_by(id, RE_profession) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = id, names_from = RE_profession, values_from = n, values_fill = 0, names_prefix = "visit_") %>% 
  clean_names()

visits_sum <- visits %>%
  left_join(semi_final_cohort %>%
              select(id, start_date = RE_date, district), by = "id") %>%
  filter(id %in% cohort_id,
         RE_date >= start_date - 365,
         RE_date <= start_date) %>%
  group_by(id) %>%
  summarise(n_visits = n(),
            follow_up = as.numeric(difftime(max(RE_date), min(RE_date), units = "weeks")),
            diff_profession = length(unique(RE_profession)),
            proffesion_primary = sum(RE_profession_type == "1") / n_visits,
            proffesion_proffesional = sum(RE_profession_type == "2") / n_visits,
            primary_prof = ifelse(proffesion_proffesional / proffesion_primary < 1/10, TRUE, FALSE))

# add visits to cohort 
semi_visit_cohort <- semi_final_cohort %>%
  left_join(visits_sum, by = "id", suffix = c("_coh", "_visit")) %>%
  left_join(visits_prof, by = "id")

# diagnosis data --------------------------------------------------------------
# analysis diagnosis according to PCI
pci_vars <- c("alcohol_abuse", "anemia", "anxiety", "malignancy", "asthma", "cardiovascular", "chromosomal_anomalies",
              "condact_disorder", "congenital_malformations", "depression", "developmental_delay", "diabetes_melitus",
              "drug_abuse", "eating_disorders", "epilepsy", "GI", "joints", "menstrual", "nausa_vomit", "pain", 
              "psychotic", "sleep", "smoking", "weight_loss")

diagnosis <- diagnosis_raw %>%
  left_join(semi_visit_cohort %>%
              select(id, start_date = RE_date), by = "id") %>%
  mutate(alcohol_abuse = grepl(c("Z71.41|F10|K70"), ICD10),
         anemia = grepl(c("D50|D51|D52|D53|D55|D56|D57"), ICD10),
         anxiety = grepl(c("F40|F41|F06.4|F43.0|F43.22|F43.8|F43.9"), ICD10),
         malignancy = grepl(c("C[0-1][0-9]|C2[0-6]|C3[0-9]|C4[0-1]C4[3,5-9]|C5[0-8]|C6[0-9]|C7[0-6]|C8[1-9]|C9[0-3]|C94\\.[0-3,8]|C95|C96\\.[0,2,4,9,A,Z]|D45"), ICD10),
         asthma = grepl(c("J45"), ICD10),
         cardiovascular = grepl(c("I0[0-2,5-9]|I1[0-6]|I2[0-8]|I[3-4][0-9]|I5[0-2]|I[6-8][0-9]|I9[5-9]"), ICD10),
         chromosomal_anomalies = grepl(c("Q9[0-3,5-9]"), ICD10),
         condact_disorder = grepl(c("F91"), ICD10),
         congenital_malformations = grepl(c("Q0[0-7]|Q1[0-8]|Q2[0-8]|Q3[0-9]|Q4[0-5]|Q5[0-6]|Q[6-8][0-9]"), ICD10),
         depression = grepl(c("F3[2-3]|F06\\.3[0-2]|F34\\.9|F39|F43\\.2[1,3]"), ICD10),
         developmental_delay = grepl(c("R48\\.0|H93\\.25|F[7-8][0-2,8,9]"), ICD10),
         diabetes_melitus = grepl(c("E0[8-9]|E1[0,1,3]"), ICD10),
         drug_abuse = grepl(c("F1[1-6,8,9]|F55|Z71\\.51"), ICD10),
         eating_disorders = grepl(c("F50"), ICD10),
         epilepsy = grepl(c("G40|R56"), ICD10),
         GI = grepl(c("K2[0-3,5-9]|K3[0,1]|K5[0-2,8]|Z87\\.1|K[6,9]2"), ICD10),
         joints = grepl(c("M2[1,4]"), ICD10),
         menstrual = grepl(c("M9[1,2]"), ICD10),
         nausa_vomit = grepl(c("R11"), ICD10),
         pain = grepl(c("G89|R52|R10|M54|R07|M25\\.5|F45\\.4"), ICD10),
         psychotic = grepl(c("F3[0,1]|F06\\.33|F2[0,2-5,8,9]|R44"), ICD10),
         sleep = grepl(c("F51|G47\\.[1-6,8,9]"), ICD10),
         smoking = grepl(c("F17|T65\\.2|Z87\\.891"), ICD10),
         weight_loss = grepl(c("E4[0-6]|E64\\.0|R63\\.4|R64"), ICD10),
         ADHD = grepl(c("F90"), ICD10),
         headache = grepl(c("G4[3,4]"), ICD10),
         injuries = grepl(c("S[0-9][0-9]"), ICD10),
         obesity = grepl(c("E6[5,6]|Z68\\.2[5-9]|Z68\\.[3,4]|Z68\\.5[3,4]"), ICD10),
         OCD = grepl(c("F42"), ICD10),
         personality_disorder = grepl(c("F60"), ICD10),
         pervasive_developmental_disorder = grepl(c("F84"), ICD10),
         rheumatologic_disorder = grepl(c("M08|M32|M4[5-9]"), ICD10)) 

diagnosis_score <- diagnosis %>%  
  filter(id %in% cohort_id,
         RE_date >= start_date - 365,
         RE_date <= start_date) %>%
  group_by(id) %>%
  summarise(n_diagnosis = n(),
            alcohol_abuse = ifelse(any(alcohol_abuse),1,0),
            anemia = ifelse(any(anemia),2,0),
            anxiety = ifelse(any(anxiety),1,0),
            malignancy = ifelse(any(malignancy),5,0),
            asthma = ifelse(any(asthma),1,0),
            cardiovascular = ifelse(any(cardiovascular),2,0),
            chromosomal_anomalies = ifelse(any(chromosomal_anomalies),2,0),
            condact_disorder = ifelse(any(condact_disorder),1,0),
            congenital_malformations = ifelse(any(congenital_malformations),2,0),
            depression = ifelse(any(depression),4,0),
            developmental_delay = ifelse(any(developmental_delay),1,0),
            diabetes_melitus = ifelse(any(diabetes_melitus),4,0),
            drug_abuse = ifelse(any(drug_abuse),3,0),
            eating_disorders = ifelse(any(eating_disorders),1,0),
            epilepsy = ifelse(any(epilepsy),4,0),
            GI = ifelse(any(GI),1,0),
            joints = ifelse(any(joints),1,0),
            menstrual = ifelse(any(menstrual),2,0),
            nausa_vomit = ifelse(any(nausa_vomit),1,0),
            pain = ifelse(any(pain),1,0),
            psychotic = ifelse(any(psychotic),3,0),
            sleep = ifelse(any(sleep),1,0),
            smoking = ifelse(any(smoking),2,0),
            weight_loss = ifelse(any(weight_loss),2,0))

any_cancer <- diagnosis %>%
  left_join(membership_dates %>% select(id,member_start,member_end), by = "id") %>%
  filter(id %in% first_purch$id,
         RE_date >= member_start,
         RE_date <= member_end,
         RE_date <= as.Date("2022-12-31")) %>%
  group_by(id) %>%
  summarise(cancer = any(malignancy,na.rm = TRUE)) %>%
  select(id, cancer)

# add diagnosis to cohort
semi_visit_diag_cohort <- semi_visit_cohort %>%
  left_join(diagnosis_score, by = "id") %>%
  rowwise() %>%
  mutate(pci = sum(alcohol_abuse, anemia, anxiety, malignancy, asthma, cardiovascular, chromosomal_anomalies,
                   condact_disorder, congenital_malformations, depression, developmental_delay, diabetes_melitus,
                   drug_abuse, eating_disorders, epilepsy, GI, joints, menstrual, nausa_vomit, pain, psychotic,
                   sleep, smoking, weight_loss)) %>%
  ungroup() %>%
  mutate_at(pci_vars, function(x) {ifelse(x == 0, FALSE, TRUE)}) %>%
  rowwise() %>%
  mutate(psychiatric_not_pci = any(anxiety, depression, eating_disorders, psychotic, sleep)) %>%
  ungroup()

# Drugs data ------------------------------------------------------------------
other_drugs <- c("אורולוגיה", "אורטופדיה", "הרדמה/הנשמה/טיפול נמרץ", "חבישה כללית",
                 "חדר ניתוח", "כירורגיה", "כללי", "לא הוגדר", "נשים ומיילדות", "סכרת",
                 "עיניים", "פיזיוטרפיה", "ציוד רפואי כללי - מתכלה", "ציוד רפואי מתכלה לבדיקות למעבדה",
                 "קוד חומר אינו צרפ", "ריאות", "תמיסות", "Various", "Items not in the ATC Classification")

not_other_drugs <- c("Alimentary Tract & Metabolism", "General Antiinfectives for Systemic Use",
                     "Nervous System", "Blood and Blood Forming Organs", "Dermatologicals",
                     "Musculo-Skeletal System", "Respiratory System", "Sensory Organs",
                     "Antiparasitic Products, Insecticides and Repellants", "Cardiovascular System",
                     "Genitourinary System & Sex Hormones", "Systemic Hormonal Preparations, Excluding Sex Hormones",
                     "Antineoplastic and Immunomodulating Agents")

drugs_sum <- drugs %>%
  left_join(semi_visit_diag_cohort %>%
              select(id, start_date = RE_date), by = "id") %>%
  filter(id %in% cohort_id,
         RE_date >= start_date - 365, 
         RE_date <= start_date) %>%
  select(id, RE_anatomy) %>%
  mutate(RE_anatomy = factor(case_when(
    !(RE_anatomy %in% not_other_drugs) ~ "Other", 
    TRUE ~ as.character(RE_anatomy)))) %>%
  group_by(id, RE_anatomy) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = id, names_from = RE_anatomy, values_from = n, values_fill = 0) %>% 
  rename(
    drug_metabolism = "Alimentary Tract & Metabolism",
    drug_antiinfective = "General Antiinfectives for Systemic Use",
    drug_nervous = "Nervous System",
    drug_blood = "Blood and Blood Forming Organs",
    drug_derma = "Dermatologicals",
    drug_muscle = "Musculo-Skeletal System",
    drug_other = "Other",
    drug_respiratory = "Respiratory System",
    drug_sensory = "Sensory Organs",
    drug_antiparasitic = "Antiparasitic Products, Insecticides and Repellants",
    drug_CV = "Cardiovascular System",
    drug_GU = "Genitourinary System & Sex Hormones",
    drug_hormones = "Systemic Hormonal Preparations, Excluding Sex Hormones",
    drug_antineoplastic = "Antineoplastic and Immunomodulating Agents"
  ) %>%
  rowwise() %>%
  mutate(drug_sum = sum(across(starts_with("drug_")), na.rm = T)) %>%
  ungroup()

final_cohort <- semi_visit_diag_cohort %>%
  left_join(drugs_sum, by = "id")

# Correct missing data --------------------------------------------------------
# assume all misisng data in those vars = 0 / FALSE
visits_var <- colnames(visits_prof)[2:ncol(visits_prof)]
diagnosis_var <- colnames(diagnosis_score)[2:ncol(diagnosis_score)]
drugs_var <- colnames(drugs_sum)[2:ncol(drugs_sum)]

final_cohort <- final_cohort %>%
  mutate_at(all_of(c("n_visits", "follow_up", "diff_profession",
                     "proffesion_primary", "proffesion_proffesional",
                     "n_diagnosis", "pci", drugs_var, visits_var)), 
            function(x) {ifelse(is.na(x), 0, x)}) %>%
  mutate_at(all_of(c("primary_prof","psychiatric_not_pci",pci_vars)), 
            function(x) {ifelse(is.na(x), FALSE, x)}) 

# LABELLS ---------------------------------------------------------------------
var_label(final_cohort) <- list(
  birth_date = "Birth Date",
  gender = "Sex",
  RE_age = "Age at first purchase",
  age_group = "Age at first purchase",
  bmi = "BMI",
  RE_date = "Date of first purchase",
  SES = "Socioeconomic status", 
  district = "Residency district",
  periphery = "Residency",
  sector = "Sector",
  generic = "Drug at first purchase",
  n_purch = "Number of Purchases",
  n_drugs = "Different drugs",
  follow_up_coh = "Time under opioids (D)",
  med_purch_int = "Time between purchases (D)",
  med_months_from_first = "Median months from first purchase",
  total_mme = "Total MME",
  dist_num_coh = "Different purchase districts",
  sub_dist_num_coh = "Different purchase sub-districts",
  quan = "Total pills dispensed",
  mme_per_day = "MME per day",
  mme_per_pres = "MME per purchase",
  time_to_outcome = "Time to diagnosis",
  outcome = "Outcome",
  end_date = "LOF date",
  time = "Time of follow-up (M)",
  end_reason = "Censore Reason",
  n_visits = "Doctor visits (num)",
  follow_up_visit = "Doctor visits Follow up time (W)",
  clinic_num = "Different clinics (num)",
  clinic_type_num = "Different clinic types",
  clinic_sector_arab_num = "Percent of visits at Arab-sector clinics",
  clinic_sector_general_num = "Fraction of visits at General-sector clinics",
  clinic_sector_other_num = "Percent of visits at Other-sector clinics",
  clinic_sector_ortho_num = "Percent of visits at Ultraorthodox-sector clinics",
  dist_num_visit = "Different clinic districts",
  sub_dist_num_visit = "Different clinic sub-districts",
  diff_dist_num =  "Different districts visited",
  diff_profession =  "Different professions visited",
  proffesion_primary = "Fraction of primary doctor visits",
  proffesion_proffesional =  "Fraction of professional doctor visits", 
  primary_prof = "Primary / Professional doctor visits ratio",
  n_diagnosis= "Number of diagnosises",
  alcohol_abuse = "Alcohol Abuse",
  anemia = "Anemia", 
  anxiety = "Anxiety", 
  malignancy = "Any Malignancy", 
  asthma = "Asthma", 
  cardiovascular = "Cardiovascular conditions", 
  chromosomal_anomalies = "Chromosomal Anomalies",
  condact_disorder = "Condact Disorder", 
  congenital_malformations = "Congenital Malformations", 
  depression = "Depression", 
  developmental_delay = "Developmental Delay", 
  diabetes_melitus = "Diabetes Melitus",
  drug_abuse = "Drug Abuse", 
  eating_disorders = "Eating Disorders", 
  epilepsy = "Epilepsy", 
  GI = "GI tract conditions", 
  joints = "Joint diseases", 
  menstrual = "Menstrual related conditions", 
  nausa_vomit = "Nausa Vomiting", 
  pain = "Pain conditions", 
  psychotic = "Psychotic episodes",
  sleep = "Sleep disorders", 
  smoking = "Smoking", 
  weight_loss = "Weight Loss",
  pci = "PCI",
  psychiatric_not_pci = "Any Psychiatric Disorder",
  drug_metabolism = "Alimentary Tract & Metabolism",
  drug_antiinfective = "General Anti-infectives for Systemic Use",
  drug_nervous = "Nervous System",
  drug_blood = "Blood and Blood Forming Organs",
  drug_derma = "Dermatologicals",
  drug_muscle = "Musculo-Skeletal System",
  drug_other = "Other",
  drug_respiratory = "Respiratory System",
  drug_sensory = "Sensory Organs",
  drug_antiparasitic = "Antiparasitic Products, Insecticides and Repellants",
  drug_CV = "Cardiovascular System",
  drug_GU = "Genitourinary System & Sex Hormones",
  drug_hormones = "Systemic Hormonal Preparations, Excluding Sex Hormones",
  drug_antineoplastic = "Antineoplastic and Immunomodulating Agents",
  drug_sum = "Total drugs purchased"
)

# saving the data --------------------------------------------------------------
save(final_cohort, purch, visits_sum, diagnosis,  membership_dates, any_cancer,
     diagnosis_score, drugs_sum, purch_refine, purch_corrected, cohort_id,
     pci_vars, drugs_var, file = file.path("op_data_new.RData"))
