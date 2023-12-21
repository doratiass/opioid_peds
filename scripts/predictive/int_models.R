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
library(readr)
library(corrplot)

source("funcs.R", encoding = "utf8")
load("op_models_raw_data_new.RData")
set.seed(45)
cat("\f")

# additional data ####

## labs ####
hem <- read_csv("Rprojects/hematology.csv",show_col_types = FALSE) %>%
  janitor::clean_names()  %>%
  left_join(final_cohort %>% select(id, RE_date), by = c("patient_id" = "id")) %>%
  mutate(diff = difftime(RE_date, cohort_reference_event_result_date, units = "days")) %>%
  filter(diff >=0,
         diff <= 365,
         cohort_reference_event_result_vs_norma != "Abnormal",
         !(cohort_reference_event_lab_test %in% c("COMPLETE BLOOD COUNT", "RDW-CV", "RDW-SD",
                                                  "HCT/HGB RATIO","RETICUL. COUNT abs",
                                                  "RETICULOCYTES COUNT%","MONOCYTES%")))

chem <- read_csv("Rprojects/cohort_lab_results_Opioid_Aviv_Cohort_360.csv",
                    show_col_types = FALSE) %>%
  left_join(final_cohort %>% select(id, RE_date), by = c("patient_id" = "id")) %>%
  mutate(diff = difftime(RE_date, result_date, units = "days")) %>%
  filter(diff >=0,
         diff <= 365) %>%
  ungroup()

add_lab <- bind_rows(hem, chem)

lab_n <- add_lab %>%
  distinct(id, diff) %>%
  group_by(id) %>%
  summarise(lab_n = n())

lab_num <- add_lab %>%
  group_by(id, test_name) %>%
  filter(diff == min(diff)) %>%
  distinct(id, test_name, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = "id",
    names_from = "test_name",
    values_from = "result",
    names_prefix = "lab_num_"
  )

lab_txt <- add_lab %>%
  group_by(id, test_name) %>%
  filter(diff == min(diff)) %>%
  distinct(id, test_name, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = "id",
    names_from = "test_name",
    values_from = "code",
    names_prefix = "lab_txt_"
  )

## images ####

### clean daat ####
general_service <- c(
  "CT טומוגרפיה ממוחשב למעט בדיקות להן קוד מיוחד",
  "פעולת דימות, ללא חיוב",
  "US בדיקה על קולית, עם תיעוד תמונות",
  "תוספת עבור חומר ניגוד לבדיקת MRI",
  "MRI בהרדמה כללית, תעריף לבדיקה",
  "MRI תוספת לבדיקה עם גדוליניום",
  "תוספת עבור צילום נוסף, למעט ברך ובית החזה",
  "CT בהרדמה",
  "MRI בדיקה במערכת תהודה מגנטית למעט בדיקה להן קוד",
  "ביופסיה/פעולה כירורגית בהנחיית CT",
  "ביופסיה מלעורית של הכבד בעזרת מחט ללא הנחיה CT/MRI",
  "ביופסיה/פעולה כירורגית בהנחיית US",
  "ניקור מחט בהנחיית US כולל ציטולוגיה",
  "CT/CTA ארתרוגרפ/CT אורוגר ללא CTA בית החזה לנבדק"
)

add_img_all <- read_csv("Rprojects/cohort_imaging_Opioid_Aviv_Cohort_360.csv",
                        show_col_types = FALSE) %>%
  mutate(imaging_type_code = case_when(
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("MRA",service) ~ "MRA",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("MRI",service) ~ "MRI",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("MR אנטרוגרפיה",service) ~ "MRE",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("US",service) ~ "US",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("CTA",service) ~ "CTA",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("CT",service) ~ "CT",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("PET",service) ~ "PT",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("צילום",service) ~ "CR",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("דופלקס",service) ~ "US",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("מיפוי",service) ~ "NM",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & grepl("PAP",service) ~ "PAP",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "אורתרוציסטוגרפיה, לאחר מילוי רטרוגרדי" ~ "RF",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "הוצאת צנתר קבוע מוריד מרכזי, כגון פורטה-קאת, היקמן" ~ "XA",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "פורטה-קאת,החדרה תחת אנגיוגרפיה" ~ "XA",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "שטיפת היקמן או פורטה-קאת" ~ "XA",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "מבחן שדה ראיה ממוחשב" ~ "OP",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "פענוח/יעוץ לבדיקת דימות מבחוץ" ~ "OT",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "פעולת דימות, ללא חיוב" ~ "OT",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service =="העתקת צילומי רנטגן וסורק ממוחשב -כל סרט"  ~ "OT",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "שיקוף ושט,קיבה,תרסריון,מעי דק,כולל צילומים סדרתיים" ~ "RF",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "תפקוד הבליעה,כולל הסרטה, סינה/וידאורדיוגרפיה" ~ "RF",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "ארתרוגרפיה של הכתף, כולל הזרקה ופענוח"  ~ "XA",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "מיאלוגרפיה"  ~ "XA",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "צנתור אבחנתי, למעט צנתור לב"  ~ "XA",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "אקוקרדיוגרפיה חיצונית בילדים"  ~ "USC",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "זרימת דם בכלי דם היקפיים, כגון דופלר"  ~ "US",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "אקוקרדיוגרפיה חיצונית, TTE, מבוגר"  ~ "USC",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service == "צלום רנטגן"  ~ "CR",
    (imaging_type_code == "לא ידוע" | imaging_type_code == "0") & service %in% c("בדיקה היסטולוגית כולל מיקרוסקופיה ופענוח", "טפול בכאב באמצעות גלי רדיו, לטפול", "לא ידוע") ~ "other",
    TRUE ~ imaging_type_code
  ),
  body_part = case_when(
    (body_part == "NO DATA") & (service == "MR ארתרוגרפיה") ~ "EXTREMITY UNK",
    (body_part == "NO DATA") & (service == "MRI בטן, לנבדק ליום") ~ "ABDOMEN PELVIS",
    (body_part == "NO DATA") & (service == "MRI מפרקי הגף התחתון, לנבדק ליום")  ~ "EXTREMITY LOWER",
    (body_part == "NO DATA") & (service == "USאגן, לא מיילדותי, עם תיעוד תמונות, מלא")  ~ "PELVIS",
    (body_part == "NO DATA") & (service == "דופלקס כלי דם בבטן, אגן ורטרופריטונאום")  ~ "ABDOMEN PELVIS",
    (body_part == "NO DATA") & (service == "צילום אגן, עד שני מבטים")  ~ "PELVIS",
    (body_part == "NO DATA") & (service == "צילום אמה, עד שני מבטים")  ~ "EXTREMITY UPPER",
    (body_part == "NO DATA") & (service == "צילום בית החזה, עד שני מבטים")  ~ "CHEST",
    (body_part == "NO DATA") & (service == "צילום ברך, עד שני מבטים, לצד")  ~ "EXTREMITY LOWER",
    (body_part == "NO DATA") & (service == "צילום זרוע, עד שני מבטים")  ~ "EXTREMITY UPPER",
    (body_part == "NO DATA") & (service == "צילום ירך, עד שני מבטים")  ~ "EXTREMITY LOWER",
    (body_part == "NO DATA") & (service == "צילום מסטואיד, עד שני מבטים")  ~ "SPINE",
    (body_part == "NO DATA") & (service == "צילום ע\"ש מעבר צוארי-גבי,תנוחת שחיין,עד שני מבטים")  ~ "SPINE CERVICAL",
    (body_part == "NO DATA") & (service == "צילום עמוד שדרה גבי-מותני,עד שני מבטים")  ~ "SPINE",
    (body_part == "NO DATA") & (service == "צילום עמוד שדרה גבי, עד שני מבטים")  ~ "SPINE THORACIC",
    (body_part == "NO DATA") & (service == "צילום עמוד שדרה מותני-סקרלי, עד שני מבטים")  ~ "SPINE LUMBAR SACRAL",
    (body_part == "NO DATA") & (service == "צילום עמוד שדרה צווארי, עד שני מבטים")  ~ "SPINE CERVICAL",
    (body_part == "NO DATA") & (service == "צילום רקמות רכות בצוואר, כולל קנה נשימה")  ~ "HEAD",
    (body_part == "NO DATA") & (service == "צילום שוק, עד שני מבטים")  ~ "EXTREMITY LOWER",
    (body_part == "NO DATA") & (service == "US רקמות רכות בראש וצואר,עם תיעוד תמונות")  ~ "NECK",
    (body_part == "NO DATA") & (service == "דופלקס של עורקי הגפיים או שתלים עוקפים")  ~ "EXTREMITY UNK",
    (body_part == "NO DATA") & (service == "מיפוי עצם ו \\או מפרק, כל גופי, לאחר 24 שעות")  ~ "GENERAL",
    (body_part == "NO DATA") & (service %in% general_service)  ~ "OTHER",
    TRUE ~ body_part
  ))

### summarize data ####
new_body_part <- add_img_all %>%
  filter(body_part != "NO DATA") %>%
  filter(!(service %in% general_service)) %>%
  distinct(body_part_new = body_part, service)

add_img <- left_join(add_img_all,new_body_part, by = "service") %>%
  left_join(final_cohort %>% select(id, RE_date), by = c("patient_id" = "id")) %>%
  mutate(diff = difftime(RE_date, start_date, units = "days")) %>%
  filter(diff >=0,
         diff <= 365) %>%
  transmute(
    id = patient_id,
    imaging_type = factor(imaging_type_code),
    body_part = factor(ifelse(body_part == "NO DATA", body_part_new,body_part)))

img_n <- add_img %>%
  group_by(id) %>%
  summarise(img_n = n())

img_type_num <- add_img %>%
  group_by(id, imaging_type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = id, 
              names_from = imaging_type, 
              values_from = n, 
              values_fill = 0,
              names_prefix = "img_type_") %>%
  janitor::clean_names()

img_body_num <- add_img %>%
  group_by(id, body_part) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = id, 
              names_from = body_part, 
              values_from = n, 
              values_fill = 0,
              names_prefix = "img_body_") %>%
  janitor::clean_names()

add_bmi <- read_csv("Rprojects/cohort_measurements_bmi_w_h_Opioid_Aviv_Cohort_360.csv",
                    show_col_types = FALSE) %>%
  left_join(final_cohort %>% select(id, RE_date), by = c("patient_id" = "id")) %>%
  mutate(diff = difftime(RE_date, measurement_date, units = "days"),
         bmi = as.numeric(bmi)) %>%
  filter(diff >=0,
         diff <= 365,
         !is.na(bmi)) %>%
  group_by(id = patient_id) %>%
  filter(diff == min(diff)) %>%
  distinct(id, bmi)

# Model Data ####
gen_collapse <- c("Buprenorphine", "Morphine", "Fentanyl")

## 1 year FO ####
model_data_1 <- final_cohort %>%
  mutate(time = case_when(
    outcome == "Misuser" & time <= 12 ~ 12,
    TRUE ~ time
  ),
  outcome = factor(case_when(
    outcome == "Misuser" & time > 12 ~ "Proper",
    outcome == "Proper User" ~ "Proper",
    TRUE ~ as.character(outcome)),
    levels = c("Misuser","Proper")
  )) %>%
  filter(time >= 12,
         year(RE_date) <= 2020,
         RE_age <= 18) %>%
  mutate(
    birth_year = year(birth_date),
    birth_month = factor(month(birth_date)),
    RE_year = year(RE_date),
    RE_month = factor(month(RE_date)),
    generic = factor(case_when(
      generic %in% gen_collapse ~ "Other",
      TRUE ~ as.character(generic)
    ))
  ) %>%
  rename(sum_drug = drug_sum) %>%
  left_join(lab_n, by = "id") %>%
  left_join(lab_txt, by = "id") %>% # lab_num lab_txt
  left_join(img_n, by = "id") %>%
  left_join(img_type_num, by = "id") %>%
  left_join(img_body_num, by = "id") %>%
  select(-c(birth_date, RE_date, generic_codacamol, RE_pres_sub_dist, #id,
            member_end, death_date, cont_date, cont, esc_date, proffesion_proffesional,
            esc_mme, outcome_date, time_to_outcome, periphery,
            death_time, time, end_reason, age_group)) %>%
  mutate_at(vars(starts_with("img_")), 
            function(x) {ifelse(is.na(x), 0, x)}) %>%
  mutate_at(vars(starts_with("lab_txt")), 
            function(x) {factor(ifelse(is.na(x), "untested", as.character(x)))}) %>%
  mutate(lab_n = ifelse(is.na(lab_n), 0, lab_n)) %>%
  mutate_if(is.logical, as.factor) %>%
  mutate(
    outcome = relevel(factor(outcome), ref = "Misuser"),
    generic = relevel(factor(generic), ref = "Codeine"),
    sector = relevel(factor(sector), ref = "General"),
    district = relevel(factor(district), ref = "Center")
  )


## 2 year FO ####
model_data_2 <- final_cohort %>%
  mutate(time = case_when(
    outcome == "Misuser" & time <= 24 ~ 24,
    TRUE ~ time
  ),
  outcome = factor(case_when(
    outcome == "Misuser" & time > 24 ~ "Proper",
    outcome == "Proper User" ~ "Proper",
    TRUE ~ as.character(outcome)),
    levels = c("Misuser","Proper")
  )) %>%
  filter(time >= 24,
         year(RE_date) <= 2019,
         RE_age <= 17) %>%
  mutate(
    birth_year = year(birth_date),
    birth_month = factor(month(birth_date)),
    RE_year = year(RE_date),
    RE_month = factor(month(RE_date)),
    generic = factor(case_when(
      generic %in% gen_collapse ~ "Other",
      TRUE ~ as.character(generic)
    ))
  ) %>%
  rename(sum_drug = drug_sum) %>%
  left_join(lab_n, by = "id") %>%
  left_join(lab_txt, by = "id") %>% # lab_num lab_txt
  left_join(img_n, by = "id") %>%
  left_join(img_type_num, by = "id") %>%
  left_join(img_body_num, by = "id") %>%
  select(-c(birth_date, RE_date, generic_codacamol, RE_pres_sub_dist, #id,
            member_end, death_date, cont_date, cont, esc_date, proffesion_proffesional,
            esc_mme, outcome_date, time_to_outcome, periphery,
            death_time, time, end_reason, age_group)) %>%
  mutate_at(vars(starts_with("img_")), 
            function(x) {ifelse(is.na(x), 0, x)}) %>%
  mutate_at(vars(starts_with("lab_txt")), 
            function(x) {factor(ifelse(is.na(x), "untested", as.character(x)))}) %>%
  mutate(lab_n = ifelse(is.na(lab_n), 0, lab_n)) %>%
  mutate_if(is.logical, as.factor) %>%
  mutate(
    outcome = relevel(factor(outcome), ref = "Misuser"),
    generic = relevel(factor(generic), ref = "Codeine"),
    sector = relevel(factor(sector), ref = "General"),
    district = relevel(factor(district), ref = "Center")
  )

## 3 year FO ####
model_data_3 <- final_cohort %>%
  mutate(time = case_when(
    outcome == "Misuser" & time <= 36 ~ 36,
    TRUE ~ time
  ),
  outcome = factor(case_when(
    outcome == "Misuser" & time > 36 ~ "Proper",
    outcome == "Proper User" ~ "Proper",
    TRUE ~ as.character(outcome)),
    levels = c("Misuser","Proper")
  )) %>%
  filter(time >= 36,
         year(RE_date) <= 2018,
         RE_age <= 16) %>%
  mutate(
    birth_year = year(birth_date),
    birth_month = factor(month(birth_date)),
    RE_year = year(RE_date),
    RE_month = factor(month(RE_date)),
    generic = factor(case_when(
      generic %in% gen_collapse ~ "Other",
      TRUE ~ as.character(generic)
    ))
  ) %>%
  rename(sum_drug = drug_sum) %>%
  left_join(lab_n, by = "id") %>%
  left_join(lab_txt, by = "id") %>% # lab_num lab_txt
  left_join(img_n, by = "id") %>%
  left_join(img_type_num, by = "id") %>%
  left_join(img_body_num, by = "id") %>%
  select(-c(birth_date, RE_date, generic_codacamol, RE_pres_sub_dist, #id,
            member_end, death_date, cont_date, cont, esc_date, proffesion_proffesional,
            esc_mme, outcome_date, time_to_outcome, periphery,
            death_time, time, end_reason, age_group)) %>%
  mutate_at(vars(starts_with("img_")), 
            function(x) {ifelse(is.na(x), 0, x)}) %>%
  mutate_at(vars(starts_with("lab_txt")), 
            function(x) {factor(ifelse(is.na(x), "untested", as.character(x)))}) %>%
  mutate(lab_n = ifelse(is.na(lab_n), 0, lab_n)) %>%
  mutate_if(is.logical, as.factor) %>%
  mutate(
    outcome = relevel(factor(outcome), ref = "Misuser"),
    generic = relevel(factor(generic), ref = "Codeine"),
    sector = relevel(factor(sector), ref = "General"),
    district = relevel(factor(district), ref = "Center")
  )

# save ####
save(model_data_1, model_data_2, model_data_3, file = file.path("Rprojects","op_models_data_3.RData"))
