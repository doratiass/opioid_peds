level_assertion <- function (df1, df2) {
  factors <- sapply(colnames(df1), function(x) is.factor(df1[,x]))
  lev_table <- tibble(
    features = names(factors[factors]),
    unclean = sapply(df2[,factors], function(x) length(levels(factor(x)))),
    clean = sapply(df1[,factors], function(x) length(levels(x))),
    equal = clean == unclean
  )
  return(lev_table)
}

na_table <- function(df1, df2) {
  table <- tibble(
    var = names(df1),
    unclean = sapply(df2, function(x) sum(is.na(x))),
    clean = sapply(df1, function(x) sum(is.na(x))),
    equal = clean == unclean
  )
  return(table)
}

ret_mme <- function(purch, val) {
  inx <- match(purch$RE_med, val$RE_med)
  generic <- val[inx, "RE_ATC"]
  factor <- val[inx, "factor"]
  dose <- val[inx, "dose"]
  cbind(purch, generic, factor, dose)
}

import_data <- function(file_path, type = c("pres", "purch", "chronic", "all_purch", 
                                            "visit", "drugs", "diagnosis"),
                        med_path = "med_list_new.csv") {
  message("\n")
  message(paste("Importing", file_path))
  df_unclean <- read_csv(file_path, show_col_types = FALSE) 
  warns <- 0
  warn_mes <- ""
  ## Identifications
  if (type == "purch" | type == "pres") {
    if (length(df_unclean$`Patient ID`) != length(unique(df_unclean$`Patient ID`))) {
      warns <- warns + 1
      dup <- df_unclean$`Patient ID`[duplicated(df_unclean$`Patient ID`)]
      dups <- paste(unique(dup), collapse = ",")
      stop("\n",paste(length(dup), "ID duplicates detected. \nIDs: ", dups))
    } else {message("No duplicate IDs found")}
    df_clean <- df_unclean %>%
      transmute(
        id = `Patient ID`,
        birth_date = as.Date(`Birth date`, tz = "UTC"),
        gender = factor(Gender))
  } else if (type == "chronic" | type == "all_purch") {
    df_clean <- df_unclean %>%
      transmute(
        ref = `Reference occurrence number`,
        id = `Patient ID`,
        birth_date = `Birth date`,
        gender = factor(Gender))
  } else if (type == "visit" | type == "diagnosis") {
    df_clean <- df_unclean %>%
      transmute(
        id = patient_id,
        encounter_id = encounter_id)
  } else if (type == "drugs") {
    df_clean <- df_unclean %>%
      transmute(
        id = patient_id,
        prescription_id = prescription_id)
  }
  if (!(type %in% c("visit", "diagnosis", "drugs"))) {
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          SES = factor(ifelse(`Socioeconomic score five level scale` == "no data", NA, 
                                              `Socioeconomic score five level scale`),
                                       levels = c("Very Low", "Low", "Medium", "High", "Very High"), 
                                       ordered = TRUE)))
  }
  ## Reference Event
  if (type == "purch" | type == "all_purch") {
    age_range <- which(df_unclean$`Reference Event-Age when dispensed` < 0 | 
                         df_unclean$`Reference Event-Age when dispensed` >= 19)
    if (length(age_range) > 0) {
      warns <- warns + 1
      age_lines <- paste(age_range, collapse = ",")
      mes_purch <- paste("\n",length(age_range),"observations out of age range \n Lines:", age_lines)
      mes_all <- paste("\n",length(age_range),"observations out of age range")
      mes <- ifelse(type == "purch", mes_purch, mes_all)
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of age range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          RE_age = `Reference Event-Age when dispensed`))
    date_range <- which(year(df_unclean$`Reference Event-Date dispensed`) < 2002 | 
                          df_unclean$`Reference Event-Date dispensed` < df_unclean$`Birth date`)
    if (length(date_range) > 0) {
      warns <- warns + 1
      date_lines <- paste(date_range, collapse = ",")
      mes_purch <- paste("\n",length(date_range),"observations out of date range \n Lines:", date_lines)
      mes_all <- paste("\n",length(date_range),"observations out of date range")
      mes <- ifelse(type == "purch", mes_purch, mes_all)
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of date range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          RE_date = as.Date(`Reference Event-Date dispensed`, tz = "UTC"),
                          RE_anatomy = factor(`Reference Event-Medication anatomical group`),
                          RE_med = `Reference Event-Medication name`,
                          RE_subs = factor(`Reference Event-Substance_ATC5`),
                          RE_ATC5 = `Reference Event-ATC5`,
                          RE_quantity_pres = `Reference Event-Prescribed quantity`,
                          RE_tpd = `Reference Event-Times per day`,
                          RE_member = `Reference Event-Membership`,
                          RE_pres_sub_dist = `Reference Event-Prescribing sub district`,
                          RE_pres_dist = factor(case_when(
                            `Reference Event-Prescribing district` == 'מחוז דן - פ\"ת' ~ "Dan PT",
                            `Reference Event-Prescribing district` == "מחוז חיפה וגליל מערבי" ~ "Haifa",
                            `Reference Event-Prescribing district` == "מחוז מרכז" ~ "Center",
                            `Reference Event-Prescribing district` == "מחוז שרון - שומרון" ~ "Sharon Shomron",
                            `Reference Event-Prescribing district` == "מחוז דרום" ~ "South",
                            `Reference Event-Prescribing district` == "אזור אילת" ~ "Eilat",
                            `Reference Event-Prescribing district` == "מחוז צפון" ~ "North",
                            `Reference Event-Prescribing district` == "מחוז ירושלים" ~ "Jerusalem",
                            `Reference Event-Prescribing district` == "מרכז רפואי מאיר" ~ "Meir medical center",
                            `Reference Event-Prescribing district` == "מרכז רפואי כרמל" ~ "Carmel medical center",
                            `Reference Event-Prescribing district` == "מרכז רפואי סורוקה" ~ "Soroka medical center",
                            `Reference Event-Prescribing district` == "מחוז תל אביב יפו" ~ "Tel-Aviv Jaffa",
                            `Reference Event-Prescribing district` == "מרכז רפואי רבין-קמפוס בילינסון" ~ "Rabin medical center"
                          )),
                          RE_quantity_disp = `Reference Event-Dispensed quantity`,
                          RE_quantity_ddd = `Reference Event-Dispensed quantity DDD`
                        ) %>%
                        mutate(RE_quantity_disp = ifelse(RE_quantity_disp == 0, RE_quantity_pres, RE_quantity_disp)))
  } else if (type == "chronic") {
    age_range <- which(df_unclean$`Chronic Medications-Age at event` < 0 | 
                         df_unclean$`Chronic Medications-Age at event` >= 19)
    if (length(age_range) > 0) {
      warns <- warns + 1
      age_lines <- paste(age_range, collapse = ",")
      mes <- paste("\n",length(age_range),"observations out of age range")
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of age range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          CR_age = `Chronic Medications-Age at event`,
                          CR_active = `Chronic Medications-Is chronic medication active`,
                          CR_med = `Chronic Medications-Medication`,
                          CR_ID = `Chronic Medications-Patient ID`))
    date_range <- which(year(df_unclean$`Chronic Medications-Start date`) < df_unclean$`Birth date`)
    if (length(date_range) > 0) {
      warns <- warns + 1
      date_lines <- paste(date_range, collapse = ",")
      mes <- paste("\n",length(date_range),"observations out of date range \n Lines:", date_lines)
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of date range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          CR_date = as.Date(`Chronic Medications-Start date`, tz = "UTC"),
                          CR_diagnosis = `Chronic Medications-Diagnosis`,
                          CR_diagnosis_original = `Chronic Medications-Diagnosis original description`))
  } else if (type == "pres") {
    age_range <- which(df_unclean$`Reference Event-Age at event` < 0 | 
                         df_unclean$`Reference Event-Age at event` >= 19)
    if (length(age_range) > 0) {
      warns <- warns + 1
      age_lines <- paste(age_range, collapse = ",")
      mes <- paste("\n",length(age_range),"observations out of age range \n Lines:", age_lines)
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of age range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          RE_age = `Reference Event-Age at event`,
                          RE_med = `Reference Event-Medication`))
    date_range <- which(year(df_unclean$`Reference Event-Prescribed Date`) < 2002 | 
                          df_unclean$`Reference Event-Prescribed Date` < df_unclean$`Birth date`)
    if (length(date_range) > 0) {
      warns <- warns + 1
      date_lines <- paste(date_range, collapse = ",")
      mes <- paste("\n",length(date_range),"observations out of date range \n Lines:", date_lines)
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of date range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          RE_date = as.Date(`Reference Event-Prescribed Date`, tz = "UTC")
                        ))
  } else if (type == "visit") {
    date_range <- which(year(df_unclean$visit_date) < 2002 | 
                          df_unclean$visit_date < df_unclean$visit_date)
    if (length(date_range) > 0) {
      warns <- warns + 1
      date_lines <- paste(date_range, collapse = ",")
      mes <- paste("\n",length(date_range),"observations out of date range")
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of date range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          RE_date = as.Date(visit_date, tz = "UTC")))
    age_range <- which(df_unclean$age_at_event < 0 | 
                         df_unclean$age_at_event >= 19)
    if (length(age_range) > 0) {
      warns <- warns + 1
      age_lines <- paste(age_range, collapse = ",")
      mes <- paste("\n",length(age_range),"observations out of age range")
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of age range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          RE_profession = factor(case_when(
                            visit_profession == "א.א.ג" ~ "ENG",
                            visit_profession == "אונקולוגיה" ~ "Oncology",
                            visit_profession == "אורולוגיה" ~ "Urology",
                            visit_profession == "אורטופדיה" ~ "Orthopedic",
                            visit_profession == "אלרגיה ואימונולוגיה" ~ "Allergy and Immunology",
                            visit_profession == "אנדוקרינולוגיה" ~ "Endocrinology",
                            visit_profession == "גאסטרואנטרולוגיה" ~ "Gasteroenterology",
                            visit_profession == "גריאטריה" ~ "Geriatric",
                            visit_profession == "המטולוגיה" ~ "Hematology",
                            visit_profession == "זיהומולוגיה" ~ "Infecious diseases",
                            visit_profession == "טראומטולוגיה" ~ "Traumatology",
                            visit_profession == "יילוד ונשים" ~ "Gynecology",
                            visit_profession == "ילדים" ~ "Pediatrics",
                            visit_profession == "ילדים מייעצת" ~ "Pediatrics - consultant",
                            visit_profession == "כירורגיה + טראומה" ~ "Trauma + surgery",
                            visit_profession == "כירורגיה כללית" ~ "General surgery",
                            visit_profession == "כירורגית בית חזה" ~ "Cardiothoracic surgery",
                            visit_profession == "כלי דם" ~ "Vascular surgery",
                            visit_profession == "מרפאת כאב" ~ "Pain clinic",
                            visit_profession == "מרפאת פוריות" ~ "Fertility clinic",
                            visit_profession == "נוירולוגיה" ~ "Neurology",
                            visit_profession == "נפרולוגיה" ~ "Nephrology",
                            visit_profession == "סכרת" ~ "Diabetis",
                            visit_profession == "עור" ~ "Dermatology",
                            visit_profession == "עיניים" ~ "Ophtalmology",
                            visit_profession == "פיזיקלית ושיקום" ~ "Rehabilitation",
                            visit_profession == "פלסטיקה" ~ "Plasic surgery",
                            visit_profession == "פנימית מיעצת" ~ "Internal medicine - consultant",
                            visit_profession == "פרוקטולוגיה" ~ "Proctology",
                            visit_profession == "קרדיולוגיה כולל ילדי" ~ "Cardiology incliudng pediatric",
                            visit_profession == "רופא ברפואה משלימה" ~ "Alternative medicine",
                            visit_profession == "רופא התפתחות" ~ "Child development",
                            visit_profession == "רופא כללי" ~ "General physician",
                            visit_profession == "ריאומטולוגיה" ~ "Rheumatologist",
                            visit_profession == "ריאות" ~ "Pulmunology",
                            visit_profession == "רפואת משפחה" ~ "Family medicine",
                            visit_profession == "רפואת שיניים" ~ "Dentist"
                          )),
                          RE_profession_type = factor(visit_profession_type
                            #case_when(
                            #`Reference Event-Profession Type` == "רפואה ראשונית" ~ "Primary care",
                            #`Reference Event-Profession Type` == "רפואה יועצת" ~ "Proffesional",
                            #`Reference Event-Profession Type` == "n(n" ~ "Nurse")
                          ),
                          RE_age = age_at_event, 
                          RE_district = factor(case_when(
                            district == 'דן - פ"ת' ~ "Dan PT",
                            district == "חיפה" ~ "Haifa",
                            district == "מרכז" ~ "Center",
                            district == "שרון-שומרון" ~ "Sharon Shomron",
                            district == "דרום" ~ "South",
                            district == "אילת" ~ "Eilat",
                            district == "צפון" ~ "North",
                            district == "ירושלים" ~ "Jerusalem",
                            district == "תל אביב-יפו" ~ "Tel-Aviv Jaffa"
                          )),
                          RE_visit_type = factor(encountercode_encounter),
                          RE_home_visit = factor(case_when(
                            encountercode_home_visit == "ביקור בית" ~ "Home Visit",
                            encountercode_home_visit == "ביקור במתקן רפואי" ~ "Medical Facility",
                            encountercode_home_visit == "לא ידוע" ~ "Unknown"
                          )),
                          RE_clinic = clinic,
                          RE_clinic_copy = factor(
                            case_when(
                              clinic_type == "מ.בריאות הילד" ~ "Child Health",
                              clinic_type == "כפרית" ~ "Country-side",
                              clinic_type == "יח. להמשך טיפול" ~ "Continuous treatment",
                              clinic_type == "בית אבות" ~ "Elderly home",
                              clinic_type == "עצמאית מקצועית" ~ "Private proffesional",
                              clinic_type == "מרפאה צבאית" ~ "Military",
                              clinic_type == "מרפאה פיקטיבית" ~ "Fake",
                              clinic_type == "מ.רופ.עצ.ראשוני" ~ "Primary doc private",
                              clinic_type == "מ.רופ.עצ.מקצועי" ~ "Proffesional doc private",
                              clinic_type == "ראשונית ומקצועי" ~ "Primary and Professional",
                              clinic_type == "ראשונית" ~ "Primary",
                              clinic_type == "פנימיה" ~ "Boarding school",
                              clinic_type == "עצמאית ראשונית" ~ "Private primary"
                            )
                          ),
                          RE_clinic_sub_district = factor(clinic_sub_district_name),
                          RE_clinic_district = factor(case_when(
                            clinic_district == 'דן - פ"ת' ~ "Dan PT",
                            clinic_district == "חיפה" ~ "Haifa",
                            clinic_district == "מרכז" ~ "Center",
                            clinic_district == "שרון-שומרון" ~ "Sharon Shomron",
                            clinic_district == "דרום" ~ "South",
                            clinic_district == "אילת" ~ "Eilat",
                            clinic_district == "צפון" ~ "North",
                            clinic_district == "ירושלים" ~ "Jerusalem",
                            clinic_district == "תל אביב-יפו" ~ "Tel-Aviv Jaffa",
                            clinic_district == "בטוח מושלם" ~ "Mushlam",
                            clinic_district == "ההנהלה הראשית" ~ "Managment"#,
                        #    clinic_district == "מחוז לא ידוע" ~ NA
                          )),
                          RE_clinic_sector = factor(case_when(
                            clinic_sector == "כללי - כללי" ~ "General",
                            clinic_sector == "כללי- דתי מעורב" ~ "Religious mixed",
                            clinic_sector == "כללי - צ'רקסים" ~ "Cherkess",
                            clinic_sector == "ערבים-בדואים" ~ "Bedouin",
                            clinic_sector == "ערבים-אחר" ~ "Arab - others"
                          )),
                          RE_clinic_sector_group = factor(case_when(
                            clinic_sector_group == "כללי" ~ "General",
                            clinic_sector_group == "מגזר החרדים" ~ "Ultraorthodox",
                            clinic_sector_group == "מגזר ערבי" ~ "Arab",
                            clinic_sector_group == "אחר" ~ "others"
                          )),
                          RE_sub_dis_unk = sub_district,
                          RE_sector_unk = sector
                        ))
  } else if (type == "drugs") {
    date_range <- which(year(df_unclean$date_dispensed) < 2002)
    if (length(date_range) > 0) {
      warns <- warns + 1
      date_lines <- paste(date_range, collapse = ",")
      mes <- paste("\n",length(date_range),"observations out of date range")
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of date range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          RE_date = as.Date(date_dispensed, tz = "UTC"),
                          RE_serving_form_amount = serving_form_amount,
                          RE_ATC5 = atc5_code,
                          RE_subs = factor(substance_atc5),
                          RE_chemical_subgroup_atc4 = chemical_subgroup_atc4,
                          RE_pharmacological_subgroup_atc3 = pharmacological_subgroup_atc3,
                          RE_therapeutic_main_group_atc2 = therapeutic_main_group_atc2,
                          RE_anatomy = factor(anatomical_main_group_atc1),
                          RE_quantity_disp = dispensed_quantity,
                          RE_tpd = times_per_day))
    age_range <- which(df_unclean$age_when_dispensed < 0 | 
                         df_unclean$age_when_dispensed >= 19)
    if (length(age_range) > 0) {
      warns <- warns + 1
      age_lines <- paste(age_range, collapse = ",")
      mes <- paste("\n",length(age_range),"observations out of age range")
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of age range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          RE_age = age_when_dispensed,
                          RE_quantity_pres = prescribed_quantity))
  } else if (type == "diagnosis") {
    date_range <- which(year(df_unclean$start_date) < 2002)
    if (length(date_range) > 0) {
      warns <- warns + 1
      date_lines <- paste(date_range, collapse = ",")
      mes <- paste("\n",length(date_range),"observations out of date range")
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of date range")}
    age_range <- which(df_unclean$age_at_diagnosis < 0 | 
                         df_unclean$age_at_diagnosis >= 19)
    if (length(age_range) > 0) {
      warns <- warns + 1
      age_lines <- paste(age_range, collapse = ",")
      mes <- paste("\n",length(age_range),"observations out of age range")
      warn_mes <- paste(warn_mes, mes, sep = "\n")
    } else {message("No observations out of age range")}
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          RE_date = as.Date(start_date, tz = "UTC"),
                          RE_date_end = end_date, #as.Date(end_date, tz = "UTC"),
                          RE_type_code = factor(diagnosis_type_code),
                          RE_type = factor(diagnosis_type_description),
                          RE_diagnosis_code = diagnosis_code,
                          RE_diagnosis_code_description = diagnosis_code_description,
                          RE_diagnosis_original_description = diagnosis_original_description,
                          RE_diagnoses_alternate_description = diagnoses_alternate_description,
                          RE_subcategory = diagnoses_subcategory,
                          RE_category = diagnoses_category,
                          RE_block = diagnoses_block,
                          RE_chapter = diagnoses_chapter,
                          ICD10 = diagnoses_icd10_code,
                          RE_snomed_code = snomed_code,
                          RE_snomed_description = snomed_description,
                          RE_code_origin = code_origin,
                          RE_code_origin_desc = code_origin_desc,
                          RE_code_origin_type = factor(code_origin_type),
                          RE_diagnosis_origin = diagnosis_origin,
                          RE_note = note,
                          RE_age = age_at_diagnosis))
    
  }
  ## Demography
  if (type == "purch" | type == "pres") {
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          demo_date = as.Date(`DEMO-Date`, tz = "UTC"),
                          demo_date_days_from = `DEMO-Date copy-Days from Reference`,
                          district = factor(case_when(
                            `DEMO-District` == 'דן - פ"ת' ~ "Dan PT",
                            `DEMO-District` == "חיפה" ~ "Haifa",
                            `DEMO-District` == "מרכז" ~ "Center",
                            `DEMO-District` == "שרון-שומרון" ~ "Sharon Shomron",
                            `DEMO-District` == "דרום" ~ "South",
                            `DEMO-District` == "אילת" ~ "Eilat",
                            `DEMO-District` == "צפון" ~ "North",
                            `DEMO-District` == "ירושלים" ~ "Jerusalem",
                            `DEMO-District` == "תל אביב-יפו" ~ "Tel-Aviv Jaffa"
                          )),
                          sector = factor(case_when(
                            `DEMO-Sector` == "כללי - כללי" ~ "General",
                            `DEMO-Sector` == "כללי- דתי מעורב" ~ "Religious mixed",
                            `DEMO-Sector` == "כללי - צ'רקסים" ~ "Cherkess",
                            `DEMO-Sector` == "ערבים-בדואים" ~ "Bedouin",
                            `DEMO-Sector` == "ערבים-אחר" ~ "Arab - others"
                          )),
                          residency = `DEMO-Residency`,
                          bmi = `BMI-BMI`,
                          bmi_date = `BMI-Measurement date`,
                          bmi_days = `BMI-Measurement date copy-Days from Reference`
                        ))
    bmi_range <- which(df_clean$bmi < 5 | df_clean$bmi > 50)
    if (length(bmi_range) > 0 ) {
      warns <- warns + 1
      bmi_lines <- paste(bmi_range, collapse = ",")
      mes <- paste("\n",length(bmi_range),"observations out of BMI range",
                    "\n values replaced with NA")
      warn_mes <- paste(warn_mes, mes, sep = "\n")
      df_clean$bmi[bmi_range] <- NA
    } else {message("No observations out of BMI range")} 
  }
  ## Prescriptions
  if (type == "purch" | type == "all_purch") {
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          pres_age = `med_prescribed-Age at event`,
                          pres_med = `med_prescribed-Medication`,
                          pres_subs = `med_prescribed-Substance_ATC5`,
                          pres_atc = `med_prescribed-ATC5`,
                          pres_date = as.Date(`med_prescribed-Prescribed Date`, tz = "UTC"),
                          pres_dist = factor(case_when(
                            `med_prescribed-district` == 'דן - פ"ת' ~ "Dan PT",
                            `med_prescribed-district` == "חיפה" ~ "Haifa",
                            `med_prescribed-district` == "מרכז" ~ "Center",
                            `med_prescribed-district` == "שרון-שומרון" ~ "Sharon Shomron",
                            `med_prescribed-district` == "דרום" ~ "South",
                            `med_prescribed-district` == "אילת" ~ "Eilat",
                            `med_prescribed-district` == "צפון" ~ "North",
                            `med_prescribed-district` == "ירושלים" ~ "Jerusalem",
                            `med_prescribed-district` == "תל אביב-יפו" ~ "Tel-Aviv Jaffa",
                            `med_prescribed-district` == "ההנהלה הראשית" ~ "Main managment"
                          )),
                          pres_sub_dist = `med_prescribed-sub district`))
  }
  # Diagnosis
  if (type == "purch") {
    df_clean <- cbind(df_clean, 
                      df_unclean %>%
                        transmute(
                          diagnosis_com = `Diagnosis_community-Diagnosis`,
                          diagnosis_com_icd = `Diagnosis_community-ICD10`,
                          diagnosis_com_type = factor(`Diagnosis_community-Diagnosis type copy`),
                          diagnosis_com_date = as.Date(`Diagnosis_community-Start date`, tz = "UTC"),
                          diagnosis_chron = `Diagnosis_chronic-Diagnosis`,
                          diagnosis_chron_icd = `Diagnosis_chronic-ICD10`,
                          diagnosis_chron_type = factor(`Diagnosis_chronic-Diagnosis type`),
                          diagnosis_chron_date = as.Date(`Diagnosis_chronic-Start date`, tz = "UTC"),
                          diagnosis_hosp_icd = `Diagnosis_hosp-ICD10`,
                          diagnosis_hosp = `Diagnosis_hosp-Diagnosis copy`,
                          diagnosis_hosp_type = factor(`Diagnosis_hosp-Diagnosis type`),
                          diagnosis_hosp_date = as.Date(`Diagnosis_hosp-Start date`, tz = "UTC"),
                          member_end_date = as.Date(`Membership-Membership End Date`, tz = "UTC"),
                          member_end_days = `Membership-Membership End Date-Days from Reference`,
                          member_start_days = `Membership-Membership Start Date-Days from Reference`,
                          member_start_date = as.Date(`Membership-Membership Start Date`, tz = "UTC"),
                          med_first_date = as.Date(`First_ever-Date dispensed`, tz = "UTC"),
                          first_med = `First_ever-Medication`,
                        ))
  }
  ## Quality assurance
  na_t <- na_table(df_clean, df_unclean)
  na_range <- which(!na_t$equal)
  if (sum(na_range) > 0) {
    warns <- warns + 1
    na_vars <- paste(names(na_range), collapse = ", ")
    mes <- paste("\n",length(na_range),"features with NA discrepancies \n Features:", na_vars)
    warn_mes <- paste(warn_mes, mes, sep = "\n")
    print(na_t[!na_t$equal,])
  } else {message("No NA discrepancies")}
  lev_t <- level_assertion(df_clean, df_unclean)
  levels_range <- which(!lev_t$equal)
  if (sum(levels_range) > 0) {
    warns <- warns + 1
    lev_vars <- paste(names(levels_range), collapse = ", ")
    mes <- paste("\n",length(levels_range),"features with factor levels discrepancies \n Features:", lev_vars)
    warn_mes <- paste(warn_mes, mes, sep = "\n")
    print(lev_t[!lev_t$equal,])
  } else {message("No factor levels discrepancies")}
  error_99 <- sum(df_clean$ref == 99)
  if (error_99 > 0) {
    warns <- warns + 1
    mes <- paste("\n There are",error_99,"observations with ERROR 99")
    warn_mes <- paste(warn_mes, mes, sep = "\n")
  }
  if (warns == 0) {
    message("Imported succesfully")
  } else {
    warning(warn_mes)
  }
  # add generic
  if (type == "purch" | type == "all_purch") {
    meds <- read_csv(med_path, show_col_types = FALSE)
    inx <- match(df_clean$RE_med, meds$med)
    df_clean <- df_clean %>% 
      mutate(generic = meds[inx, "generic", drop = TRUE],
             factor = meds[inx, "factor", drop = TRUE],
             dose = meds[inx, "dose", drop = TRUE],
             mme = (RE_quantity_disp * factor * dose))
    message("MME created")
  }
  # apply exclusion criteria
  if (length(date_range) > 0 | length(age_range) > 0) {
    sum_er <- length(date_range) + length(age_range)
    if (type == "chronic") {
      df_clean <-  df_clean %>%
        filter(CR_age < 19, CR_date >= '2002-01-01')
    } else {
      df_clean <-  df_clean %>%
        filter(RE_age < 19, RE_date >= '2002-01-01')
    }
    warning(paste("\n",sum_er,"observations removed"))
  }
  return(df_clean)
}

import_files <- function(list_files, type, path = NA) {
  for (data in list_files) {
    if (is.na(path)) {
      file_path <- data
    } else {
      file_path <- file.path(path, data)
    }
    if (!exists("dataset")){
      dataset <- import_data(file_path, type)
    } else if (exists("dataset")){
      tempory <-import_data(file_path, type)
      dataset <-rbind(dataset, tempory)
      rm(tempory)
    }
  }
  return(dataset)
}

calc_purch_diff <- function(date, n) {
  diff_vec <- c()
  date <- sort(date)
  for (i in 1:(n-1)) {
    if (!is.na(date[i]) & !is.na(date[i+1])) {
      diff_vec[i] <- as.numeric(difftime(date[i+1], date[i], units = "days"))
    } else {
      diff_vec[i] <- NA
    }
  }
  return(diff_vec)
}

create_sum_metrics <- function (purch, interval = 10) {
  purch %>% 
    select(id, RE_date, RE_med, generic, RE_quantity_disp, mme, months_from_first, 
           RE_pres_dist, RE_pres_sub_dist) %>%
    group_by(id) %>%
    summarise(n_purch = n(),
              n_drugs = length(unique(RE_med)),
              min = min(RE_date),
              max = max(RE_date),
              follow_up = (as.numeric(difftime(max, min, units = "days")) + 30),
              med_months_from_first = median(months_from_first),
              purch_interval = list(calc_purch_diff(RE_date, interval)),
              med_purch_int = median(unlist(purch_interval), na.rm = TRUE),
              dist_num = length(unique(RE_pres_dist)),
              sub_dist_num = length(unique(RE_pres_sub_dist)),
              quan = sum(RE_quantity_disp),
              total_mme = sum(mme),
              mme_per_day = sum(mme) / follow_up,
              mme_per_pres = sum(mme) / n_purch) -> df
  return(df)
}

ret_int <- function(x, i) {
  sapply(x, function(y) {y[[i]]})
}

create_purch_int_df <- function(df) {
  n <- length(df$purch_interval[[1]])
  for (i in 1:9) {
    new <- ret_int(df$purch_interval, i)
    df[, ncol(df) + 1] <- new
    colnames(df)[ncol(df)] <- paste0("int_", i)
  }
  return(df)
}

return_outcome_cont_date <- function(time) {
  time <- sort(unique(time))
  if (length(time) > 2) {
    for (i in 3:length(time)) {
      #if (as.numeric(difftime(date[i], date[i-2], units = "days")) < 183) {
      if (time[i] - time[i-2] <= 6) {
        return(time[i])
      }
    }
  }
  return(NA)
}

return_outcome_rep_date <- function(date) {
  date <- sort(unique(date))
  if (length(date) > 5) { 
    return(date[6])
  }
  return(NA)
}

return_outcome_esc_date <- function(time, dose) {
  df <- tibble(time = time, dose = dose) %>%
    group_by(time) %>%
    summarise(sum_dose = sum(dose)) %>%
    arrange(time)
  if (length(df$time) > 2 & (sum(is.na(df$sum_dose)) == 0)) {
    for (i in 2:(length(df$time)-1)) {
      if (df$sum_dose[i] >= (1.25 * df$sum_dose[i-1])) {
        for (j in i:(length(df$time)-1)) {
          if (df$sum_dose[j+1] >= (1.25 * df$sum_dose[i])) {
            return(df$time[j+1])
          }
        }
      }
    }
  }
  return(NA)
}

create_outcome <- function(purch, med_path = "med_list_new.csv") {
  ### Filter for duplicated and replace zeros
  df <- purch #%>%
  #  distinct(id, time, RE_med, .keep_all = TRUE) %>% #remove duplicate purchases
  #  group_by(id, start_date, time, RE_med, RE_package_size) %>%
  #  summarise(#RE_mean_day = mean(RE_day),
  #    pres_quant = sum(RE_quantity_pres, na.rm = TRUE),      # sum prescribed dose from same drug
  #    old_disp_quant = sum(RE_quantity_disp, na.rm = TRUE)) %>%  # sum puchased dose from same drug
  #  ungroup() %>%
  #  mutate(RE_date = make_date(year(start_date), month(start_date), 01) %m+% months(time),
   #        disp_quant = case_when(
    #         old_disp_quant > 0 ~ old_disp_quant, 
     #        old_disp_quant == 0 & pres_quant > 0 ~ pres_quant,
      #       old_disp_quant == 0 & pres_quant == 0 ~ RE_package_size)) #if quantity zero -> take prescribed quantitiy
  
  ### Calculate MME
#  meds <- read_csv(med_path, show_col_types = FALSE)
#  inx <- match(df$RE_med, meds$med)
 # df <- df %>% 
  #  mutate(generic = meds[inx, "generic", drop = TRUE],
   #        factor = meds[inx, "factor", drop = TRUE],
    #       dose = meds[inx, "dose", drop = TRUE],
     #      mme = (disp_quant * factor * dose))
  
  ### create outcome
  df %>%
    select(id, time, generic, mme) %>%
    group_by(id) %>%
    summarise(min = min(time),
              cont_date = return_outcome_cont_date(time),
              cont = ifelse(!(is.na(cont_date)), "Misuser", "Proper User"),
          #    rep_date = return_outcome_rep_date(RE_date),
           #   rep = ifelse(!(is.na(rep_date)), "Misuser", "Proper User"),
              esc_date = return_outcome_esc_date(time, mme),
              esc_mme = ifelse(!(is.na(esc_date)), "Misuser", "Proper User"),
              outcome_date = pmin(cont_date, esc_date, na.rm = TRUE), #, rep_date
              time_to_outcome = ifelse(is.na(outcome_date),
                                       NA,
                                       as.numeric(outcome_date - min)),
                                      #as.numeric(difftime(outcome_date, min, units = "days"))),
              outcome = ifelse(any(cont == "Misuser", 
                                 #  rep == "Misuser", 
                                   esc_mme == "Misuser"), "Misuser", "Proper User")) %>%
    select(-min) -> df
  
  df$outcome <- df$outcome %>% replace_na('Proper User')
  
  outp <- round(100 * (nrow(df[df$outcome == "Misuser",]) / nrow(df)), 1)
  contp <- round(100 * (nrow(df[df$cont == "Misuser",]) / nrow(df)), 1)
#  repp <-round(100 * (nrow(df[df$rep == "Misuser",]) / nrow(df)), 1)
  escp <-round(100 * (nrow(df[df$esc_mme == "Misuser",]) / nrow(df)), 1)
  message(paste("Calculation completed. \nThere are",
                outp, "% misuse patients: \n",
                contp, "% Continuous users \n",
             #   repp, "% Repetative users \n",
                escp, "% Escalting users"))
  return(df)
}

coef_table <- function(model) {
  coefs_tb <- coef(summary(model)) 
  
  CI <- data.frame(
    UL = as.numeric(coefs_tb[,1]) + 1.96*as.numeric(coefs_tb[,2]),
    LL = as.numeric(coefs_tb[,1]) - 1.96*as.numeric(coefs_tb[,2])
  ) 
  
  coefs_log <- tibble(
    Feature = factor(names(coefs_tb[,1])),
    `log(OR)` = round(coefs_tb[,1, drop = TRUE],2),
    UL = round(CI$UL,2),
    LL = round(CI$LL,2),
    p = round(coefs_tb[,4, drop = TRUE],2)
  )
  
  coefs <- tibble(
    Feature = factor(names(coefs_tb[,1])),
    OR = round(exp(coefs_tb[,1, drop = TRUE]),2),
    UL = round(exp(CI$UL),2),
    LL = round(exp(CI$LL),2),
    p = round(coefs_tb[,4, drop = TRUE],2)
  )
  
  ymax <- max(coefs$OR) + 2
  ymax_log <- max(coefs_log$`log(OR)`) + 2
  
  coefs_log <- coefs_log[2:nrow(coefs_log),] %>%
    mutate(UL = ifelse(UL > ymax_log, ymax_log, UL)) %>%
    arrange(desc(`log(OR)`))
  
  coefs <- coefs[2:nrow(coefs),] %>%
    mutate(UL = ifelse(UL > ymax, ymax, UL)) %>%
    arrange(desc(OR))
  
  print(coefs, n = nrow(coefs))
  
  g1 <- ggplot(coefs, aes(x = reorder(Feature, OR), OR)) + 
    geom_col() + 
    geom_errorbar(aes(ymin = LL, ymax = UL)) + 
    geom_hline(yintercept = 1, color = "navy") +
    coord_flip()
  
  g2 <- ggplot(coefs_log, aes(x = reorder(Feature, `log(OR)`), `log(OR)`)) + 
    geom_col() + 
    geom_errorbar(aes(ymin = LL, ymax = UL)) + 
    geom_hline(yintercept = 0, color = "navy") +
    coord_flip()
  
  print(g1)
  print(g2)
}

coef_table_2 <- function(model, lambda = "no") {
  if (lambda == "no") {
    coefs_tb <- plyr::name_rows(as.data.frame(as.matrix(coef(model)))) %>% 
      filter(s1 != 0)
  } else {
    coefs_tb <- plyr::name_rows(as.data.frame(as.matrix(coef(model, lambda)))) %>% 
      filter(s1 != 0)
  }
  
  coefs <- tibble(
    Feature = coefs_tb[,2],
    log_OR = round(coefs_tb[,1, drop = TRUE],2),
    OR = round(exp(coefs_tb[,1, drop = TRUE]),2)
  )
  
  coefs <- coefs[2:nrow(coefs),] %>%
    arrange(desc(OR))
  
  coefs %>%
    select(-log_OR) %>%
    print(n = nrow(coefs))
  
  g1 <- ggplot(coefs, aes(x = reorder(Feature, OR), OR)) + 
    geom_col() + 
    geom_hline(yintercept = 1, color = "navy") +
    coord_flip()
  
  g2 <- ggplot(coefs, aes(x = reorder(Feature, log_OR), log_OR)) + 
    geom_col() + 
    geom_hline(yintercept = 0, color = "navy") +
    coord_flip()
  
  print(g1)
  print(g2)
}

calibration_plot_2 <- function(prob, outcome, out = "Misuser", cap = "Calibration plot") {
  tibble(
    outcome = outcome, 
    predicted_prob = prob
  ) %>%
    mutate(
      quan = ntile(log_prob, 10)
    ) %>%
    group_by(quan) %>%
    summarise(
      n = n(),
      outcome = sum(outcome == out),
      pred_prob = mean(predicted_prob)
    ) %>%
    mutate(obs_prob = outcome / n) %>%
    ggplot(aes(pred_prob, obs_prob)) + 
    geom_point() + 
    geom_line() +
    geom_abline(color = "blue", linetype = 3) + 
    coord_cartesian(xlim =c(0, 1), ylim = c(0, 1)) + 
    ggtitle(cap) 
}

calibration_plot <- function(prob, outcome, out = "Misuser", cap = "") {
  if (cap != "") {
    capt <- paste(cap, "Calibration Plot")
  } else {
    capt <- "Calibration Plot"
  }
  tibble(
    prob = prob, 
    y = ifelse(outcome == out, 1, 0)
  ) %>%
    ggplot(aes(prob, y)) +
    geom_point(shape = 21, size = 2) +
    geom_abline(slope = 1, intercept = 0) +
    geom_smooth(method = stats::loess, se = TRUE) +
    scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    xlab("Estimated Prob.") +
    ylab("Data w/ Empirical Prob.") +
    ggtitle(capt) 
}


no_use <- function(df) {
  id_m <- df[,"id", drop = TRUE]
  ids <- unique(df[,"id", drop = TRUE])
  new_df <- df
  
  n_iter <- length(ids)
  
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")
  
  for(i in 1:n_iter) {
    id <- ids[i]
 #   dates <- as.Date(sort(df[df$id == id, "RE_date", drop = TRUE]))
    times <- as.numeric(sort(df[df$id == id, "time", drop = TRUE]))
    for(t in 1:length(times)) {
      if(t == length(times)) {
        new_t <- as.numeric(times[t] + 1)
  #      new_d <- dates[t] %m+% months(1)
   #     new_c <- ifelse(new_d >= as.Date("2020-03-25"),1,0)
   #     tib <- tibble(id = id, RE_date = new_d, time = new_t, covid = new_c, generic = "No Use")
        tib <- tibble(id = id, time = new_t, generic = "No Use")
        new_df <- rbind(new_df, tib)
      } else {
        dif <- times[(t+1)] - times[t]
        if(dif > 1) {
          new_t <- as.numeric(times[t] + 1)
         # new_d <- dates[t] %m+% months(1)
        #  new_c <- ifelse(new_d >= as.Date("2020-03-25"),1,0)
          tib <- tibble(id = id, time = new_t, generic = "No Use")
          new_df <- rbind(new_df, tib)
        }
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(new_df)
}
