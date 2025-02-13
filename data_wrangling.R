# Load libraries
pacman::p_load(
  rio, # Load dataset
  janitor, # Clean column names,
  tidyverse, # data wrangling
  here, # Relative path
  lubridate, # Working with dates
  feather, # Interoperable data object
  skimr, # EDA
  DataExplorer, # EDA
  explore # EDA
)

# Import datasets
ehr <- import(here("data", "ehr.xlsx"), skip = 3) # Convert "Missing" to NA # Import EHR data (777 observations)
and <- import(here("data", "and.xlsx")) %>%
  mutate(across(everything(), ~ na_if(trimws(as.character(.)), "Missing"))) %>%
  mutate(across(everything(), ~ na_if(., "DK/Missing")))  # Convert both to NA # Import A&D data (716 observations)

# Clean colnames
ehr <- clean_names(ehr) 
and <- clean_names(and)


# -------------------------------
# EHR cleaning

# Remove irrelevant features ehr
ehr <- ehr |>
  select(-c(sr_no,
            source_of_admission,
            bed_no,
            ward_room,
            visit_no
            ))
# Source of admission feature: NA

# Mutate admission and discharge date into date
ehr$admission_date <- dmy(ehr$admission_date)
ehr$discharge_date <- dmy(ehr$discharge_date)

# Function to convert all age units to years
convert_to_years <- function(age_str) {
  age_str <- str_trim(str_to_lower(age_str))  # Standardize text (lowercase, trim spaces)
  num <- as.numeric(str_extract(age_str, "\\d+"))  # Extract the numeric part
  
  if (is.na(num)) return(NA)  # Handle cases where extraction fails
  
  if (str_detect(age_str, "year")) {
    return(num)  # Already in years
  } else if (str_detect(age_str, "month")) {
    return(num / 12)  # Convert months to years
  } else if (str_detect(age_str, "week")) {
    return(num / 52)  # Convert weeks to years
  } else {
    return(NA)  # Handle unexpected cases
  }
}

# Apply conversion to dataset
ehr <- ehr %>%
  mutate(age_numeric = sapply(age, convert_to_years))

# Split into adults and pediatrics
adults <- ehr %>% filter(age_numeric >= 18)
pediatrics <- ehr %>% filter(age_numeric < 18)



#----------------------------
# Admissions and Discharge data
# Select relevant features and
and <- and |>
  select(
    patient_number,
    admission_date,
    admission_time,
    marital_status,
    educational_level,
    religion,
    ethnicity,
    ethnicity_other_specify,
    occupational_status,
    occupational_status_others_specify,
    comorbidities,
    severity_score_14,
    referral,
    referral_others_specify,
    antibiotic_treatment,
    surgery_before_during_admission,
    urinary_catheter,
    peripheral_catheter,
    central_venous_catheter,
    intubation,
    ng_tube,
    chest_tube,
    discharge_date,
    discharge_time,
    mortality
  )

# Create eda reports for ehr
skim(ehr)
create_report(ehr, report_title = "Mortality EDA (EHR)")

# Create eda reports for and
skim(and)
create_report(and, report_title = "Mortality EDA (A&D)")



