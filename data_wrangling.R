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
  SmartEDA, # EDA
  explore # EDA
)

# Import datasets
ehr <- import(here("data", "ehr.xlsx"), skip = 3) # Import EHR data (780 observations)
and <- import(here("data", "and.xlsx")) # Import A&D data (716 observations)

# Clean colnames
ehr <- clean_names(ehr) 
and <- clean_names(and)

# Select relevant features
and |>
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
create_report(ehr, report_title = "Mortality EDA (EHR)")
ExpReport(ehr, op_file="Mortality EDA (EHR)")

# Create eda reports for and
create_report(ehr, report_title = "Mortality EDA (A&D)")
ExpReport(ehr, op_file="Mortality EDA (A&D)")



