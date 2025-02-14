# Load libraries
pacman::p_load(
  rio, # Load dataset
  janitor, # Clean column names,
  tidyverse, # data wrangling
  here, # Relative path
  lubridate, # Working with dates
  hms, # Work with time
  feather, # Interoperable data object
  skimr, # EDA
  DataExplorer, # EDA
  explore # EDA
)

# Import datasets
ehr <- import(here("data", "ehr.xlsx"), skip = 3) # Convert "Missing" to NA # Import EHR data (777 observations)
and <- import(here("data", "and.xlsx")) %>%
  mutate(across(everything(), ~ na_if(trimws(as.character(.)), "Missing"))) %>%
  mutate(across(everything(), ~ na_if(., "DK/Missing"))) %>% 
  mutate(across(everything(), ~ na_if(., "999"))) 

# Convert both to NA # Import A&D data (716 observations)

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

# Function to standardize time format
clean_time <- function(x) {
  x <- trimws(x)  # Remove spaces
  ifelse(grepl("AM|PM", x), format(parse_date_time(x, "h:M p"), "%H:%M:%S"), 
         format(parse_date_time(x, "H:M"), "%H:%M:%S"))
}

# Apply function to both admission_time and discharge_time
ehr <- ehr %>%
  mutate(across(c(admission_time, discharge_time), clean_time))

# Convert cleaned time variables to hms format
ehr <- ehr %>%
  mutate(across(c(admission_time, discharge_time), as_hms))

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

# Rename severity_score_14 -> gcs
and <- and |> rename(
  gcs = severity_score_14
)

# Standardise admission and discharge time
# Apply function to both admission_time and discharge_time
and <- and %>%
  mutate(
    admission_time = str_remove(admission_time, "\\.\\d+Z"),
    discharge_time = str_remove(discharge_time, "\\.\\d+Z"),
    admission_time = as_hms(admission_time),
    discharge_time = as_hms(discharge_time)
  )

# Clean gcs data
and <- and %>%
  mutate(
    intubated = case_when(
      is.na(gcs) ~ NA_character_,  # Preserve missing values
      gcs == "NOT ASSESSED" ~ NA_character_,  # Recode "NOT ASSESSED" as NA
      str_detect(gcs, "[tT]") ~ "Yes",  # Check for 't' or 'T'
      TRUE ~ "No"
    ),
    gcs_cleaned = str_extract(gcs, "\\d+"),  # Extract numeric value
    gcs_cleaned = as.numeric(gcs_cleaned)  # Convert to numeric
  )

# Clean Other referrals
and <- and %>%
  mutate(referral_others_specify = case_when(
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("St\\. Francis|Zavier|Assin Fosu", ignore_case = TRUE)) ~ "St. Francis Xavier Hospital",
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("Home|from home", ignore_case = TRUE)) ~ "Home",
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("Dialysis", ignore_case = TRUE)) ~ "Dialysis unit",
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("saltpond municipal hospital", ignore_case = TRUE)) ~ "Saltpond Municipal Hospital",
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("ambulance", ignore_case = TRUE)) ~ "Ambulance",
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("Covid theatre", ignore_case = TRUE)) ~ "COVID theatre",
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("Covid holding bay", ignore_case = TRUE)) ~ "COVID holding bay",
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("ICU admission", ignore_case = TRUE)) ~ NA_character_,
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("trans in but it wasn't indicated", ignore_case = TRUE)) ~ NA_character_,
    !is.na(referral_others_specify) & str_detect(referral_others_specify, regex("A&e through dialysis", ignore_case = TRUE)) ~ "A&E",
    TRUE ~ referral_others_specify  # Keep original if no match
  ))

# Move A&E from others to referral feature
and %>%
  mutate(
    referral = ifelse(referral_others_specify == "A&E", "A&E", referral), 
    referral_others_specify = ifelse(referral_others_specify == "A&E", NA_character_, referral_others_specify)
  )


# Mutate LOS feature


# Create eda reports for ehr
skim(ehr)
create_report(ehr, report_title = "Mortality EDA (EHR)")

# Create eda reports for and
skim(and)
create_report(and, report_title = "Mortality EDA (A&D)")



