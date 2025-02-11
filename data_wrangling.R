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
ehr <- import(here("data", "ehr.xlsx")) # Import EHR data (780 observations)
and <- import(here("data", "and.xlsx")) # Import A&D data (716 observations)

# Clean colnames
ehr <- clean_names(ehr) 
and <- clean_names(and)