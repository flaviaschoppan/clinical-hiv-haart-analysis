# ----------------------------------------------------------
# 02_data_cleaning.R
# Data cleaning and preparation
# Pediatric HIV dataset
# ----------------------------------------------------------

# This public version documents the analytical intent,
# data dependencies, and structural flow of the cleaning step.
# Selected recoding, validation, and clinical variable preparation
# details are intentionally omitted.


# ----------------------------------------------------------
# Imports
# ----------------------------------------------------------

library(tidyverse)
library(here)


# ----------------------------------------------------------
# Configuration
# ----------------------------------------------------------

RAW_DATASET_PATH <- here("data", "processed", "hiv_data_raw.rds")
CLEAN_DATASET_PATH <- here("data", "processed", "hiv_data_clean.rds")


# ----------------------------------------------------------
# Data loading
# ----------------------------------------------------------

data_raw <- readRDS(RAW_DATASET_PATH)


# ----------------------------------------------------------
# Initial inspection
# ----------------------------------------------------------

glimpse(data_raw)


# ----------------------------------------------------------
# Data cleaning and preparation
# ----------------------------------------------------------

data_clean <- data_raw

# --- categorical recoding logic omitted ---

# --- ordered clinical classification logic omitted ---

# --- derived clinical variable logic omitted ---


# ----------------------------------------------------------
# Validation checks
# ----------------------------------------------------------

# --- structural and categorical validation checks omitted ---

# --- clinical numeric inspection logic omitted ---


# ----------------------------------------------------------
# Final inspection
# ----------------------------------------------------------

glimpse(data_clean)


# ----------------------------------------------------------
# Save cleaned dataset
# ----------------------------------------------------------

saveRDS(data_clean, CLEAN_DATASET_PATH)


# ----------------------------------------------------------
# Finalization
# ----------------------------------------------------------

message("✓ Data cleaning completed.")