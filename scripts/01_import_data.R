# ----------------------------------------------------------
# 01_import_data.R
# Import pediatric HIV dataset
# ----------------------------------------------------------

# This public script imports the original dataset, standardizes column names,
# performs a basic structural inspection, and saves an .rds version
# for downstream scripts.
#
# Selected inspection and validation details are intentionally omitted
# from the public version.


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------

library(readxl)     # Read Excel files.
library(tidyverse)  # Support the data analysis workflow.
library(janitor)    # Standardize column names.
library(here)       # Build project-relative file paths.


# ----------------------------------------------------------
# File paths
# ----------------------------------------------------------

raw_data_path <- here("data", "raw", "problema_aids_banco.xlsx")
processed_data_path <- here("data", "processed", "hiv_data_raw.rds")


# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------

data_raw <- read_excel(raw_data_path)


# ----------------------------------------------------------
# Standardize column names
# ----------------------------------------------------------

data_raw <- clean_names(data_raw)


# ----------------------------------------------------------
# Initial structural inspection
# ----------------------------------------------------------

print(head(data_raw))
str(data_raw)
print(summary(data_raw))
print(dim(data_raw))


# ----------------------------------------------------------
# Missing value inspection
# ----------------------------------------------------------

print(colSums(is.na(data_raw)))


# ----------------------------------------------------------
# Targeted variable inspection
# ----------------------------------------------------------

# --- targeted categorical and clinical variable checks omitted ---


# ----------------------------------------------------------
# Save imported dataset
# ----------------------------------------------------------

saveRDS(data_raw, processed_data_path)


# ----------------------------------------------------------
# Script completion message
# ----------------------------------------------------------

message("✓ Data import completed successfully")