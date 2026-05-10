# ----------------------------------------------------------
# 03_descriptive_analysis.R
# Descriptive analysis
# Pediatric HIV dataset
# ----------------------------------------------------------

# This script generates descriptive summaries by HAART group,
# creates Table 1 in Portuguese and English, and exports the
# resulting descriptive tables.
#
# Report-specific automated narrative helpers are intentionally
# omitted from the public version.


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------

library(tidyverse)
library(gtsummary)
library(here)
library(readr)
library(writexl)


# ----------------------------------------------------------
# Load cleaned dataset
# ----------------------------------------------------------

data_clean <- readRDS(
  here("data", "processed", "hiv_data_clean.rds")
)


# ----------------------------------------------------------
# Output directory
# ----------------------------------------------------------

dir.create(
  here("tables"),
  showWarnings = FALSE,
  recursive = TRUE
)


# ----------------------------------------------------------
# Total sample size
# ----------------------------------------------------------

n_total <- nrow(data_clean)


# ----------------------------------------------------------
# Missing values summary
# ----------------------------------------------------------

missing_summary <- tibble(
  variable = names(data_clean),
  n_missing = colSums(is.na(data_clean)),
  pct_missing = round(colSums(is.na(data_clean)) / nrow(data_clean) * 100, 2)
)


# ----------------------------------------------------------
# Categorical summaries
# ----------------------------------------------------------

sexo_tab <- data_clean %>%
  count(sexo) %>%
  mutate(percentual = round(n / sum(n) * 100, 2))

haart_tab <- data_clean %>%
  count(haart) %>%
  mutate(percentual = round(n / sum(n) * 100, 2))


# ----------------------------------------------------------
# Numeric summary - overall
# ----------------------------------------------------------

numeric_summary <- data_clean %>%
  summarise(
    n_total = n(),

    ninternou_mediana = median(ninternou, na.rm = TRUE),
    ninternou_p25 = quantile(ninternou, 0.25, na.rm = TRUE),
    ninternou_p75 = quantile(ninternou, 0.75, na.rm = TRUE),

    cd4in_mediana = median(cd4in, na.rm = TRUE),
    cd4in_p25 = quantile(cd4in, 0.25, na.rm = TRUE),
    cd4in_p75 = quantile(cd4in, 0.75, na.rm = TRUE),

    cd4ult_mediana = median(cd4ult, na.rm = TRUE),
    cd4ult_p25 = quantile(cd4ult, 0.25, na.rm = TRUE),
    cd4ult_p75 = quantile(cd4ult, 0.75, na.rm = TRUE),

    cvin_mediana = median(cvin, na.rm = TRUE),
    cvin_p25 = quantile(cvin, 0.25, na.rm = TRUE),
    cvin_p75 = quantile(cvin, 0.75, na.rm = TRUE),

    cvult_mediana = median(cvult, na.rm = TRUE),
    cvult_p25 = quantile(cvult, 0.25, na.rm = TRUE),
    cvult_p75 = quantile(cvult, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    across(-n_total, ~ round(.x, 2))
  )


# ----------------------------------------------------------
# Numeric summary - by HAART group
# ----------------------------------------------------------

haart_numeric_summary <- data_clean %>%
  group_by(haart) %>%
  summarise(
    n = n(),

    ninternou_mediana = median(ninternou, na.rm = TRUE),
    ninternou_p25 = quantile(ninternou, 0.25, na.rm = TRUE),
    ninternou_p75 = quantile(ninternou, 0.75, na.rm = TRUE),

    cd4in_mediana = median(cd4in, na.rm = TRUE),
    cd4in_p25 = quantile(cd4in, 0.25, na.rm = TRUE),
    cd4in_p75 = quantile(cd4in, 0.75, na.rm = TRUE),

    cd4ult_mediana = median(cd4ult, na.rm = TRUE),
    cd4ult_p25 = quantile(cd4ult, 0.25, na.rm = TRUE),
    cd4ult_p75 = quantile(cd4ult, 0.75, na.rm = TRUE),

    cvin_mediana = median(cvin, na.rm = TRUE),
    cvin_p25 = quantile(cvin, 0.25, na.rm = TRUE),
    cvin_p75 = quantile(cvin, 0.75, na.rm = TRUE),

    cvult_mediana = median(cvult, na.rm = TRUE),
    cvult_p25 = quantile(cvult, 0.25, na.rm = TRUE),
    cvult_p75 = quantile(cvult, 0.75, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )


# ----------------------------------------------------------
# Select and format variables for Table 1
# ----------------------------------------------------------

table_data <- data_clean %>%
  select(
    haart,
    sexo,
    internou,
    grave,
    modleve,
    desnutri,
    obito,
    ninternou,
    cd4in,
    cd4ult,
    cvin,
    cvult
  )

table_data_pt <- table_data %>%
  mutate(
    haart = factor(
      haart,
      levels = c("No_HAART", "HAART"),
      labels = c("Sem HAART", "HAART")
    ),
    sexo = factor(
      sexo,
      levels = c("Female", "Male"),
      labels = c("Feminino", "Masculino")
    ),
    internou = factor(
      internou,
      levels = c("No", "Yes"),
      labels = c("Não", "Sim")
    ),
    grave = factor(
      grave,
      levels = c("No", "Yes"),
      labels = c("Não", "Sim")
    ),
    modleve = factor(
      modleve,
      levels = c("No", "Yes"),
      labels = c("Não", "Sim")
    ),
    desnutri = factor(
      desnutri,
      levels = c("No", "Yes"),
      labels = c("Não", "Sim")
    ),
    obito = factor(
      obito,
      levels = c("Alive", "Death"),
      labels = c("Vivo", "Óbito")
    )
  )

table_data_en <- table_data %>%
  mutate(
    haart = factor(
      haart,
      levels = c("No_HAART", "HAART"),
      labels = c("No HAART", "HAART")
    )
  )


# ----------------------------------------------------------
# Create Table 1 - Portuguese
# ----------------------------------------------------------

table1_pt <- table_data_pt %>%
  tbl_summary(
    by = haart,
    label = list(
      sexo ~ "Sexo",
      internou ~ "Internação",
      grave ~ "Doença grave",
      modleve ~ "Doença moderada/leve",
      desnutri ~ "Desnutrição",
      obito ~ "Óbito",
      ninternou ~ "Número de internações",
      cd4in ~ "CD4 inicial",
      cd4ult ~ "CD4 final",
      cvin ~ "Carga viral inicial",
      cvult ~ "Carga viral final"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_overall(last = FALSE, col_label = "**Total**<br>N = {N}") %>%
  modify_header(label = "**Característica**") %>%
  modify_footnote_header(
    footnote = "n (%); Mediana (Q1, Q3)",
    columns = all_stat_cols()
  ) %>%
  bold_labels()


# ----------------------------------------------------------
# Create Table 1 - English
# ----------------------------------------------------------

table1_en <- table_data_en %>%
  tbl_summary(
    by = haart,
    label = list(
      sexo ~ "Sex",
      internou ~ "Hospitalization",
      grave ~ "Severe opportunistic disease",
      modleve ~ "Mild/moderate opportunistic disease",
      desnutri ~ "Malnutrition",
      obito ~ "Death",
      ninternou ~ "Number of hospitalizations",
      cd4in ~ "Baseline CD4 count",
      cd4ult ~ "Final CD4 count",
      cvin ~ "Baseline viral load",
      cvult ~ "Final viral load"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_overall() %>%
  bold_labels()


# ----------------------------------------------------------
# Export tables
# ----------------------------------------------------------

table1_pt_export <- as_tibble(table1_pt)
table1_en_export <- as_tibble(table1_en)

write_csv(
  table1_pt_export,
  here("tables", "table1_descriptive_pt.csv")
)

write_xlsx(
  table1_pt_export,
  here("tables", "table1_descriptive_pt.xlsx")
)

write_csv(
  table1_en_export,
  here("tables", "table1_descriptive_en.csv")
)

write_xlsx(
  table1_en_export,
  here("tables", "table1_descriptive_en.xlsx")
)

# Backward-compatible exports for existing file names.
write_csv(
  table1_pt_export,
  here("tables", "table1_descriptive.csv")
)

write_xlsx(
  table1_pt_export,
  here("tables", "table1_descriptive.xlsx")
)


# ----------------------------------------------------------
# Public version note
# ----------------------------------------------------------

# Automated report narrative objects, descriptive contrast rules,
# and bilingual conclusion helpers are intentionally omitted from
# this public script. Rendered reports are provided as analysis outputs.


# ----------------------------------------------------------
# Finalization message
# ----------------------------------------------------------

message("✓ Descriptive Table 1 created successfully.")