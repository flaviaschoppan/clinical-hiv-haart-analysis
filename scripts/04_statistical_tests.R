# ----------------------------------------------------------
# 04_statistical_tests.R
# Association tests between HAART and categorical variables
# Pediatric HIV dataset
# ----------------------------------------------------------

# This script performs association tests between HAART group
# and selected categorical variables, generating technical and
# report-ready outputs.
#
# Extended methodological notes, teaching annotations, and
# internal reporting rationale are intentionally omitted from
# the public version.


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------

library(tidyverse)
library(here)
library(readr)


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
# Define categorical variables
# ----------------------------------------------------------

vars_categorical <- c(
  sexo = "Sex",
  internou = "Hospitalization",
  grave = "Severe disease",
  modleve = "Mild/moderate disease",
  desnutri = "Malnutrition",
  obito = "Death",
  cdcult = "Final CDC clinical-laboratory classification"
)


# ----------------------------------------------------------
# Validate required variables
# ----------------------------------------------------------

required_vars <- c("haart", names(vars_categorical))

missing_vars <- setdiff(required_vars, names(data_clean))

if (length(missing_vars) > 0) {
  stop(
    "Missing required variable(s): ",
    paste(missing_vars, collapse = ", ")
  )
}


# ----------------------------------------------------------
# Helper function to format p-values
# ----------------------------------------------------------

format_p_value <- function(p_value) {
  case_when(
    is.na(p_value) ~ NA_character_,
    p_value < 0.001 ~ "<0.001",
    TRUE ~ formatC(p_value, format = "f", digits = 3)
  )
}


# ----------------------------------------------------------
# Function to run association test
# ----------------------------------------------------------

run_association_test <- function(var_name, var_label) {
  cat("Running association test for:", var_name, "\n")

  contingency_table <- table(
    data_clean[[var_name]],
    data_clean$haart
  )

  if (nrow(contingency_table) < 2 || ncol(contingency_table) < 2) {
    return(
      tibble(
        variable = var_name,
        variable_label = var_label,
        test_used = "Not applicable",
        p_value = NA_real_,
        p_value_formatted = format_p_value(NA_real_),
        min_expected = NA_real_,
        fisher_used = NA,
        odds_ratio = NA_real_,
        odds_ratio_note = "Not calculated because the contingency table has fewer than two rows or columns."
      )
    )
  }

  chi_result <- suppressWarnings(
    chisq.test(contingency_table, correct = FALSE)
  )

  min_expected <- min(chi_result$expected)
  use_fisher <- any(chi_result$expected < 5)

  if (use_fisher) {
    test_result <- fisher.test(contingency_table)

    test_used <- "Fisher's exact test"
    p_value <- test_result$p.value

    odds_ratio <- if (all(dim(contingency_table) == c(2, 2))) {
      unname(test_result$estimate)
    } else {
      NA_real_
    }

    odds_ratio_note <- if (all(dim(contingency_table) == c(2, 2))) {
      "Odds ratio returned by fisher.test according to factor and table ordering."
    } else {
      "Odds ratio not calculated because the contingency table is not 2x2."
    }
  } else {
    test_used <- "Pearson's chi-square test"
    p_value <- chi_result$p.value
    odds_ratio <- NA_real_
    odds_ratio_note <- "Odds ratio not calculated for chi-square test output."
  }

  tibble(
    variable = var_name,
    variable_label = var_label,
    test_used = test_used,
    p_value = p_value,
    p_value_formatted = format_p_value(p_value),
    min_expected = min_expected,
    fisher_used = use_fisher,
    odds_ratio = odds_ratio,
    odds_ratio_note = odds_ratio_note
  )
}


# ----------------------------------------------------------
# Run association tests
# ----------------------------------------------------------

test_results <- map2_dfr(
  names(vars_categorical),
  unname(vars_categorical),
  run_association_test
)


# ----------------------------------------------------------
# Create report-ready table - English
# ----------------------------------------------------------

test_results_report_en <- test_results %>%
  transmute(
    Variable = variable_label,
    Test = test_used,
    `p-value` = p_value_formatted,
    `Minimum expected count` = round(min_expected, 2),
    `Low expected count` = if_else(fisher_used, "Yes", "No"),
    `Odds ratio` = if_else(
      is.na(odds_ratio),
      NA_character_,
      formatC(odds_ratio, format = "f", digits = 3)
    )
  )

# Backward-compatible alias for the main report-ready table.
test_results_report <- test_results_report_en


# ----------------------------------------------------------
# Create report-ready table - Portuguese
# ----------------------------------------------------------

test_results_report_pt <- test_results %>%
  transmute(
    Variável = recode(
      variable,
      sexo = "Sexo",
      internou = "Internação",
      grave = "Doença grave",
      modleve = "Doença moderada/leve",
      desnutri = "Desnutrição",
      obito = "Óbito",
      cdcult = "Classificação clínico-laboratorial final"
    ),
    Teste = recode(
      test_used,
      `Fisher's exact test` = "Teste exato de Fisher",
      `Pearson's chi-square test` = "Qui-quadrado de Pearson",
      `Not applicable` = "Não aplicável",
      .default = test_used
    ),
    `p-valor` = p_value_formatted,
    `Menor frequência esperada` = round(min_expected, 2),
    `Baixa frequência esperada` = if_else(fisher_used, "Sim", "Não"),
    `Razão de chances` = if_else(
      is.na(odds_ratio),
      NA_character_,
      formatC(odds_ratio, format = "f", digits = 3)
    )
  )


# ----------------------------------------------------------
# Export results
# ----------------------------------------------------------

write_csv(
  test_results,
  here("tables", "haart_association_tests.csv")
)

write_csv(
  test_results_report_en,
  here("tables", "haart_association_tests_report_en.csv")
)

write_csv(
  test_results_report_pt,
  here("tables", "haart_association_tests_report_pt.csv")
)

# Backward-compatible export for the main report-ready table.
write_csv(
  test_results_report,
  here("tables", "haart_association_tests_report.csv")
)


# ----------------------------------------------------------
# Finalization message
# ----------------------------------------------------------

cat("✓ HAART association tests completed successfully.\n")