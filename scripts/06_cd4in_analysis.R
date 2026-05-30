# ----------------------------------------------------------
# 06_cd4in_analysis.R
# Baseline CD4+ T lymphocyte count by HAART group
# Pediatric HIV dataset
# ----------------------------------------------------------

# This public script analyzes baseline CD4+ T lymphocyte count
# by HAART group. It generates a visualization, evaluates
# normality within each group, selects a two-group comparison
# test, and exports technical and report-ready outputs.


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------

library(tidyverse)
library(here)
library(readr)
library(ggplot2)


# ----------------------------------------------------------
# Load cleaned dataset
# ----------------------------------------------------------

data_clean <- readRDS(
  here("data", "processed", "hiv_data_clean.rds")
)


# ----------------------------------------------------------
# Output directories
# ----------------------------------------------------------

dir.create(
  here("figures"),
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  here("tables"),
  showWarnings = FALSE,
  recursive = TRUE
)


# ----------------------------------------------------------
# Validate required variables
# ----------------------------------------------------------

required_vars <- c(
  "haart",
  "cd4in"
)

missing_vars <- setdiff(required_vars, names(data_clean))

if (length(missing_vars) > 0) {
  stop(
    "Missing required variable(s): ",
    paste(missing_vars, collapse = ", ")
  )
}


# ----------------------------------------------------------
# Prepare visualization labels
# ----------------------------------------------------------

plot_data <- data_clean %>%
  mutate(
    haart_plot = factor(
      haart,
      levels = c("No_HAART", "HAART"),
      labels = c("No HAART", "HAART")
    )
  )


# ----------------------------------------------------------
# Define visualization palette
# ----------------------------------------------------------

haart_box_palette <- c(
  "No HAART" = "#1D3557",
  "HAART" = "#ED5656"
)


# ----------------------------------------------------------
# Inspect variable
# ----------------------------------------------------------

cd4in_summary <- data_clean %>%
  summarise(
    n_total = n(),
    n_non_missing = sum(!is.na(cd4in)),
    n_missing = sum(is.na(cd4in)),
    median = median(cd4in, na.rm = TRUE),
    p25 = quantile(cd4in, 0.25, na.rm = TRUE),
    p75 = quantile(cd4in, 0.75, na.rm = TRUE),
    min = min(cd4in, na.rm = TRUE),
    max = max(cd4in, na.rm = TRUE)
  )


# ----------------------------------------------------------
# Plot: Baseline CD4 by HAART group
# ----------------------------------------------------------

plot_cd4in_haart <- ggplot(
  plot_data,
  aes(
    x = haart_plot,
    y = cd4in
  )
) +
  geom_boxplot(
    aes(fill = haart_plot),
    width = 0.58,
    alpha = 0.82,
    outlier.shape = NA,
    color = "#222222",
    linewidth = 0.55
  ) +
  geom_point(
    aes(fill = haart_plot),
    position = position_jitter(
      width = 0.15,
      height = 0
    ),
    alpha = 0.78,
    size = 2.3,
    shape = 21,
    color = "#111111",
    stroke = 0.30
  ) +
  scale_fill_manual(
    values = haart_box_palette
  ) +
  labs(
    title = "Baseline CD4+ T Lymphocyte Count by HAART Group",
    x = "HAART group",
    y = "Baseline CD4+ T lymphocyte count",
    fill = "HAART group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11,
      color = "#222222"
    ),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "#E6E6E6",
      linewidth = 0.4
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )


# ----------------------------------------------------------
# Plot: Baseline CD4 by HAART group - full scale
# ----------------------------------------------------------

plot_cd4in_haart_full <- ggplot(
  plot_data,
  aes(
    x = haart_plot,
    y = cd4in
  )
) +
  geom_boxplot(
    aes(fill = haart_plot),
    width = 0.58,
    alpha = 0.82,
    outlier.shape = NA,
    color = "#222222",
    linewidth = 0.55
  ) +
  geom_point(
    aes(fill = haart_plot),
    position = position_jitter(
      width = 0.15,
      height = 0
    ),
    alpha = 0.78,
    size = 2.3,
    shape = 21,
    color = "#111111",
    stroke = 0.30
  ) +
  scale_fill_manual(
    values = haart_box_palette
  ) +
  labs(
    title = "Baseline CD4+ T Lymphocyte Count by HAART Group",
    x = "HAART group",
    y = "Baseline CD4+ T lymphocyte count",
    fill = "HAART group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11,
      color = "#222222"
    ),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "#E6E6E6",
      linewidth = 0.4
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )


# ----------------------------------------------------------
# Plot: Baseline CD4 by HAART group - zoomed view
# ----------------------------------------------------------

plot_cd4in_haart <- plot_cd4in_haart_full +
  # Visual zoom only; boxplot statistics are computed from original values.
  coord_cartesian(
    ylim = c(0, 4500)
  )


# ----------------------------------------------------------
# Save plots
# ----------------------------------------------------------

ggsave(
  filename = here("figures", "cd4in_haart_boxplot_full_scale.png"),
  plot = plot_cd4in_haart_full,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = here("figures", "cd4in_haart_boxplot.png"),
  plot = plot_cd4in_haart,
  width = 8,
  height = 5,
  dpi = 300
)


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
# Function to run Shapiro-Wilk normality test by group
# ----------------------------------------------------------

run_shapiro_by_group <- function(data, variable, group_var) {
  data %>%
    filter(
      !is.na(.data[[variable]]),
      !is.na(.data[[group_var]])
    ) %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n = n(),
      shapiro_p_value = if_else(
        n >= 3,
        shapiro.test(.data[[variable]])$p.value,
        NA_real_
      ),
      .groups = "drop"
    )
}


# ----------------------------------------------------------
# Function to run numeric comparison test
# ----------------------------------------------------------

run_numeric_comparison <- function(data, variable, group_var) {
  cat("Running numeric comparison for:", variable, "\n")

  analysis_data <- data %>%
    filter(
      !is.na(.data[[variable]]),
      !is.na(.data[[group_var]])
    )

  normality_results <- run_shapiro_by_group(
    data = analysis_data,
    variable = variable,
    group_var = group_var
  )

  if (n_distinct(analysis_data[[group_var]]) != 2) {
    return(
      tibble(
        variable = variable,
        test_used = "Not applicable",
        statistic = NA_real_,
        p_value = NA_real_,
        p_value_formatted = format_p_value(NA_real_),
        normality_p_no_haart = NA_real_,
        normality_p_haart = NA_real_,
        normality_decision = "Comparison requires exactly two groups.",
        test_decision = "Not applied."
      )
    )
  }

  normality_p_no_haart <- normality_results %>%
    filter(.data[[group_var]] == "No_HAART") %>%
    pull(shapiro_p_value)

  normality_p_haart <- normality_results %>%
    filter(.data[[group_var]] == "HAART") %>%
    pull(shapiro_p_value)

  normality_p_no_haart <- if_else(
    length(normality_p_no_haart) == 0,
    NA_real_,
    normality_p_no_haart
  )

  normality_p_haart <- if_else(
    length(normality_p_haart) == 0,
    NA_real_,
    normality_p_haart
  )

  use_wilcoxon <- any(
    c(normality_p_no_haart, normality_p_haart) < 0.05,
    na.rm = TRUE
  )

  if (any(is.na(c(normality_p_no_haart, normality_p_haart)))) {
    use_wilcoxon <- TRUE
  }

  if (use_wilcoxon) {
    test_result <- wilcox.test(
      analysis_data[[variable]] ~ analysis_data[[group_var]]
    )

    test_used <- "Wilcoxon rank-sum test"
    statistic <- unname(test_result$statistic)
    p_value <- test_result$p.value
    test_decision <- "Wilcoxon test selected because at least one group did not meet the Shapiro-Wilk normality criterion or normality could not be fully assessed."
  } else {
    test_result <- t.test(
      analysis_data[[variable]] ~ analysis_data[[group_var]]
    )

    test_used <- "Welch two-sample t-test"
    statistic <- unname(test_result$statistic)
    p_value <- test_result$p.value
    test_decision <- "Welch t-test selected because both groups met the Shapiro-Wilk normality criterion."
  }

  normality_decision <- paste0(
    "Shapiro-Wilk p-values: No HAART = ",
    format_p_value(normality_p_no_haart),
    "; HAART = ",
    format_p_value(normality_p_haart),
    "."
  )

  tibble(
    variable = variable,
    test_used = test_used,
    statistic = statistic,
    p_value = p_value,
    p_value_formatted = format_p_value(p_value),
    normality_p_no_haart = normality_p_no_haart,
    normality_p_haart = normality_p_haart,
    normality_decision = normality_decision,
    test_decision = test_decision
  )
}


# ----------------------------------------------------------
# Run numeric comparison test
# ----------------------------------------------------------

test_cd4in <- run_numeric_comparison(
  data = data_clean,
  variable = "cd4in",
  group_var = "haart"
)


# ----------------------------------------------------------
# Create report-ready table - English
# ----------------------------------------------------------

test_cd4in_report_en <- test_cd4in %>%
  transmute(
    Variable = "Baseline CD4+ T lymphocyte count",
    Test = test_used,
    Statistic = round(statistic, 3),
    `p-value` = p_value_formatted,
    `Shapiro-Wilk p-value: No HAART` = format_p_value(normality_p_no_haart),
    `Shapiro-Wilk p-value: HAART` = format_p_value(normality_p_haart),
    `Test selection` = test_decision
  )

# Backward-compatible alias for the main report-ready table.
test_cd4in_report <- test_cd4in_report_en


# ----------------------------------------------------------
# Create report-ready table - Portuguese
# ----------------------------------------------------------

test_cd4in_report_pt <- test_cd4in %>%
  transmute(
    Variável = "Contagem inicial de linfócitos T CD4+",
    Teste = recode(
      test_used,
      `Wilcoxon rank-sum test` = "Teste de Wilcoxon para amostras independentes",
      `Welch two-sample t-test` = "Teste t de Welch para duas amostras",
      `Not applicable` = "Não aplicável",
      .default = test_used
    ),
    Estatística = round(statistic, 3),
    `p-valor` = p_value_formatted,
    `p-valor Shapiro-Wilk: Sem HAART` = format_p_value(normality_p_no_haart),
    `p-valor Shapiro-Wilk: HAART` = format_p_value(normality_p_haart),
    `Critério de seleção do teste` = recode(
      test_decision,
      `Wilcoxon test selected because at least one group did not meet the Shapiro-Wilk normality criterion or normality could not be fully assessed.` =
        "Teste de Wilcoxon selecionado porque ao menos um grupo não atendeu ao critério de normalidade pelo teste de Shapiro-Wilk ou a normalidade não pôde ser totalmente avaliada.",
      `Welch t-test selected because both groups met the Shapiro-Wilk normality criterion.` =
        "Teste t de Welch selecionado porque ambos os grupos atenderam ao critério de normalidade pelo teste de Shapiro-Wilk.",
      `Not applied.` =
        "Não aplicado.",
      .default = test_decision
    )
  )


# ----------------------------------------------------------
# Export results
# ----------------------------------------------------------

write_csv(
  cd4in_summary,
  here("tables", "cd4in_summary.csv")
)

write_csv(
  test_cd4in,
  here("tables", "cd4in_haart_test.csv")
)

write_csv(
  test_cd4in_report_en,
  here("tables", "cd4in_haart_test_report_en.csv")
)

write_csv(
  test_cd4in_report_pt,
  here("tables", "cd4in_haart_test_report_pt.csv")
)


# ----------------------------------------------------------
# Finalization message
# ----------------------------------------------------------

cat("✓ Baseline CD4 analysis completed successfully.\n")