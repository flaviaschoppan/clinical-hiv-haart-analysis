# ----------------------------------------------------------
# 05_visualizations.R
# Main visualizations
# Pediatric HIV dataset
# ----------------------------------------------------------

# This public script generates the main figures used to visually
# inspect selected clinical and therapeutic patterns in the
# pediatric HIV dataset.
#
# Statistical tests are handled in dedicated analysis scripts.


# ----------------------------------------------------------
# Load libraries
# ----------------------------------------------------------

library(tidyverse)
library(here)
library(ggplot2)
library(scales)


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
  here("figures"),
  showWarnings = FALSE,
  recursive = TRUE
)


# ----------------------------------------------------------
# Validate required variables
# ----------------------------------------------------------

required_vars <- c(
  "haart",
  "cdcult",
  "ninternou"
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
# Plot 1: HAART distribution by final clinical classification
# ----------------------------------------------------------

plot_cdcult_haart <- ggplot(
  plot_data,
  aes(
    x = cdcult,
    fill = haart_plot
  )
) +
  geom_bar(
    position = "fill",
    color = "#222222",
    linewidth = 0.25,
    alpha = 0.92
  ) +
  scale_fill_manual(
    values = haart_box_palette
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "HAART Use by Final Clinical-Laboratory Classification",
    x = "Final clinical-laboratory classification",
    y = "Proportion of children",
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
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    legend.text = element_text(
      size = 10
    ),
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
# Save Plot 1
# ----------------------------------------------------------

ggsave(
  filename = here("figures", "haart_cdcult_distribution.png"),
  plot = plot_cdcult_haart,
  width = 8,
  height = 5,
  dpi = 300
)


# ----------------------------------------------------------
# Plot 2: Number of hospitalizations by HAART group
# ----------------------------------------------------------

plot_ninternou_haart <- ggplot(
  plot_data,
  aes(
    x = haart_plot,
    y = ninternou
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
  geom_jitter(
    aes(fill = haart_plot),
    width = 0.15,
    alpha = 0.88,
    size = 2.4,
    shape = 21,
    color = "#111111",
    stroke = 0.35
  ) +
  scale_fill_manual(
    values = haart_box_palette
  ) +
  labs(
    title = "Number of Hospitalizations by HAART Group",
    x = "HAART group",
    y = "Number of hospitalizations",
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
# Save Plot 2
# ----------------------------------------------------------

ggsave(
  filename = here("figures", "ninternou_haart_boxplot.png"),
  plot = plot_ninternou_haart,
  width = 8,
  height = 5,
  dpi = 300
)


# ----------------------------------------------------------
# Finalization message
# ----------------------------------------------------------

cat("✓ Main visualizations completed successfully.\n")