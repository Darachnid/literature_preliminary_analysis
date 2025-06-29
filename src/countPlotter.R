# =============================================================================
# Script Purpose:
# This script loads theme frequency data and creates a styled HTML table
# using conditional formatting to visually emphasize rows by thematic category.
# 
# Input:
# - "out/tables/theme_counts.csv": CSV file containing theme frequency data
#   including a column named 'BLOC' which categorizes each row.
# 
# Output:
# - An HTML-rendered table styled with color-coded row backgrounds by BLOC.
# =============================================================================

# ---- load_libraries ---------------------------------------------------------
library(tidyverse)    # for data handling
library(kableExtra)   # for HTML table styling

# ---- define_color_palette --------------------------------------------------
# RGBA colors to distinguish each thematic BLOC category visually
bloc_colors <- c(
  "Barrier"     = "rgba(255, 0, 0, 0.3)",
  "Opportunity" = "rgba(0, 128, 0, 0.3)",
  "Limitation"  = "rgba(255, 255, 0, 0.3)",
  "Consequence" = "rgba(148, 0, 211, 0.3)"
)

# ---- load_data -------------------------------------------------------------
theme_count <- read_csv("out/tables/theme_counts.csv")

# ---- generate_table_object -------------------------------------------------
# Builds a basic left-aligned kable table with HTML styling
tbl <- kable(theme_count, escape = FALSE) |>
  kable_styling(full_width = FALSE, position = "left")

# ---- apply_conditional_formatting ------------------------------------------
# Adds background color and borders based on BLOC category
for (i in seq_len(nrow(theme_count))) {
  tbl <- tbl |>
    row_spec(
      i,
      background = bloc_colors[theme_count$BLOC[i]],
      extra_css = "border-bottom: 2px solid black;"
    )
}

# ---- render_table ----------------------------------------------------------
tbl

