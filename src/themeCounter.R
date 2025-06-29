# ================================================================
# Summarize theme citation frequency across papers
# Input: 'paper_theme_tbl.csv', 'blocMetadata.csv'
# Output: 'theme_counts.csv' with count of papers per theme
# ================================================================

# ---- load packages ----
library(tidyverse)

# ---- read data ----
paper_theme_tbl <- read_csv("out/tables/paper_theme_tbl.csv") |>
  select(doi, theme = value)

bloc <- read_csv("out/tables/blocMetadata.csv") |> 
  rename(theme = themeID)

# ---- count papers per theme ----
theme_counts <- paper_theme_tbl |>
  distinct(doi, theme) |>           # ensure unique paper-theme pairs
  count(theme, name = "theme_count") |> 
  left_join(bloc, by = "theme") |>  # join to bloc metadata
  select("BLOC" = blocID, type, subtype, theme, papers = theme_count) |>
  arrange(desc(papers))

# ---- write results ----
write_csv(theme_counts, "out/tables/theme_counts.csv")

# ---- clean environment ----
rm(paper_theme_tbl, theme_counts)
