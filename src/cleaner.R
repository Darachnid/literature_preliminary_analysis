# =============================================================================
# Data Cleaner: Convert wide-format Qualtrics export to long-format CSV
#
# This script trims metadata, pivots coder variables from wide to long format,
# filters irrelevant or empty values, and exports the cleaned dataset.
#
# INPUT:
#  - data/Claim+Analysis_June+18,+2025_09.53.csv (Qualtrics export)
#
# OUTPUT:
#  - out/tables/cleandata.csv (long-format coding data)
# =============================================================================


# ---- Load libraries ------------------------------------------------------ #

library(tidyverse)


# ---- Read and trim raw data --------------------------------------------- #

raw <- read.csv("data/Claim+Analysis_June+18,+2025_09.53.csv")
raw <- raw[3:nrow(raw), ]  # Skip Qualtrics metadata rows


# ---- Extract coder columns only ----------------------------------------- #

incols <- names(raw)[18:length(names(raw))]
widedata <- raw[, incols]
rm(raw, incols)


# ---- Pivot from wide to long format ------------------------------------- #

cols_to_pivot <- grep("^X\\d+_", names(widedata), value = TRUE)

longdata <- widedata |>
  pivot_longer(
    cols = all_of(cols_to_pivot),
    names_to = c("group", "variable"),
    names_pattern = "^X(\\d+)_(.*)",
    values_to = "value"
  ) |>
  filter(value != "") |>
  select(coder.id, paper.doi_1, Aim,performance.type, 
         Aim.disagree, Q49, group, variable, value) |>
  mutate(Aim = if_else(Q49 != "", Q49, Aim)) |>
  select(-Q49, -Aim.disagree) 

meta_vars <- c("existence", "fulfillment", "section", "problem.or.benefit")

df_wide <- longdata |>
  filter(variable %in% meta_vars) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  )

# Step 2: Keep all other variables in long format and join them to the meta info
df_long_rest <- longdata |>
  filter(!variable %in% meta_vars)

# Step 3: Join them back together on ID columns (like group)
final <- df_long_rest |>
  left_join(df_wide, by = c("paper.doi_1", "group", "Aim", "coder.id", "performance.type")) |>
  filter(variable != "more") |> 
  unnest(existence) |>
  unnest(fulfillment) |>
  mutate(
    existence = as.numeric(str_extract(existence, "^-?\\d+(\\.\\d+)?")),
    fulfillment = as.numeric(str_extract(fulfillment, "^-?\\d+(\\.\\d+)?"))
  ) |>
  rename(BLOC = problem.or.benefit) |>
  separate_rows(BLOC, sep = ",") |>
  separate_rows(value, sep = ",") |>
  filter(variable != "develop.or.adopt") |>
  filter(value != "Humans") |>
  filter(value != "Public Health") |>
  filter(value != "Poor Sensory Attribute") |>
  rename(coder = coder.id,
         doi = paper.doi_1,
         aim = Aim) |> 
  unnest(section) |>
  write_csv("out/tables/cleandata.csv")
final

# ---- Write cleaned output ------------------------------------------------ #

write_csv(final, "out/tables/cleandata.csv")
rm(longdata_clean)
