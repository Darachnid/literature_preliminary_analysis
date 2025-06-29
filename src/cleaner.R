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
  )
rm(widedata, cols_to_pivot)


# ---- Filter out irrelevant and empty entries ---------------------------- #

longdata_clean <- longdata |>
  filter(variable != "section") |>
  filter(!is.na(value), value != "", value != "yes")
rm(longdata)


# ---- Write cleaned output ------------------------------------------------ #

write_csv(longdata_clean, "out/tables/cleandata.csv")
rm(longdata_clean)
