######################################################################
##  Papers per Aim by 5‑Year Bin – empty‑bin columns removed        ##
######################################################################

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(reactable)

## ── 1  LOAD & PREP  -------------------------------------------------
papers <- read_csv("out/tables/paper_tbl.csv", show_col_types = FALSE)
aims   <- read_csv("data/AIM_tbl.csv",        show_col_types = FALSE)

plotdata <- papers %>%
  select(dateCreated, aimID = aim) %>%
  filter(!aimID %in% c("Cell Differentiation", "Religion", "MIN", "Review")) %>%
  left_join(aims, by = "aimID") %>%
  select(Aim = aimLongName, Date = dateCreated) %>%
  drop_na(Aim)

## ── 2  5‑YEAR BINS  -------------------------------------------------
min_year <- year(min(plotdata$Date, na.rm = TRUE))
max_year <- year(max(plotdata$Date, na.rm = TRUE))

bin_starts <- seq(floor(min_year / 5) * 5, ceiling(max_year / 5) * 5, by = 5)
bin_labels <- sprintf("%d–%d", bin_starts, bin_starts + 4)

plotdata <- plotdata %>%
  mutate(
    BinStart = bin_starts[findInterval(year(Date), bin_starts, left.open = FALSE)],
    BinLabel = factor(sprintf("%d–%d", BinStart, BinStart + 4), levels = bin_labels)
  )

## ── 3  COUNT PAPERS PER AIM × BIN  ---------------------------------
counts_long <- plotdata %>%
  count(Aim, BinLabel, name = "n") %>%
  complete(Aim, BinLabel, fill = list(n = 0))

## order aims by total volume
aim_levels <- counts_long %>%
  group_by(Aim) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(Aim)

counts_long$Aim <- factor(counts_long$Aim, levels = aim_levels)

counts_wide <- counts_long %>%
  pivot_wider(names_from = BinLabel, values_from = n, values_fill = 0) %>%
  arrange(match(Aim, aim_levels))

## ── 4  DROP BIN COLUMNS WITH ALL ZEROS  -----------------------------
non_empty_cols <- counts_wide %>%
  select(-Aim) %>%                             # leave Aim for now
  summarise(across(everything(), sum)) %>%
  select(where(~ .x > 0)) %>%                  # keep columns with at least one paper
  names()

counts_wide <- counts_wide %>%
  select(Aim, all_of(non_empty_cols)) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

## ── 5  REACTABLE TABLE  --------------------------------------------
num_cols <- setdiff(names(counts_wide), "Aim")

p <- reactable(
  counts_wide,
  defaultSorted     = "Total",
  defaultSortOrder  = "desc",
  searchable        = TRUE,
  filterable        = TRUE,
  striped           = TRUE,
  highlight         = TRUE,
  defaultPageSize   = 20,
  paginationType    = "jump",
  theme = reactableTheme(
    stripedColor   = "#f7f7f7",
    highlightColor = "#ffe7ad",
    style = list(fontFamily = "system-ui, sans-serif", fontSize = 14)
  ),
  columns = c(
    list(Aim = colDef(name = "Aim", sticky = "left", minWidth = 220)),
    setNames(
      lapply(num_cols, function(col) {
        colDef(align = "right",
               format = colFormat(separators = TRUE, digits = 0),
               minWidth = 70)
      }),
      num_cols
    )
  ),
  wrap = FALSE
)
p

saveWidget(p, file = "out/plots/TableAims.html")
