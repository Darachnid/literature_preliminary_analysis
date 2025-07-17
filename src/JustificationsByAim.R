###############################################################
##  Rain‑Cloud by Aim – coloured by Humans/Livestock/Env     ##
##  • one dot per paper‑theme pair (max 3 per paper)         ##
##  • no dot overlap, grouped legend, click opens DOI        ##
###############################################################

library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

## preferred legend labels ---------------------------------
display_name <- c(
  "Humans"          = "Human Wellbeing",
  "Livestock"       = "Livestock Wellbeing",
  "the Environment" = "Environmental Wellbeing"
)

## ── 1  LOAD DATA ───────────────────────────────────────────
papers <- read_csv("out/tables/paper_tbl.csv", show_col_types = FALSE)
aims   <- read_csv("data/AIM_tbl.csv",        show_col_types = FALSE)
themes <- read_csv("out/tables/paper_theme_tbl.csv", show_col_types = FALSE)

## ── 2  FILTER TO THREE THEMES & DUPLICATE ROWS -------------
theme_keep <- c("Humans", "Livestock", "the Environment")

themes_sub <- themes %>% 
  filter(themeID %in% theme_keep) %>% 
  distinct(doi, themeID)              # one row per (paper, theme)

plotdata <- papers %>% 
  select(DOI = doi, Date = dateCreated,
         aimID = aim, title, publisher = publisherID, journal = journalID) %>% 
  left_join(aims,  by = "aimID") %>% 
  left_join(themes_sub, by = c("DOI" = "doi")) %>%   # duplicate rows if multi‑theme
  filter(!is.na(themeID)) %>%                        # keep only papers w/ target themes
  distinct() %>% 
  transmute(
    Aim      = aimLongName,
    aimType,
    Theme    = themeID,
    DOI, Date, title, publisher, journal
  )

## ── 3  ORDER AIMS: Dev‑first, high‑>low within blocks -------
aim_counts <- plotdata %>% count(aimType, Aim, name = "n")

aim_levels <- aim_counts %>% 
  arrange(factor(aimType, levels = c("Development", "Assessment")),
          desc(n)) %>% 
  pull(Aim)

plotdata$Aim    <- factor(plotdata$Aim, levels = aim_levels)
plotdata$y_base <- as.numeric(plotdata$Aim)

## ── 4  FAST RANDOM OFFSETS – NEVER OVERLAP --------------------------
stack_gap <- 0.20     # minimum vertical spacing
jit_days  <- 50      # tiny horizontal jitter
set.seed(123)

plotdata <- plotdata %>%
  mutate(date_bin = as.Date(cut(Date, breaks = "45 days"))) %>%
  group_by(Aim, date_bin, DOI) %>%            # dots from same paper
  arrange(Theme) %>%                          # reproducible ordering
  mutate(
    offset = sample(                          # 1: random permutation …
      ((row_number()) - (n() + 1) / 2) * stack_gap   # … of an evenly‑spaced grid
    ),
    y_plot = y_base + offset,
    x_plot = Date + runif(n(), -jit_days, jit_days)           # slight x jitter
  ) %>%
  ungroup()

## ── 5  WIDTH‑SCALED “CLOUDS” -------------------------------
max_sqrt <- sqrt(max(aim_counts$n))
aim_counts <- aim_counts %>% 
  mutate(rel_width = 0.15 + 0.45 * sqrt(n) / max_sqrt)

## ── 6  PALETTE & URL ---------------------------------------
col_pal <- c("Humans" = "magenta",
             "Livestock" = "cyan",
             "the Environment" = 'yellow')

plotdata$color <- col_pal[plotdata$Theme]
plotdata$url   <- paste0("https://doi.org/", plotdata$DOI)

## ── 7  BUILD PLOTLY ----------------------------------------
p <- plot_ly()

## 7a  clouds (violins) per Aim
for (i in seq_along(aim_levels)) {
  this_aim <- aim_levels[i]
  w        <- aim_counts$rel_width[aim_counts$Aim == this_aim]
  
  p <- add_trace(
    p,
    data        = filter(plotdata, Aim == this_aim),
    type        = "violin",
    orientation = "h",
    x           = ~Date,
    y           = i,
    width       = w,
    spanmode    = "hard",
    points      = FALSE,
    line        = list(width = 0),
    fillcolor   = "rgba(160,160,160,0.25)",
    hoverinfo   = "skip",
    showlegend  = FALSE
  )
}

## 7b  dots – one scatter trace per Theme (drives clean legend)
for (t in theme_keep) {
  p <- add_trace(
    p,
    data        = filter(plotdata, Theme == t),
    x           = ~x_plot,          # ← use jittered dates
    y           = ~y_plot,
    type        = "scatter",
    mode        = "markers", opacity = 0.45,
    marker      = list(size = 10, color = col_pal[t]),
    name        = display_name[t],
    legendgroup = display_name[t],
    showlegend  = TRUE,
    customdata  = ~url,
    hoverinfo   = "text",
    text = ~paste0(
      "<b>Theme:</b> ", display_name[Theme],
      "<br><b>Type:</b> ",  aimType,
      "<br><b>Aim:</b> ",   Aim,
      "<br><b>Date:</b> ",  Date,
      "<br><b>DOI:</b> ",   DOI
    )
  )
}


p <- layout(
  p,
  title = "<b>Justifications for Developing Cultivated Meat Through Time</b>",
  xaxis = list(
    title = "<b>Date Created</b>",
    gridcolor = "gray85",     # light grid on white
    zerolinecolor = "gray85",
    linecolor = "black",
    tickfont  = list(color = "black"),
    titlefont = list(color = "black")
  ),
  yaxis = list(
    title = "<b>Aim of Paper</b>",
    tickmode = "array",
    tickvals = seq_along(aim_levels),
    ticktext = aim_levels,
    autorange = "reversed",
    gridcolor = "gray85",
    zerolinecolor = "gray85",
    linecolor = "black",
    tickfont  = list(color = "black"),
    titlefont = list(color = "black")
  ),
  legend = list(
    title      = list(text = "<b>Justification for<br>Developing CM</b>",
                      font = list(color = "black")),
    traceorder = "normal",
    font       = list(color = "black")
  ),
  plot_bgcolor  = "white",   # panel background
  paper_bgcolor = "white",   # entire canvas
  font          = list(color = "black")  # default text colour
)


p    # interactive plot

saveWidget(p, file = "out/plots/justificationsByAim.html")
