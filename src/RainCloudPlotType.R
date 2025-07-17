######################################################################
##  Rain‑Cloud Plot by Aim Type — dots open DOI in new tab          ##
######################################################################

library(readr)
library(dplyr)
library(tidyr)    # <-- gives us drop_na()
library(plotly)
library(htmlwidgets)

## 1 ─ Load & wrangle -------------------------------------------------
papers <- read_csv("out/tables/paper_tbl.csv", show_col_types = FALSE)
aims   <- read_csv("data/AIM_tbl.csv",        show_col_types = FALSE)

plotdata <- papers %>%
  select(doi, dateCreated, title,
         publisher = publisherID,
         journal   = journalID,
         aimID     = aim) %>%
  filter(!aimID %in% c("Cell Differentiation", "Religion", "MIN", "Review")) %>%
  left_join(aims, by = "aimID") %>%
  select(aimType, doi, dateCreated, title, publisher, journal) %>%
  distinct() %>%
  drop_na(aimType)                # <- now works because tidyr is loaded

plotdata$aimType <- factor(plotdata$aimType,
                           levels = c("Development", "Assessment"))
plotdata$y_base  <- as.numeric(plotdata$aimType)

## 2 ─ 14‑day conditional jitter -------------------------------------
set.seed(123)
plotdata <- plotdata %>%
  mutate(date_bin = as.Date(cut(dateCreated, breaks = "14 days"))) %>%
  group_by(aimType, date_bin) %>%
  mutate(
    n_dup  = n(),
    y_plot = y_base + ifelse(n_dup > 1, runif(n(), -0.20, 0.20), 0)
  ) %>%
  ungroup()

plotdata$color <- ifelse(plotdata$aimType == "Development", "orange", "black")
plotdata$url   <- paste0("https://doi.org/", plotdata$doi)######################################################################
##  Rain‑Cloud Plot by Aim Type — dots open DOI in new tab          ##
######################################################################

library(readr)
library(dplyr)
library(tidyr)    # <-- gives us drop_na()
library(plotly)
library(htmlwidgets)

## 1 ─ Load & wrangle -------------------------------------------------
papers <- read_csv("out/tables/paper_tbl.csv", show_col_types = FALSE)
aims   <- read_csv("data/AIM_tbl.csv",        show_col_types = FALSE)

plotdata <- papers %>%
  select(doi, dateCreated, title,
         publisher = publisherID,
         journal   = journalID,
         aimID     = aim) %>%
  filter(!aimID %in% c("Cell Differentiation", "Religion", "MIN", "Review")) %>%
  left_join(aims, by = "aimID") %>%
  select(aimType, doi, dateCreated, title, publisher, journal) %>%
  distinct() %>%
  drop_na(aimType)                # <- now works because tidyr is loaded

plotdata$aimType <- factor(plotdata$aimType,
                           levels = c("Development", "Assessment"))
plotdata$y_base  <- as.numeric(plotdata$aimType)

## 2 ─ 14‑day conditional jitter -------------------------------------
set.seed(123)
plotdata <- plotdata %>%
  mutate(date_bin = as.Date(cut(dateCreated, breaks = "14 days"))) %>%
  group_by(aimType, date_bin) %>%
  mutate(
    n_dup  = n(),
    y_plot = y_base + ifelse(n_dup > 1, runif(n(), -0.20, 0.20), 0)
  ) %>%
  ungroup()

plotdata$color <- ifelse(plotdata$aimType == "Development", "orange", "black")
plotdata$url   <- paste0("https://doi.org/", plotdata$doi)

## 3 ─ Cloud width per type (√‑scaled) -------------------------------
type_counts <- plotdata %>% count(aimType, name = "n")
max_sqrt    <- sqrt(max(type_counts$n))
type_counts <- type_counts %>%
  mutate(rel_width = 0.30 + 0.50 * sqrt(n) / max_sqrt)

## 4 ─ Build plotly ---------------------------------------------------
p <- plot_ly()

for (i in seq_along(levels(plotdata$aimType))) {
  this_type <- levels(plotdata$aimType)[i]
  w         <- type_counts$rel_width[type_counts$aimType == this_type]
  
  p <- add_trace(
    p,
    data        = filter(plotdata, aimType == this_type),
    type        = "violin",
    orientation = "h",
    x           = ~dateCreated,
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

p <- add_trace(
  p,
  data  = plotdata,
  x     = ~dateCreated,
  y     = ~y_plot,
  type  = "scatter",
  mode  = "markers",
  marker = list(size = 4, color = ~color),
  text = ~paste0(
    "<b>Type:</b> ",  aimType,
    "<br><b>Title:</b> ", title,
    "<br><b>Publisher:</b> ", publisher,
    "<br><b>Journal:</b> ",  journal,
    "<br><b>Date:</b> ",     dateCreated,
    "<br><b>DOI:</b> ",      doi
  ),
  customdata = ~url,
  hoverinfo  = "text",
  showlegend = FALSE
)

p <- layout(
  p,
  title = "Papers by Aim Type through Time — click a dot to open DOI",
  xaxis = list(title = "Date Created"),
  yaxis = list(
    title    = "Aim Type",
    tickmode = "array",
    tickvals = c(1, 2),
    ticktext = levels(plotdata$aimType),
    autorange = "reversed"
  )
)

## 5 ─ JS callback to open DOI on click ------------------------------
p <- onRender(
  p,
  "
  function(el, x) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      if(url) { window.open(url); }
    });
  }
  "
)

p   # interactive plot


## 3 ─ Cloud width per type (√‑scaled) -------------------------------
type_counts <- plotdata %>% count(aimType, name = "n")
max_sqrt    <- sqrt(max(type_counts$n))
type_counts <- type_counts %>%
  mutate(rel_width = 0.30 + 0.50 * sqrt(n) / max_sqrt)

## 4 ─ Build plotly ---------------------------------------------------
p <- plot_ly()

for (i in seq_along(levels(plotdata$aimType))) {
  this_type <- levels(plotdata$aimType)[i]
  w         <- type_counts$rel_width[type_counts$aimType == this_type]
  
  p <- add_trace(
    p,
    data        = filter(plotdata, aimType == this_type),
    type        = "violin",
    orientation = "h",
    x           = ~dateCreated,
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

p <- add_trace(
  p,
  data  = plotdata,
  x     = ~dateCreated,
  y     = ~y_plot,
  type  = "scatter",
  mode  = "markers",
  marker = list(size = 4, color = ~color),
  text = ~paste0(
    "<b>Type:</b> ",  aimType,
    "<br><b>Title:</b> ", title,
    "<br><b>Publisher:</b> ", publisher,
    "<br><b>Journal:</b> ",  journal,
    "<br><b>Date:</b> ",     dateCreated,
    "<br><b>DOI:</b> ",      doi
  ),
  customdata = ~url,
  hoverinfo  = "text",
  showlegend = FALSE
)

p <- layout(
  p,
  title = "Papers by Aim Type through Time — click a dot to open DOI",
  xaxis = list(title = "Date Created"),
  yaxis = list(
    title    = "Aim Type",
    tickmode = "array",
    tickvals = c(1, 2),
    ticktext = levels(plotdata$aimType),
    autorange = "reversed"
  )
)

## 5 ─ JS callback to open DOI on click ------------------------------
p <- onRender(
  p,
  "
  function(el, x) {
    el.on('plotly_click', function(d) {
      var url = d.points[0].customdata;
      if(url) { window.open(url); }
    });
  }
  "
)

p   # interactive plot

saveWidget(p, file = "out/plots/AimType-ThruTime.html")
