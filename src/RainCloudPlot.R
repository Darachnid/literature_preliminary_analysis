###############################################################
##  Rain‑Cloud plot by Aim (grouped & sorted)                ##
##  • Dev aims first, then Assessment                        ##
##  • within each block: highest‑N at top                    ##
##  • 45‑day jitter, width‑scaled clouds, click‑to‑DOI       ##
###############################################################

library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

## ── 1  LOAD & TIDY ─────────────────────────────────────────
papers <- read_csv("out/tables/paper_tbl.csv", show_col_types = FALSE)
aims   <- read_csv("data/AIM_tbl.csv",        show_col_types = FALSE)

plotdata <- papers %>%
  select(doi, dateCreated, aimID = aim) %>%
  filter(!aimID %in% c("Cell Differentiation", "Religion", "MIN", "Review")) %>%
  left_join(aims, by = "aimID") %>%
  select(aimType, Aim = aimLongName, DOI = doi, Date = dateCreated) %>%
  distinct() %>%
  drop_na(aimType)

## ── 2  COUNT & SET FACTOR LEVELS  -------------------------
aim_counts <- plotdata %>%
  count(aimType, Aim, name = "n")

## order: Development (desc n)  → Assessment (desc n)
aim_levels <- aim_counts %>%
  arrange(factor(aimType, levels = c("Development", "Assessment")),
          desc(n)) %>%
  pull(Aim)

plotdata$Aim   <- factor(plotdata$Aim, levels = aim_levels)
plotdata$y_base <- as.numeric(plotdata$Aim)

## ── 3  45‑DAY CONDITIONAL JITTER  -------------------------
set.seed(123)
plotdata <- plotdata %>%
  mutate(date_bin = as.Date(cut(Date, breaks = "45 days"))) %>%
  group_by(Aim, date_bin) %>%
  mutate(
    n_dup  = n(),
    y_plot = y_base + ifelse(n_dup > 1, runif(n(), -0.20, 0.20), 0)
  ) %>%
  ungroup()

plotdata$color <- ifelse(plotdata$aimType == "Development", "orange", "black")
plotdata$url   <- paste0("https://doi.org/", plotdata$DOI)

## cloud width (√‑scaled)
max_sqrt <- sqrt(max(aim_counts$n))
aim_counts <- aim_counts %>%
  mutate(rel_width = 0.15 + 0.45 * sqrt(n) / max_sqrt)

## ── 4  BUILD PLOTLY  --------------------------------------
p <- plot_ly()

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

p <- add_trace(
  p,
  data  = plotdata,
  x     = ~Date,
  y     = ~y_plot,
  type  = "scatter",
  mode  = "markers",
  marker = list(size = 4, color = ~color),
  text = ~paste0(
    "<b>Type:</b> ",  aimType,
    "<br><b>Aim:</b> ", Aim,
    "<br><b>Date:</b> ", Date,
    "<br><b>DOI:</b> ", DOI
  ),
  customdata = ~url,
  hoverinfo  = "text",
  showlegend = FALSE
)

p <- layout(
  p,
  title = "Papers by Aim through Time — Dev aims first, Assessment last",
  xaxis = list(title = "Date Created"),
  yaxis = list(
    title    = "Aim of Paper",
    tickmode = "array",
    tickvals = seq_along(aim_levels),
    ticktext = aim_levels,
    autorange = "reversed"
  )
)

## open DOI on click
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
# ── after building p (right before onRender) ────────────────────────
caption_txt <- "Figure 2. Distribution of research papers over time, categorized by research aims. The plot illustrates that 'consumer acceptance' and 'scaffolding materials' are the most represented aims, while other categories are comparatively sparse."

p <- p |>
  layout(
    margin = list(b = 90),            # ⬅ enlarge bottom margin
    annotations = list(
      x        = 0,   y = -0.15,      # a bit below the x‑axis
      xref     = "paper", yref = "paper",
      xanchor  = "left",  yanchor = "top",
      text     = caption_txt,
      showarrow = FALSE,
      font = list(size = 12, color = "black")
    )
  )

library(htmltools)     # <- needed for tags / tagList

p <- onRender(
  p,
  "
  function(el, x){
    el.on('plotly_click', function(d){
      var url = d.points[0].customdata;
      if(url){ window.open(url); }
    });
  }"
)

# ── Caption -------------------------------------------------
caption_html <- tags$p(
  style = "font-family: sans-serif; font-size: 12px; margin: 6px 0 0 0;",
  HTML(
    "Figure&nbsp;2.&nbsp;Distribution of research papers over time, categorized by research aims. ",
    "The plot illustrates that <em>consumer acceptance</em> and <em>scaffolding materials</em> ",
    "are the most represented aims, while other categories are comparatively sparse."
  )
)

# enlarge bottom margin inside the widget so caption isn't cramped
p <- layout(p, margin = list(b = 60))

# ── Save widget + caption in one HTML file -----------------
out_path <- "out/plots/figure-2.html"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

saveWidget(
  tagList(p, caption_html),   # widget first, caption below
  file          = out_path,
  selfcontained = TRUE
)
