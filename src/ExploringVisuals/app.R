# Fixed Shiny app for Literature Analysis Visualizations
# This app displays plots and tables created by the standalone scripts

## ---- packages ----
library(shiny)
library(bslib)      # Bootstrap 5 theming helpers
library(plotly)     # interactive charts
library(reactable)  # interactive tables
library(ggplot2)    # for static plots
library(dplyr)      # data manipulation
library(readr)      # reading CSV files
library(kableExtra) # for styled tables
library(htmltools)  # for HTML widgets
library(here)      # for robust file paths
library(tidyr)     # for drop_na and other tidyr functions
library(htmlwidgets) # for onRender used with plotly

# ---- Color Variables ----
# Aim type colors
color_development <- "orange"
color_assessment  <- "black"

# BLOC colors
color_barrier     <- "#FF0000"      # red
color_opportunity <- "#008000"      # green4
color_limitation  <- "#FFD700"      # goldenrod2
color_consequence <- "#9400D3"      # purple

# Theme colors (Justification plot)
color_human       <- "magenta"
color_livestock   <- "cyan"
color_environment <- "yellow"

# Grey violin color for barrier plot
color_violin_grey <- "rgba(160,160,160,0.25)"

## ---- create wrapper functions for each visualization ----

# Rain Cloud Plot wrapper
create_rain_cloud_plot <- function() {
  # Load data
  papers <- read_csv("paper_tbl.csv", show_col_types = FALSE)
  aims   <- read_csv("AIM_tbl.csv", show_col_types = FALSE)
  
  plotdata <- papers %>%
    select(doi, dateCreated, aimID = aim) %>%
    filter(!aimID %in% c("MIN", "Review")) %>%
    left_join(aims, by = "aimID") %>%
    select(aimType, Aim = aimLongName, DOI = doi, Date = dateCreated) %>%
    distinct() %>%
    drop_na(aimType)
  
  # Count & set factor levels
  aim_counts <- plotdata %>%
    count(aimType, Aim, name = "n")
  
  aim_levels <- aim_counts %>%
    arrange(factor(aimType, levels = c("Development", "Assessment")),
            desc(n)) %>%
    pull(Aim)
  
  plotdata$Aim   <- factor(plotdata$Aim, levels = aim_levels)
  plotdata$y_base <- as.numeric(plotdata$Aim)
  
  # 45-day conditional jitter
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
  
  # Cloud width (√-scaled)
  max_sqrt <- sqrt(max(aim_counts$n))
  aim_counts <- aim_counts %>%
    mutate(rel_width = 0.15 + 0.45 * sqrt(n) / max_sqrt)
  
  # Build plotly
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
    title = "Papers by Aim through Time — Dev aims first, Assessment last",
    xaxis = list(title = "Date Created"),
    yaxis = list(
      title    = "Aim of Paper",
      tickmode = "array",
      tickvals = seq_along(aim_levels),
      ticktext = aim_levels,
      autorange = "reversed"
    ),
    height = 600
  )
  
  # Open DOI on click
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
  
  return(p)
}

# Aims Table wrapper
create_aims_table <- function() {
  # Load data
  papers <- read_csv("paper_tbl.csv", show_col_types = FALSE)
  aims   <- read_csv("AIM_tbl.csv", show_col_types = FALSE)
  
  plotdata <- papers %>%
    select(dateCreated, aimID = aim) %>%
    filter(!aimID %in% c("MIN", "Review")) %>%
    left_join(aims, by = "aimID") %>%
    select(Aim = aimLongName, Date = dateCreated) %>%
    drop_na(Aim)
  
  # 5-year bins
  min_year <- lubridate::year(min(plotdata$Date, na.rm = TRUE))
  max_year <- lubridate::year(max(plotdata$Date, na.rm = TRUE))
  
  bin_starts <- seq(floor(min_year / 5) * 5, ceiling(max_year / 5) * 5, by = 5)
  bin_labels <- sprintf("%d–%d", bin_starts, bin_starts + 4)
  
  plotdata <- plotdata %>%
    mutate(
      BinStart = bin_starts[findInterval(lubridate::year(Date), bin_starts, left.open = FALSE)],
      BinLabel = factor(sprintf("%d–%d", BinStart, BinStart + 4), levels = bin_labels)
    )
  
  # Count papers per aim × bin
  counts_long <- plotdata %>%
    count(Aim, BinLabel, name = "n") %>%
    complete(Aim, BinLabel, fill = list(n = 0))
  
  # Order aims by total volume
  aim_levels <- counts_long %>%
    group_by(Aim) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    arrange(desc(total)) %>%
    pull(Aim)
  
  counts_long$Aim <- factor(counts_long$Aim, levels = aim_levels)
  
  counts_wide <- counts_long %>%
    pivot_wider(names_from = BinLabel, values_from = n, values_fill = 0) %>%
    arrange(match(Aim, aim_levels))
  
  # Drop bin columns with all zeros
  non_empty_cols <- counts_wide %>%
    select(-Aim) %>%
    summarise(across(everything(), sum)) %>%
    select(where(~ .x > 0)) %>%
    names()
  
  counts_wide <- counts_wide %>%
    select(Aim, all_of(non_empty_cols)) %>%
    mutate(Total = rowSums(across(where(is.numeric))))
  
  # Create reactable
  num_cols <- setdiff(names(counts_wide), "Aim")
  
  reactable(
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
}

# Theme Bar Plot wrapper
create_theme_bar_plot <- function() {
  # Load data
  df <- read_csv("theme_counts.csv") |>
    mutate(
      theme = as.character(theme),
      BLOC = as.factor(BLOC)
    )
  
  theme_order <- df |>
    arrange(desc(papers)) |>
    pull(theme)
  
  # Create ggplot
  p <- ggplot(df, aes(x = factor(theme, levels = rev(theme_order)), 
               y = papers, fill = BLOC)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(
      values = c(
        "Barrier" = color_barrier,
        "Opportunity" = color_opportunity,
        "Limitation" = color_limitation,
        "Consequence" = color_consequence
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = "Frame",
      y = "Number of Papers using the Frame",
      title = "Prevalence of Frames",
      fill = "BLOC Frame"
    )
  
  # Convert to plotly for interactivity
  ggplotly(p, tooltip = c("x", "y", "fill"))
}

# Theme Counts Table wrapper
create_theme_counts_table <- function() {
  # Load data
  theme_count <- read_csv("theme_counts.csv")
  
  # Define color palette
  bloc_colors <- c(
    "Barrier"     = "rgba(255, 0, 0, 0.3)",
    "Opportunity" = "rgba(0, 128, 0, 0.3)",
    "Limitation"  = "rgba(255, 255, 0, 0.3)",
    "Consequence" = "rgba(148, 0, 211, 0.3)"
  )
  
  # Create reactable with conditional formatting
  reactable(
    theme_count,
    defaultSorted = "papers",
    defaultSortOrder = "desc",
    searchable = TRUE,
    filterable = TRUE,
    striped = TRUE,
    highlight = TRUE,
    theme = reactableTheme(
      stripedColor = "#f7f7f7",
      highlightColor = "#ffe7ad"
    ),
    columns = list(
      BLOC = colDef(
        name = "BLOC Category",
        cell = function(value) {
          div(
            style = paste0(
              "background-color: ", bloc_colors[value], "; ",
              "padding: 4px 8px; border-radius: 4px; font-weight: bold;"
            ),
            value
          )
        }
      ),
      theme = colDef(name = "Theme"),
      papers = colDef(
        name = "Number of Papers",
        format = colFormat(separators = TRUE, digits = 0)
      )
    )
  )
}

# Rain Cloud Type Plot wrapper
create_rain_cloud_type_plot <- function() {
  # Load data
  papers <- read_csv("paper_tbl.csv", show_col_types = FALSE)
  aims   <- read_csv("AIM_tbl.csv", show_col_types = FALSE)
  
  plotdata <- papers %>%
    select(doi, dateCreated, title,
           publisher = publisherID,
           journal   = journalID,
           aimID     = aim) %>%
    filter(!aimID %in% c("MIN", "Review")) %>%
    left_join(aims, by = "aimID") %>%
    select(aimType, doi, dateCreated, title, publisher, journal) %>%
    distinct() %>%
    drop_na(aimType)
  
  plotdata$aimType <- factor(plotdata$aimType,
                             levels = c("Development", "Assessment"))
  plotdata$y_base  <- as.numeric(plotdata$aimType)
  
  # 14-day conditional jitter
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
  
  # Cloud width per type (√-scaled)
  type_counts <- plotdata %>% count(aimType, name = "n")
  max_sqrt    <- sqrt(max(type_counts$n))
  type_counts <- type_counts %>%
    mutate(rel_width = 0.30 + 0.50 * sqrt(n) / max_sqrt)
  
  # Build plotly
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
    title = "Papers by Aim Type through Time — click a dot to open DOI",
    xaxis = list(title = "Date Created"),
    yaxis = list(
      title    = "Aim Type",
      tickmode = "array",
      tickvals = c(1, 2),
      ticktext = levels(plotdata$aimType),
      autorange = "reversed"
    ),
    height = 600
  )
  
  # JS callback to open DOI on click
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
  
  return(p)
}

# Justification Plot wrapper
create_justification_plot <- function() {
  # Load data
  papers <- read_csv("paper_tbl.csv", show_col_types = FALSE)
  aims   <- read_csv("AIM_tbl.csv", show_col_types = FALSE)
  themes <- read_csv("paper_theme_tbl.csv", show_col_types = FALSE)
  
  # Preferred legend labels
  display_name <- c(
    "Humans"          = "Human Wellbeing",
    "Livestock"       = "Livestock Wellbeing",
    "the Environment" = "Environmental Wellbeing"
  )
  
  # Filter to three themes & duplicate rows
  theme_keep <- c("Humans", "Livestock", "the Environment")
  
  themes_sub <- themes %>% 
    filter(themeID %in% theme_keep) %>% 
    distinct(doi, themeID)
  
  plotdata <- papers %>% 
    select(DOI = doi, Date = dateCreated,
           aimID = aim, title, publisher = publisherID, journal = journalID) %>% 
    left_join(aims,  by = "aimID") %>% 
    left_join(themes_sub, by = c("DOI" = "doi")) %>%
    filter(!is.na(themeID)) %>%
    distinct() %>% 
    transmute(
      Aim      = aimLongName,
      aimType,
      Theme    = themeID,
      DOI, Date, title, publisher, journal
    )
  
  # Order aims: Dev-first, high->low within blocks
  aim_counts <- plotdata %>% count(aimType, Aim, name = "n")
  
  aim_levels <- aim_counts %>% 
    arrange(factor(aimType, levels = c("Development", "Assessment")),
            desc(n)) %>% 
    pull(Aim)
  
  plotdata$Aim    <- factor(plotdata$Aim, levels = aim_levels)
  plotdata$y_base <- as.numeric(plotdata$Aim)
  
  # Fast random offsets – never overlap
  stack_gap <- 0.20
  jit_days  <- 50
  set.seed(123)
  
  plotdata <- plotdata %>%
    mutate(date_bin = as.Date(cut(Date, breaks = "45 days"))) %>%
    group_by(Aim, date_bin, DOI) %>%
    arrange(Theme) %>%
    mutate(
      offset = sample(
        ((row_number()) - (n() + 1) / 2) * stack_gap
      ),
      y_plot = y_base + offset,
      x_plot = Date + runif(n(), -jit_days, jit_days)
    ) %>%
    ungroup()
  
  # Width-scaled "clouds"
  max_sqrt <- sqrt(max(aim_counts$n))
  aim_counts <- aim_counts %>% 
    mutate(rel_width = 0.15 + 0.45 * sqrt(n) / max_sqrt)
  
  # Palette & URL
  col_pal <- c("Humans" = color_human,
               "Livestock" = color_livestock,
               "the Environment" = color_environment)
  
  plotdata$color <- col_pal[plotdata$Theme]
  plotdata$url   <- paste0("https://doi.org/", plotdata$DOI)
  
  # Build plotly
  p <- plot_ly()
  
  # Clouds (violins) per Aim
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
  
  # Dots – one scatter trace per Theme
  for (t in theme_keep) {
    p <- add_trace(
      p,
      data        = filter(plotdata, Theme == t),
      x           = ~x_plot,
      y           = ~y_plot,
      type        = "scatter",
      mode        = "markers", 
      opacity     = 0.45,
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
      gridcolor = "gray85",
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
      titlefont = list(color = "black"),
      scaleanchor = "x",
      scaleratio = 1
    ),
    legend = list(
      title      = list(text = "<b>Justification for<br>Developing CM</b>",
                        font = list(color = "black")),
      traceorder = "normal",
      font       = list(color = "black")
    ),
    plot_bgcolor  = "white",
    paper_bgcolor = "white",
    font          = list(color = "black"),
    height = 600
  )
  
  return(p)
}

# Barrier Plot wrapper
create_barrier_plot <- function() {
  # Load data
  papers <- read_csv("paper_tbl.csv", show_col_types = FALSE)
  aims   <- read_csv("AIM_tbl.csv", show_col_types = FALSE)
  pt     <- read_csv("paper_theme_tbl.csv", show_col_types = FALSE)
  barriers <- read_csv("developmentBarrier_tbl.csv", show_col_types = FALSE)

  # Only keep development barriers
  dev_barriers <- barriers$developmentBarrierID
  pt_dev <- pt %>% filter(themeID %in% dev_barriers)

  # Join all info robustly
  plotdata <- pt_dev %>%
    left_join(papers, by = "doi") %>%
    left_join(aims, by = c("aim" = "aimID")) %>%
    left_join(barriers, by = c("themeID" = "developmentBarrierID")) %>%
    select(doi, dateCreated, aimType, barrier = themeID, barrierDesc = developmentBarrierDescription) %>%
    filter(!is.na(dateCreated), !is.na(barrier), !is.na(aimType)) %>%
    distinct(doi, barrier, .keep_all = TRUE)

  # Order barriers by descending count
  barrier_counts <- plotdata %>% count(barrier, name = "n")
  barrier_levels <- barrier_counts %>% arrange(desc(n)) %>% pull(barrier)
  n_barriers <- length(barrier_levels)
  grid_lines <- seq(0.5, n_barriers + 0.5, by = 1)
  plotdata$barrier <- factor(plotdata$barrier, levels = barrier_levels)
  plotdata$y_base <- as.numeric(plotdata$barrier)
  plotdata$color <- ifelse(plotdata$aimType == "Development", color_development, color_assessment)

  # Reduce jitter for more compact points
  jitter_width <- 0.25  # reduced from 0.45
  plotdata <- plotdata %>%
    mutate(date_bin = as.Date(cut(dateCreated, breaks = "30 days"))) %>%
    group_by(barrier, date_bin) %>%
    mutate(
      n_dup  = n(),
      y_plot = y_base + ifelse(n_dup > 1, runif(n(), -jitter_width, jitter_width), 0)
    ) %>%
    ungroup()

  # Initialize plotly object before adding violins
  p <- plot_ly()
  # Add grey violin for each barrier (make narrower)
  violin_width <- 0.45  # reduced from 0.7 for tighter spacing
  for (i in seq_along(barrier_levels)) {
    this_barrier <- barrier_levels[i]
    p <- add_trace(
      p,
      data = filter(plotdata, barrier == this_barrier),
      type = "violin",
      orientation = "h",
      x = ~dateCreated,
      y = i,
      width = violin_width,
      spanmode = "hard",
      points = FALSE,
      line = list(width = 0),
      fillcolor = color_violin_grey,
      hoverinfo = "skip",
      showlegend = FALSE,
      opacity = 0.5
    )
  }

  # Plot points (with legend for aimType)
  p <- add_trace(
    p,
    data = plotdata %>% filter(aimType == "Development"),
    x = ~dateCreated,
    y = ~y_plot,
    type = "scatter",
    mode = "markers",
    marker = list(size = 4, color = color_development),
    name = "Development",
    text = ~paste0(
      "<b>Barrier:</b> ", barrier,
      "<br><b>Date:</b> ", dateCreated,
      "<br><b>Aim Type:</b> ", aimType,
      "<br><b>DOI:</b> ", doi
    ),
    hoverinfo = "text",
    showlegend = TRUE
  )
  p <- add_trace(
    p,
    data = plotdata %>% filter(aimType == "Assessment"),
    x = ~dateCreated,
    y = ~y_plot,
    type = "scatter",
    mode = "markers",
    marker = list(size = 4, color = color_assessment),
    name = "Assessment",
    text = ~paste0(
      "<b>Barrier:</b> ", barrier,
      "<br><b>Date:</b> ", dateCreated,
      "<br><b>Aim Type:</b> ", aimType,
      "<br><b>DOI:</b> ", doi
    ),
    hoverinfo = "text",
    showlegend = TRUE
  )
  # Make y-axis range tighter
  p <- layout(
    p,
    title = "Barriers to Development by Time and Aim Type",
    xaxis = list(title = "Date Created"),
    yaxis = list(
      title = "Barrier to Development",
      tickmode = "array",
      tickvals = seq_along(barrier_levels),
      ticktext = barrier_levels,
      autorange = "reversed",
      range = c(0.7, n_barriers + 0.3),  # tighter than (0.5, n_barriers+0.5)
      showgrid = FALSE,
      zeroline = FALSE
    ),
    shapes = lapply(grid_lines, function(y) list(
      type = "line",
      x0 = 0, x1 = 1, xref = "paper",
      y0 = y, y1 = y, yref = "y",
      line = list(color = "#cccccc", width = 1)
    )),
    legend = list(title = list(text = "<b>Aim Type</b>")),
    height = 600
  )
  return(p)
}

# Barrier Table wrapper (percent columns)
create_barrier_table <- function() {
  pt     <- read_csv("paper_theme_tbl.csv", show_col_types = FALSE)
  papers <- read_csv("paper_tbl.csv", show_col_types = FALSE)
  aims   <- read_csv("AIM_tbl.csv", show_col_types = FALSE)
  barriers <- read_csv("developmentBarrier_tbl.csv", show_col_types = FALSE)

  dev_barriers <- barriers$developmentBarrierID
  pt_dev <- pt %>% filter(themeID %in% dev_barriers)

  joined <- pt_dev %>%
    left_join(papers, by = "doi") %>%
    left_join(aims, by = c("aim" = "aimID")) %>%
    select(doi, barrier = themeID, aimType)

  # Count by barrier and aimType
  counts <- joined %>%
    filter(!is.na(aimType)) %>%
    distinct(doi, barrier, aimType) %>%
    count(barrier, aimType, name = "n") %>%
    tidyr::pivot_wider(names_from = aimType, values_from = n, values_fill = 0)

  # Get total number of papers for each aim type
  total_dev <- papers %>% left_join(aims, by = c("aim" = "aimID")) %>% filter(aimType == "Development") %>% distinct(doi) %>% nrow()
  total_assess <- papers %>% left_join(aims, by = c("aim" = "aimID")) %>% filter(aimType == "Assessment") %>% distinct(doi) %>% nrow()

  # Calculate percent columns and append (n = N)
  total_all <- papers %>% distinct(doi) %>% nrow()
  counts <- counts %>% mutate(
    `Percent of Development Papers` = ifelse(Development > 0, paste0(round(100 * Development / total_dev, 1), "% (n = ", Development, ")"), "0.0% (n = 0)"),
    `Percent of Assessment Papers` = ifelse(Assessment > 0, paste0(round(100 * Assessment / total_assess, 1), "% (n = ", Assessment, ")"), "0.0% (n = 0)"),
    `Percent of All Papers` = ifelse((Development + Assessment) > 0, paste0(round(100 * (Development + Assessment) / total_all, 1), "% (n = ", Development + Assessment, ")"), "0.0% (n = 0)")
  )

  table <- barriers %>%
    left_join(counts, by = c("developmentBarrierID" = "barrier")) %>%
    select(Barrier = developmentBarrierID, Definition = developmentBarrierDescription, `Percent of Development Papers`, `Percent of Assessment Papers`, `Percent of All Papers`)
  table$`Percent of Development Papers`[is.na(table$`Percent of Development Papers`)] <- "0.0% (n = 0)"
  table$`Percent of Assessment Papers`[is.na(table$`Percent of Assessment Papers`)] <- "0.0% (n = 0)"
  table$`Percent of All Papers`[is.na(table$`Percent of All Papers`)] <- "0.0% (n = 0)"

  reactable(
    table,
    defaultSorted = "Percent of Development Papers",
    defaultSortOrder = "desc",
    searchable = TRUE,
    filterable = TRUE,
    striped = TRUE,
    highlight = TRUE,
    defaultPageSize = 20,
    theme = reactableTheme(
      stripedColor = "#f7f7f7",
      highlightColor = "#ffe7ad",
      style = list(fontFamily = "system-ui, sans-serif", fontSize = 12, lineHeight = "1.1", padding = "2px 4px")
    ),
    columns = list(
      Barrier = colDef(name = "Barrier", sticky = "left", minWidth = 180),
      Definition = colDef(name = "Definition", minWidth = 300),
      `Percent of Development Papers` = colDef(name = "Percent of Development Papers", align = "right", minWidth = 120),
      `Percent of Assessment Papers` = colDef(name = "Percent of Assessment Papers", align = "right", minWidth = 120),
      `Percent of All Papers` = colDef(name = "Percent of All Papers", align = "right", minWidth = 120)
    ),
    wrap = TRUE,
    width = "100%",
    style = list(height = "70vh", maxHeight = "none", minHeight = "300px", overflowY = "auto")
  )
}

# Barriers to Adoption Plot wrapper
create_adoption_barrier_plot <- function() {
  papers <- read_csv("paper_tbl.csv", show_col_types = FALSE)
  aims   <- read_csv("AIM_tbl.csv", show_col_types = FALSE)
  pt     <- read_csv("paper_theme_tbl.csv", show_col_types = FALSE)
  barriers <- read_csv("adoptionBarrier_tbl.csv", show_col_types = FALSE)

  # Only keep adoption barriers
  adoption_barriers <- barriers$adoptionBarrierID
  pt_adopt <- pt %>% filter(themeID %in% adoption_barriers)

  # Join all info robustly
  plotdata <- pt_adopt %>%
    left_join(papers, by = "doi") %>%
    left_join(aims, by = c("aim" = "aimID")) %>%
    left_join(barriers, by = c("themeID" = "adoptionBarrierID")) %>%
    select(doi, dateCreated, aimType, barrier = themeID, barrierDesc = adoptionBarrierDescription) %>%
    filter(!is.na(dateCreated), !is.na(barrier), !is.na(aimType)) %>%
    distinct(doi, barrier, .keep_all = TRUE)

  # Order barriers by descending count
  barrier_counts <- plotdata %>% count(barrier, name = "n")
  barrier_levels <- barrier_counts %>% arrange(desc(n)) %>% pull(barrier)
  n_barriers <- length(barrier_levels)
  grid_lines <- seq(0.5, n_barriers + 0.5, by = 1)
  plotdata$barrier <- factor(plotdata$barrier, levels = barrier_levels)
  plotdata$y_base <- as.numeric(plotdata$barrier)
  plotdata$color <- ifelse(plotdata$aimType == "Development", color_development, color_assessment)

  # Reduce jitter for more compact points
  jitter_width <- 0.25
  set.seed(123)
  plotdata <- plotdata %>%
    mutate(date_bin = as.Date(cut(dateCreated, breaks = "30 days"))) %>%
    group_by(barrier, date_bin) %>%
    mutate(
      n_dup  = n(),
      y_plot = y_base + ifelse(n_dup > 1, runif(n(), -jitter_width, jitter_width), 0)
    ) %>%
    ungroup()

  # Initialize plotly object before adding violins
  p <- plot_ly()
  violin_width <- 0.45
  for (i in seq_along(barrier_levels)) {
    this_barrier <- barrier_levels[i]
    p <- add_trace(
      p,
      data = filter(plotdata, barrier == this_barrier),
      type = "violin",
      orientation = "h",
      x = ~dateCreated,
      y = i,
      width = violin_width,
      spanmode = "hard",
      points = FALSE,
      line = list(width = 0),
      fillcolor = color_violin_grey,
      hoverinfo = "skip",
      showlegend = FALSE,
      opacity = 0.5
    )
  }
  # Plot points (with legend for aimType)
  p <- add_trace(
    p,
    data = plotdata %>% filter(aimType == "Development"),
    x = ~dateCreated,
    y = ~y_plot,
    type = "scatter",
    mode = "markers",
    marker = list(size = 4, color = color_development),
    name = "Development",
    text = ~paste0(
      "<b>Barrier:</b> ", barrier,
      "<br><b>Date:</b> ", dateCreated,
      "<br><b>Aim Type:</b> ", aimType,
      "<br><b>DOI:</b> ", doi
    ),
    hoverinfo = "text",
    showlegend = TRUE
  )
  p <- add_trace(
    p,
    data = plotdata %>% filter(aimType == "Assessment"),
    x = ~dateCreated,
    y = ~y_plot,
    type = "scatter",
    mode = "markers",
    marker = list(size = 4, color = color_assessment),
    name = "Assessment",
    text = ~paste0(
      "<b>Barrier:</b> ", barrier,
      "<br><b>Date:</b> ", dateCreated,
      "<br><b>Aim Type:</b> ", aimType,
      "<br><b>DOI:</b> ", doi
    ),
    hoverinfo = "text",
    showlegend = TRUE
  )
  p <- layout(
    p,
    title = "Barriers to Adoption by Time and Aim Type",
    xaxis = list(title = "Date Created"),
    yaxis = list(
      title = "Barrier to Adoption",
      tickmode = "array",
      tickvals = seq_along(barrier_levels),
      ticktext = barrier_levels,
      autorange = "reversed",
      range = c(0.7, n_barriers + 0.3),
      showgrid = FALSE,
      zeroline = FALSE
    ),
    shapes = lapply(grid_lines, function(y) list(
      type = "line",
      x0 = 0, x1 = 1, xref = "paper",
      y0 = y, y1 = y, yref = "y",
      line = list(color = "#cccccc", width = 1)
    )),
    legend = list(title = list(text = "<b>Aim Type</b>")),
    height = 600
  )
  return(p)
}

# Barriers to Adoption Table wrapper (percent columns)
create_adoption_barrier_table <- function() {
  pt     <- read_csv("paper_theme_tbl.csv", show_col_types = FALSE)
  papers <- read_csv("paper_tbl.csv", show_col_types = FALSE)
  aims   <- read_csv("AIM_tbl.csv", show_col_types = FALSE)
  barriers <- read_csv("adoptionBarrier_tbl.csv", show_col_types = FALSE)

  adoption_barriers <- barriers$adoptionBarrierID
  pt_adopt <- pt %>% filter(themeID %in% adoption_barriers)

  joined <- pt_adopt %>%
    left_join(papers, by = "doi") %>%
    left_join(aims, by = c("aim" = "aimID")) %>%
    select(doi, barrier = themeID, aimType)

  counts <- joined %>%
    filter(!is.na(aimType)) %>%
    distinct(doi, barrier, aimType) %>%
    count(barrier, aimType, name = "n") %>%
    tidyr::pivot_wider(names_from = aimType, values_from = n, values_fill = 0)

  total_dev <- papers %>% left_join(aims, by = c("aim" = "aimID")) %>% filter(aimType == "Development") %>% distinct(doi) %>% nrow()
  total_assess <- papers %>% left_join(aims, by = c("aim" = "aimID")) %>% filter(aimType == "Assessment") %>% distinct(doi) %>% nrow()
  total_all <- papers %>% distinct(doi) %>% nrow()

  counts <- counts %>% mutate(
    `Percent of Development Papers` = ifelse(Development > 0, paste0(round(100 * Development / total_dev, 1), "% (n = ", Development, ")"), "0.0% (n = 0)"),
    `Percent of Assessment Papers` = ifelse(Assessment > 0, paste0(round(100 * Assessment / total_assess, 1), "% (n = ", Assessment, ")"), "0.0% (n = 0)"),
    `Percent of All Papers` = ifelse((Development + Assessment) > 0, paste0(round(100 * (Development + Assessment) / total_all, 1), "% (n = ", Development + Assessment, ")"), "0.0% (n = 0)")
  )

  table <- barriers %>%
    left_join(counts, by = c("adoptionBarrierID" = "barrier")) %>%
    select(Barrier = adoptionBarrierID, Definition = adoptionBarrierDescription, `Percent of Development Papers`, `Percent of Assessment Papers`, `Percent of All Papers`)
  table$`Percent of Development Papers`[is.na(table$`Percent of Development Papers`)] <- "0.0% (n = 0)"
  table$`Percent of Assessment Papers`[is.na(table$`Percent of Assessment Papers`)] <- "0.0% (n = 0)"
  table$`Percent of All Papers`[is.na(table$`Percent of All Papers`)] <- "0.0% (n = 0)"

  reactable(
    table,
    defaultSorted = "Percent of Development Papers",
    defaultSortOrder = "desc",
    searchable = TRUE,
    filterable = TRUE,
    striped = TRUE,
    highlight = TRUE,
    defaultPageSize = 3,
    theme = reactableTheme(
      stripedColor = "#f7f7f7",
      highlightColor = "#ffe7ad",
      style = list(fontFamily = "system-ui, sans-serif", fontSize = 12, lineHeight = "1.1", padding = "2px 4px")
    ),
    columns = list(
      Barrier = colDef(name = "Barrier", sticky = "left", minWidth = 180),
      Definition = colDef(name = "Definition", minWidth = 300),
      `Percent of Development Papers` = colDef(name = "Percent of Development Papers", align = "right", minWidth = 120),
      `Percent of Assessment Papers` = colDef(name = "Percent of Assessment Papers", align = "right", minWidth = 120),
      `Percent of All Papers` = colDef(name = "Percent of All Papers", align = "right", minWidth = 120)
    ),
    wrap = TRUE,
    width = "100%",
    style = list(height = "70vh", maxHeight = "none", minHeight = "300px", overflowY = "auto")
  )
}

# Add a table for Papers by Aim Type
create_aim_type_table <- function() {
  papers <- read_csv("paper_tbl.csv", show_col_types = FALSE)
  aims   <- read_csv("AIM_tbl.csv", show_col_types = FALSE)

  plotdata <- papers %>%
    select(dateCreated, aimID = aim) %>%
    filter(!aimID %in% c("MIN", "Review")) %>%
    left_join(aims, by = "aimID") %>%
    select(aimType, Date = dateCreated) %>%
    drop_na(aimType)

  # 5-year bins
  min_year <- lubridate::year(min(plotdata$Date, na.rm = TRUE))
  max_year <- lubridate::year(max(plotdata$Date, na.rm = TRUE))
  bin_starts <- seq(floor(min_year / 5) * 5, ceiling(max_year / 5) * 5, by = 5)
  bin_labels <- sprintf("%d–%d", bin_starts, bin_starts + 4)

  plotdata <- plotdata %>%
    mutate(
      BinStart = bin_starts[findInterval(lubridate::year(Date), bin_starts, left.open = FALSE)],
      BinLabel = factor(sprintf("%d–%d", BinStart, BinStart + 4), levels = bin_labels)
    )

  # Count papers per aimType × bin
  counts_long <- plotdata %>%
    count(aimType, BinLabel, name = "n") %>%
    complete(aimType, BinLabel, fill = list(n = 0))

  # Order aimType
  aim_type_levels <- c("Development", "Assessment")
  counts_long$aimType <- factor(counts_long$aimType, levels = aim_type_levels)

  counts_wide <- counts_long %>%
    pivot_wider(names_from = BinLabel, values_from = n, values_fill = 0) %>%
    arrange(match(aimType, aim_type_levels))

  # Drop bin columns with all zeros
  non_empty_cols <- counts_wide %>%
    select(-aimType) %>%
    summarise(across(everything(), sum)) %>%
    select(where(~ .x > 0)) %>%
    names()

  counts_wide <- counts_wide %>%
    select(aimType, all_of(non_empty_cols)) %>%
    mutate(Total = rowSums(across(where(is.numeric))))

  num_cols <- setdiff(names(counts_wide), "aimType")

  reactable(
    counts_wide,
    defaultSorted     = "Total",
    defaultSortOrder  = "desc",
    searchable        = TRUE,
    filterable        = TRUE,
    striped           = TRUE,
    highlight         = TRUE,
    defaultPageSize   = 2,
    paginationType    = "jump",
    theme = reactableTheme(
      stripedColor   = "#f7f7f7",
      highlightColor = "#ffe7ad",
      style = list(fontFamily = "system-ui, sans-serif", fontSize = 14)
    ),
    columns = c(
      list(aimType = colDef(name = "Aim Type", sticky = "left", minWidth = 120)),
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
}

## ---- reusable plot-card module ----
plot_card_ui <- function(id, title = NULL, subtitle = NULL, n = NULL) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        fluidRow(
          column(6, style = "display: flex; align-items: center; gap: 8px;",
            if (!is.null(title) || !is.null(n)) tags$span(
              style = "font-size: 0.95em; color: #444; background: #f7f7f7; border-radius: 8px; padding: 2px 10px; margin: 0; display: flex; flex-direction: column; align-items: flex-start; gap: 0;",
              if (!is.null(title)) tags$span(title, style = "font-weight: 600; margin-bottom: 0;"),
              if (!is.null(n)) tags$span(paste0("Papers included in plot: n = ", n), style = "font-size: 0.85em; color: #888; margin-top: 0;")
            )
          ),
          column(6, style = "display: flex; align-items: center; justify-content: flex-end; gap: 8px;",
            actionButton(ns("help"), "Help", class = "btn-sm btn-danger"),
            actionButton(ns("desc"), "Description", class = "btn-sm btn-primary")
          )
        ),
        if (!is.null(subtitle)) tags$div(tags$strong(subtitle), style = "margin-top: 0.5em;")
      ),
      card_body(
        uiOutput(ns("plot"))
      )
    )
  )
}

plot_card_server <- function(id, plot_fun, caption_md, help_md, title = NULL, subtitle = NULL, n = NULL, is_reactable = FALSE) {
  moduleServer(id, function(input, output, session) {
    # Render the visual
    output$plot <- renderUI({
      if (is_reactable) {
        renderReactable({ plot_fun() })
      } else {
        renderPlotly({ plot_fun() })
      }
    })

    # Help modal
    observeEvent(input$help, {
      showModal(
        modalDialog(
          title = "How to interact",
          markdown::markdownToHTML(text = help_md, fragment.only = TRUE) |> HTML(),
          easyClose = TRUE, footer = NULL, size = "l"
        )
      )
    })
    # Description modal
    observeEvent(input$desc, {
      showModal(
        modalDialog(
          title = "Description",
          markdown::markdownToHTML(text = caption_md, fragment.only = TRUE) |> HTML(),
          easyClose = TRUE, footer = NULL, size = "l"
        )
      )
    })
  })
}

## ---- UI ----
ui <- page_navbar(
  title = "Literature Analysis Explorer",
  theme = bs_theme(version = 5, bootswatch = "minty") |>
    bs_add_variables(primary = "#006d77"),
  
  nav_panel("Papers by Aim",
    navs_tab_card(
      nav("Plot View",
        plot_card_ui("rain", title = "Papers Aims through Time", n = length(unique(read_csv("paper_tbl.csv", show_col_types = FALSE)$doi)))
      ),
      nav("Table View",
        card(
          card_body(
            reactableOutput("aims_table_table")
          )
        )
      )
    )
  ),
  nav_panel("Papers by Aim Type",
    navs_tab_card(
      nav("Plot View",
        plot_card_ui("rain_type", NULL)
      ),
      nav("Table View",
        card(
          card_body(
            reactableOutput("aim_type_table")
          )
        )
      )
    )
  ),
  nav_panel("Papers by Justification", plot_card_ui("justification", NULL)),
  nav_panel("Barriers to Development", 
    navs_tab_card(
      nav("Plot View",
        card_body(
          uiOutput("barrier_caption"),
          plotlyOutput("barrier_plot", height = "600px")
        )
      ),
      nav("Table View",
        card(
          card_body(
            reactableOutput("barrier_table")
          )
        )
      )
    )
  ),
  nav_panel("Barriers to Adoption", 
    navs_tab_card(
      nav("Plot View",
        card_body(
          uiOutput("adoption_barrier_caption"),
          plotlyOutput("adoption_barrier_plot", height = "600px")
        )
      ),
      nav("Table View",
        card(
          card_body(
            reactableOutput("adoption_barrier_table")
          )
        )
      )
    )
  )
)

## ---- server ----
server <- function(input, output, session) {
  # Aims Table
  plot_card_server(
    id = "aims_table",
    plot_fun = create_aims_table,
    caption_md = "From 2010 to 2014, only three research aims were represented, with philosophical assumptions being the most prevalent, followed by bioreactors and process design. Between 2015 and 2019, the field broadened, with consumer acceptance becoming the leading aim (11 articles), followed by education and outreach, philosophical assumptions, performance analysis, and media and nutrient sources (each with 4 articles). In the most recent period (2020–2024), scaffolding materials dominates with 114 papers, followed by consumer acceptance (78), media and nutrient sources (59), bioreactors and process design (47), and cell sources and maintenance (39). Notably, philosophical assumptions—once a leading category—has become the least represented, with just 4 papers.",
    help_md = "You can search, filter, and sort the table using the controls above each column. Use the search box to find specific aims or time periods. Click column headers to sort. Click a point to open the paper's DOI link in a new tab.",
    is_reactable = TRUE
  )
  
  # Papers by Aim
  plot_card_server(
    id = "rain",
    plot_fun = create_rain_cloud_plot,
    caption_md = "Over time, assessment-type aims such as policy and regulation and stakeholder assessment remain sparsely represented, with the notable exception of consumer acceptance, which shows steady growth. In contrast, development-type aims—particularly scaffolding materials, media and nutrient sources, and bioreactors and process design—have seen relatively equal and substantial representation from 2022 to 2024. Cell sources and maintenance appears to be an emerging focus, as shown by the widening, right-angled shape of the violin plot, indicating a sharp increase in recent attention. A similar triangular expansion is visible for scaffolding materials, though the growth in cell sources appears even more accelerated. Meanwhile, aims with more block-like shapes, such as consumer acceptance, suggest steadier but slower growth. These visual cues point to a shifting research landscape, where development-type aims are accelerating more rapidly than assessment, raising questions about what is driving this trend.",
    help_md = "Hover your mouse over the plot to make the icons appear in the top right. These let you download screenshots, pan, select points, adjust axes, and reset the view by clicking the house icon. Click a point to open the paper's DOI link in a new tab.",
  )
  
  # Papers by Aim Type
  plot_card_server(
    id = "rain_type",
    plot_fun = create_rain_cloud_type_plot,
    caption_md = "From 2010 to 2014, only one paper was published, categorized under a development-type aim. Between 2014 and 2016, two assessment-type papers emerged, marking the entry of evaluative studies into the field. During 2016–2018, eight additional papers were published, six of which were assessment-type, indicating an early dominance of that category. From 2018 to 2020, two more development-type papers appeared, while assessment continued to grow with twelve more. In the 2020–2022 period, representation between assessment and development-type aims became nearly equal. By 2022–2024, development-type aims surpassed assessment, suggesting a shifting focus toward advancing technologies and production methods in cultivated meat research.",
    help_md = "Hover your mouse over the plot to make the icons appear in the top right. These let you download screenshots, pan, select points, adjust axes, and reset the view by clicking the house icon. Click a point to open the paper's DOI link in a new tab.",
  )
  
  # Papers by Justification
  plot_card_server(
    id = "justification",
    plot_fun = create_justification_plot,
    caption_md = "This plot shows the frequency of opportunity justifications—identified through BLOC analysis—as they relate to human well-being (magenta), livestock well-being (cyan), and environmental well-being (yellow). These justifications highlight problems in current food systems that cultivated meat is proposed to solve. All three types of well-being are mentioned across most research aims, with human well-being most frequently referenced, particularly in consumer acceptance papers. Livestock well-being appears slightly less often but still spans across aims, while environmental well-being is consistently represented. Overall, the plot suggests that across aims, justifications for cultivated meat tend to emphasize a broad array of potential benefits.",
    help_md = "Hover your mouse over the plot to make the icons appear in the top right. These let you download screenshots, pan, select points, adjust axes, and reset the view by clicking the house icon. You can also click legend items to hide or show groups. Click a point to open the paper's DOI link in a new tab.",
  )

  # Barriers to Development
  output$barrier_plot <- renderPlotly({ create_barrier_plot() })
  output$barrier_caption <- renderUI({
    HTML("<b>Each point shows a paper-barrier association. Orange = Development aim, Black = Assessment aim. Y axis is the barrier to development, X is publication date.")
  })
  output$barrier_table <- renderReactable({ create_barrier_table() })

  # Barriers to Adoption
  output$adoption_barrier_plot <- renderPlotly({ create_adoption_barrier_plot() })
  output$adoption_barrier_table <- renderReactable({ create_adoption_barrier_table() })
  output$adoption_barrier_caption <- renderUI({
    HTML("<b>Each point shows a paper-barrier association for adoption. Orange = Development aim, Black = Assessment aim. Y axis is the barrier to adoption, X is publication date.")
  })

  # Add a table for Papers by Aim Type
  output$aims_table_table <- renderReactable({ create_aims_table() })
  output$aim_type_table <- renderReactable({ create_aim_type_table() })
}

## ---- app launcher ----
shinyApp(ui, server)
