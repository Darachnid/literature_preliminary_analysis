# =============================================================================
# Plot bar chart of theme prevalence by BLOC category
#
# This script generates a horizontal bar chart showing the number of papers
# associated with each theme, grouped and colored by BLOC category.
#
# INPUT:
#  - df: a data frame with columns 'theme', 'papers', and 'BLOC'
#
# OUTPUT:
#  - Horizontal bar chart showing theme frequency by BLOC
# =============================================================================


# ---- Load necessary packages ---------------------------------------------- #

library(ggplot2)
library(dplyr)


# ---- Format variables for plotting ---------------------------------------- #

df <- read_csv(file = "out/tables/theme_counts.csv") |>
  mutate(
    theme = as.character(theme),
    BLOC = as.factor(BLOC)
  )

theme_order <- df |>
  arrange(desc(papers)) |>
  pull(theme)


# ---- Plot theme frequencies by BLOC -------------------------------------- #

ggplot(df, aes(x = factor(theme, levels = rev(theme_order)), 
               y = papers, fill = BLOC)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Barrier" = "red",
      "Opportunity" = "green4",
      "Limitation" = "goldenrod2",
      "Consequence" = "purple"
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
