# =============================================================================
# Master script: Build complete relational database and visualizations
#
# This script sequentially sources individual R scripts required to process
# coded qualitative data, construct relational tables, count theme frequencies,
# and generate plots.
#
# INPUT:
#  - Scripts in `src/`: cleaner.R, databaseSetup.R, themeCounter.R, 
#    countPlotter.R, themeBarPlotter.R
#
# OUTPUT:
#  - Cleaned and normalized data
#  - Relational CSV tables
#  - Theme count summaries
#  - Plots of theme prevalence
# =============================================================================


# ---- Source scripts in order --------------------------------------------- #

source(file = "src/cleaner.R")            # Prepare and normalize coded data
source(file = "src/databaseSetup.R")      # Build relational database tables
source(file = "src/themeCounter.R")       # Count theme-paper associations
source(file = "src/countPlotter.R")       # Plot top-level BLOC category counts
source(file = "src/themeBarPlotter.R")    # Visualize frame use by theme

# ---- Optional workspace clearing ------------------------------------------ #

rm(list = ls())
