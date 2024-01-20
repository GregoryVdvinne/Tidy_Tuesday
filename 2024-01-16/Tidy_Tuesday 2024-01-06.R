# Clear memory
rm(list = ls(all=T))


# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  colorspace      # fancy stuff with colors  
)


######## Load and Wrangle Data ########

# Read data sets directly from github



######## Set Up For Plotting ########

# Save color palette that looks like RStudio's cobalt

myPal <- c("#D48902", "#002141", "#D2D8DD", "#1FA40E", "#A44A70", "#006BD1")

