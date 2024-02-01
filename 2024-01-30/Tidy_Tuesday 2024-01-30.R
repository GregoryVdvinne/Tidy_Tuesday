# Clear memory
rm(list = ls(all=T))


# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  colorspace,     # fancy stuff with colors  
  ggbeeswarm,     # beeswarm plot
  glue,           # text concatanation w/ formatting
  ggside          # plots in margins of main plot 
)


######## Load and Wrangle Data ########

# Read data set directly from github
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')
