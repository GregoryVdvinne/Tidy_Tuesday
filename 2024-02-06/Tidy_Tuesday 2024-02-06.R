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
  RColorBrewer,   # color palettes
  ggalluvial,     # flows between bars etc
  glue            # text concatanation w/ formatting
)


######## Load and Wrangle Data ########

# Read Tidy Tuesday data set directly from github
myData_wide <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')

# Re-shape the data
myData_long <- myData_wide %>%
  pivot_longer(cols = c(`2004`,`2022`), 
               values_to = "sites", names_to = "year")


######## The Actual Plot ########



######## Set Up For Plotting ########

# Set up a color scheme
myPal <- brewer.pal(8, "RdBu")[c(1,8)]
back_colour = lighten("#EFEFEF", 0.75)
strong_text = "black"
weak_text = lighten(strong_text, 0.2)


# Add custom font family
font_add_google("Cabin", "cabin")
showtext_auto()


# Define Theme
my_theme <- function(base_size = 24) {
  theme_classic(base_size = base_size)+
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.background = element_rect(fill = back_colour),
      plot.background = element_rect(fill = back_colour),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = rel(1.75),
                                          family = "Cabin",
                                          # face = "bold",
                                          color = strong_text,
                                          margin = margin(5, 0, 8, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = "Cabin",
                                             colour = weak_text,
                                             margin = margin(0, 0, 20, 0), 
                                             lineheight=0.4), 
      axis.title = element_blank(),
      axis.text= element_blank(),
      plot.caption = element_markdown(size = rel(0.8),
                                      colour = weak_text,
                                      family = "Cabin",
                                      hjust = 0),
      legend.text = element_text(size = rel(1),
                                 family = "Cabin",
                                 colour = strong_text),
      legend.title = element_text(size = rel(1),
                                  family = "Cabin",
                                  colour = weak_text),
      text = element_text(colour = weak_text, lineheight = 0.5)
    )
}


