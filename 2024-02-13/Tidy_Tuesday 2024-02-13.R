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
  glue           # text concatanation w/ formatting

)


######## Load and Wrangle Data ########

# Read data set directly from github
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')



######## Set Up For Plotting ########

# Save color palette

myPal <- c("#8C2F20")
myCaption <- glue("<b> Data: </b> UK Office", " ", "for National Statistics",
                  "  \n <b> Graphic: </b> Gregory Vander Vinne")

back_colour = "#817F5A"
line_colour = lighten(back_colour, 0.5)
strong_text = "#C49E77"
weak_text = lighten(strong_text, 0.2)


# #Add Roboto font family
# font_add(family = "Roboto", 
#          regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-REGULAR.ttf",
#          bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-BOLD.ttf")
# showtext_auto()

font_add_google("Roboto", "roboto")
showtext_auto()


# For centering  x axis title under main plot
my_scale <- 0.135
adjust <- ((1 / my_scale) / ((1 / my_scale) +1)) / 2

# Define Theme
my_theme <- function(base_size = 10) {
  theme_classic(base_size = base_size)+
    theme(
      axis.line = element_blank(),
      panel.grid.major.y = element_line(color = line_colour),
      axis.ticks = element_blank(),
      legend.position = c(0.1,1.025),
      legend.direction = "horizontal",
      legend.margin = margin(8, 0, 8, 0),
      legend.background = element_rect(fill = back_colour),
      panel.background = element_rect(fill = back_colour),
      plot.background = element_rect(fill = back_colour),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = rel(1.75),
                                          family = "Roboto",
                                          face = "bold",
                                          color = strong_text,
                                          margin = margin(5, 0, 10, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = "Roboto",
                                             colour = weak_text,
                                             margin = margin(0, 0, 28, 0)), 
      axis.title.x = element_text(size = rel(1.1),
                                  family = "Roboto",
                                  colour = strong_text, 
                                  margin = margin(16, 0, 4, 0), 
                                  hjust = adjust),
      axis.title.y = element_text(size = rel(1.1),
                                  family = "Roboto",
                                  colour = strong_text, 
                                  margin = margin(0, 6, 0, 2)),
      axis.text= element_text(size = rel(0.9),
                              family = "Roboto",
                              colour = weak_text),
      plot.caption = element_markdown(size = rel(0.8),
                                      colour = weak_text,
                                      family = "Roboto",
                                      hjust = 0),
      legend.text = element_text(size = rel(1),
                                 family = "Roboto",
                                 colour = strong_text),
      legend.title = element_text(size = rel(1),
                                  family = "Roboto",
                                  colour = weak_text),
      text = element_text(colour = weak_text, lineheight = 1.1)
    )
}

ggplot() + 
  my_theme()
