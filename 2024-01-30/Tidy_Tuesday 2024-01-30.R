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
  rnaturalearth,  # to get province/state borders for Canada/USA
  glue            # text concatanation w/ formatting
)


######## Load and Wrangle Data ########

# Read Tidy Tuesday data set directly from github
groundhogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv')
predictions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv')


# Summarise predictions by groundhog
myData <- predictions %>%
  select(-details) %>%
  drop_na() %>%
  group_by(id) %>%
  summarise(predictions = n(), 
            share_shadow = sum(shadow)/n(), 
            latest = max(year)) %>%
  # To get their names
  left_join(groundhogs, by = "id")

# Get borders of Canada and the USA
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada")) %>%
  filter(!(name %in% c("Hawaii", "Alaska", "Northwest Territories", "Nunavut", 
                       "Yukon")))

ggplot() + 
  geom_sf(state_prov) + 
  geom_point(myData, aes(x = longitude, y = latitude))

######## Set Up For Plotting ########

# Set up a color scheme
myPal <- c("#BF40BF", "grey")
myCaption <- glue("<b> Data: </b> Groundhog-Day.com API",
                  "  \n <b> Graphic: </b> Gregory Vander Vinne")

back_colour = lighten("#EFEFEF",0.05)
line_colour = "grey20"
strong_text = "black"
weak_text = lighten(strong_text, 0.2)


# Add custom font family
font_add_google("Roboto", "roboto")
showtext_auto()


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


######## The Actual Plot ########


