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
  sf,             # GIS stuff
  RColorBrewer,   # color palettes
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
borders <- rnaturalearth::ne_states(c("united states of america", "canada")) %>%
  filter(!(name %in% c("Hawaii", "Alaska", "Northwest Territories", "Nunavut", 
                       "Yukon")))

#Filter to just Punxsutawney Phil
phil <- myData %>%
  filter(name == "Punxsutawney Phil")


######## Set Up For Plotting ########

# Set up a color scheme
myPal <- brewer.pal(8, "RdBu")[c(1,8)]
back_colour = lighten("#f2e86d", 0.95)
strong_text = darken("#696047",0.75)
weak_text = lighten(strong_text, 0.2)


# Add custom font family
font_add_google("Cabin", "cabin")
showtext_auto()


# Define Theme
my_theme <- function(base_size = 10) {
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
                                          margin = margin(5, 0, 10, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = "Cabin",
                                             colour = weak_text,
                                             margin = margin(0, 0, 28, 0)), 
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
      text = element_text(colour = weak_text, lineheight = 1.1)
    )
}


######## The Actual Plot ########

#Define some of my text
myCaption <- glue("<b> Data: </b> Groundhog-Day.com",
                  "  \n <b> Graphic: </b> Gregory Vander Vinne")

myAnnotation <- paste0(phil$name, " has made ", 
                      "\n",
                      phil$predictions, " predictions - the most of any",
                      "\n",
                      " groundhog. He predicts a longer", 
                      "\n",
                      " winter ", round(phil$share_shadow*100,1), "% of the time.")

#Plot
ggplot() + 
  geom_sf(data = borders, fill = back_colour, color = "grey70") + 
  geom_point(data = myData, aes(x = longitude, y = latitude, 
                                color = share_shadow, size = predictions), 
             alpha = 0.75) + 
  geom_text(
    data = data.frame(
      x = -67,
      y = 36,
      label = c(myAnnotation)
    ),
    mapping = aes(x = x, y = y, label = myAnnotation),
    family = "Cabin",
    colour = weak_text,
    size = 3,
    lineheight = 0.85
  ) +
  geom_curve(
    data = data.frame(x2 = phil$longitude,
                      x1 = -73,
                      y2 = phil$latitude-0.4,
                      y1 = 37),
    mapping = aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.03, "npc")),
    colour = lighten(weak_text,0.3),
    curvature = -0.2
  ) +
  labs(title = paste0("Which Groundhogs Tend to Predict ", 
                     "<span style='color:", myPal[1], "'>More Winter</span>", 
                     " Versus an ", 
                     "<span style='color:", myPal[2], "'>Early Spring</span>", "?"), 
       subtitle = "On this map, each dot represents a groundhog that makes predictions on Groundhog Day. 
       Bigger dots represent groundhogs that have been making predictions for more years. 
       Dots that are more red represent groundhogs that predict a longer winter a higher share of the time.
       Dots that are more blue represent groundhogs that predict an early spring a higher share of the time. 
       A completely grey dot would represent a groundhog that predicts each outcome evenly.", 
       caption = myCaption) + 
  scale_y_continuous(limits = c(30,52)) +
  scale_x_continuous(limits = c(-122, -63)) +
  scale_color_gradient2(low = myPal[2],  high = myPal[1],
                       midpoint = 0.5, mid = "grey45") + 
  my_theme()


