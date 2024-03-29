# Clear memory
rm(list = ls(all=T))


# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  fishualize,     # color palettes like fish
  ggmosaic,       # Mosaic plots in R
  colorspace      # fancy stuff with colors 
)


######## Load and Wrangle Data ########

# Read data set(s) directly from github
myDataWide <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')

# Some Cleaning
myDataLong <-  myDataWide %>%
  select(Name, PlasticBottles:SportsBalls) %>%
  pivot_longer(cols = PlasticBottles:SportsBalls,
               values_to = "pieces", names_to = "type") %>%   
  drop_na() %>%
  mutate(type = snakecase::to_title_case(type))

# Aggregation
data_sum <- myDataLong %>%
  group_by(Name, type) %>%
  summarise(count = n(), pieces = sum(pieces)) %>%
  mutate(avg_pieces = pieces / count) %>%
  filter(!type %in% c("Glass Bottles", "Sports Balls"))


######## Set Up Some Aesthetic Elements ########

## Save color palette

myPal <- fish(n = 5, option = "Antennarius_commerson")[c(1,3:5)]

back_colour =  fish(n = 5, option = "Antennarius_commerson")[2] %>%
  darken(0.4)
strong_text = "#EFEFEF"
weak_text = darken(strong_text, 0.1)


## Fonts

# Title font
font_add_google("Roboto Slab", "roboto_slab")
font_add_google("Roboto", "roboto")

# Make the fonts work
showtext_auto()


## Define Theme
my_theme <- function(base_size = 10) {
  theme_mosaic(base_size = base_size)+
    theme(
      panel.background = element_rect(fill = back_colour,
                                      color = back_colour),
      plot.background = element_rect(fill = back_colour, 
                                     colour = back_colour),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = rel(2.25),
                                          family = "Roboto Slab",
                                          face = "bold",
                                          color = strong_text,
                                          margin = margin(8, 0, 12, 8)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = "Roboto",
                                             colour = weak_text,
                                             margin = margin(8, 0, 12, 8)), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = rel(1.1),
                                 family = "Roboto",
                                 colour = weak_text, 
                                 margin = margin(0,0,0,12)),
      axis.text.x = element_text(size = rel(1.1),
                                 family = "Roboto",
                                 colour = weak_text, 
                                 angle = 45),
      plot.caption = element_markdown(size = rel(0.8),
                                      colour = weak_text,
                                      family = "Roboto",
                                      hjust = 0, 
                                      margin = margin(-10,0,0,12)),
      text = element_text(colour = weak_text, lineheight = 1.1)
    )
}


## The Actual Plot
p <- ggplot(data = data_sum) + 
  geom_mosaic(
              aes(weight = avg_pieces, x = product(type), fill = Name), 
              show.legend = FALSE) +
  # geom_mosaic_text(aes(x = product(type), fill = Name)) + 
  scale_fill_manual(values = myPal) + 
  labs(title = "An Average Month of Scooping Up Trash", 
       subtitle = paste("Trash wheels are semi-autonomous devices used to gobble
                        up trash at the ends of rivers, streams and other outfalls.
                        The City of Baltimore has four such devices collecting
                        garbage from the city's waterways. Their names are", 
                        "<b><span style='color:", myPal[4], "'>Professor Trash Wheel</span></b>,",
                        "<b><span style='color:", myPal[3], "'>Mister Trash Wheel</span></b>,",
                        "<b><span style='color:", myPal[2], "'>Gwynnda The Trash Wheel</span></b>,",
                        "and <b><span style='color:", myPal[1], "'>Captain Trash Wheel</span></b>. 
                        This mosaic plot illustrates how many pieces of each of 
                        the five most common types of trash each wheel collects 
                        in an average month."),
       caption =  paste("<b> Data: </b> mrtrashwheel.com",
                       "  \n <b> Graphic: </b> Gregory Vander Vinne")
       ) + 
  my_theme()

# The label function built into ggmosaic was not working
  # May have to do with data structure
p + geom_text(
  data = ggplot_build(p)$data %>% as.data.frame() %>% filter(.wt > 0),
  aes(
    x = (xmin + xmax) / 2,
    y = (ymin + ymax) / 2,
    label = round(.wt) %>% scales::comma()
  ), 
  family = "Roboto", 
  size = 3, 
  color = darken(back_colour,0.1), 
  fontface = "bold"
)

## Save the plot

# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-03-05/2024-03-05.png"))
