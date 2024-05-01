# Setup ------------------------------------------------------------------------

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
  janitor,        # some efficient data cleaning stuff
  camcorder,      # record the making of the plot into a gif
  tidytuesdayR,   # download Tidy Tuesday Data
  glue            # glue together formatted text
)  

# Load and wrangle data --------------------------------------------------------

# Load
myData <- tidytuesdayR::tt_load(2024, week = 17)
myData <- myData$outer_space_objects

# Explore a little
myDataSum <-  myData %>%
  group_by(Entity) %>%
  summarise(num_objects = sum(num_objects)) %>%
  arrange(desc(num_objects))

# Wrangle 
myData <- myData %>%
  select(-Code) %>%
  pivot_wider(names_from = Entity, values_from = num_objects) %>%
  mutate(`Rest of World` = rowSums(., na.rm = TRUE)-`United States`-China-Russia-World-Year) %>%
  select(Year, `United States`, China, Russia, `Rest of World`) %>%
  pivot_longer(cols = c(`United States`, China, Russia, `Rest of World`), 
               names_to = "Country", values_to = "Objects") %>%
  mutate(Country = factor(Country, levels = c("United States", "China", "Russia", "Rest of World")))





# Record Plot Making------------------------------------------------------------
# gg_record(
#   dir = here("2024-04-23/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )

# gg_stop_recording()



# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer::paletteer_d("ltc::fernande")

back_colour =  "#E1F2FE"
strong_text = lighten("black",0.2)
weak_text = lighten(strong_text, 0.25)

# Fonts

# Main Font
font_add(family = "Poppins", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Poppins-Regular.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Poppins-Bold.ttf")


# Symbols
font_add(family = "Font Awesome 6 Brands",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Brands-Regular-400.otf")

# Make the fonts work
showtext_auto()

main_font = "Poppins"
title_font = "Poppins"



# Save Some Stuff for the plot -------------------------------------------------

github_icon <- "&#xf09b"
github_username <- "GregoryVdvinne  "

twitter_icon <- "\uf099"
twitter_username <- "@GregoryVdvinne  "

linkedin_icon <- "\uf08c"
linkedin_username <- "Gregory Vander Vinne"


my_caption <- glue("<b>Data: </b> UN Office for Outer Space Affairs: Online index of objects launched into outer space  ",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #3B3B3B'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #3B3B3B'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #3B3B3B'>{linkedin_username}</span>")



# The Actual Plot --------------------------------------------------------------

ggplot() + 
  geom_area(data = myData, aes(x = Year, y = Objects, 
                               fill = Country, color = Country), 
            alpha = 0.8) +
  scale_fill_manual(values = myPal, name = "") + 
  scale_colour_manual(values = myPal, name = "") + 
  labs(title = "An Astronomical Number of Objects in Outer Space", 
       subtitle = "This stacked area chart illustrates the number of objects
       launched into outer space each year from 1957 to 2021 by country according
       to UN estimates that cover about 88% of objects launched. There is some
       double-counting, as objects launched jointly by several countries are 
       recorded once for each country.", 
       caption = my_caption) + 
  ylab("Objects Launched Into Space") + 
  theme_classic(base_size = 9) +
  theme(
    legend.position = c(0.25, 0.75),
    legend.background = element_rect(fill = back_colour),
    panel.background = element_rect(fill = back_colour,
                                    color = back_colour),
    plot.background = element_rect(fill = back_colour, 
                                   colour = back_colour),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = rel(2),
                                        family = title_font,
                                        face = "bold",
                                        color = strong_text,
                                        margin = margin(8, 0, 12, 8)),
    plot.subtitle = element_textbox_simple(size = rel(1.1),
                                           family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 0, 12, 8)),
    axis.line = element_line(color = weak_text), 
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = rel(1.1),
                                family = main_font,
                                colour = weak_text,
                                margin = margin(0, 6, 0, 8)),
    axis.text = element_text(size = rel(1),
                               family = main_font,
                               colour = weak_text,
                               margin = margin(0, 2, 0, 0)),
    plot.caption = element_markdown(size = rel(0.8),
                                    colour = weak_text,
                                    family = main_font,
                                    hjust = c(0), 
                                    margin = margin(8,0,0,8)),
    text = element_text(colour = weak_text, lineheight = 1.1)
  )

  


# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-04-23/2024-04-23.png"), height = 6, width = 8)



gg_playback(
  name = here("2024-04-23/2024-04-23_recording.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
