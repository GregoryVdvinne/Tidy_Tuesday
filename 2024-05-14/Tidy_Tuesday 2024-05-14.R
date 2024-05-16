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
  paletteer,      # color palettes
  waffle,         # for geom_pictogram in this case
  glue            # glue together formatted text
)  

# Load and wrangle data --------------------------------------------------------

myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')


myData$brew %>% unique() %>% length()

myData_sum <- myData %>%
  group_by(favorite) %>%
  summarise(count = n())

total_response_count <- sum(myData_sum$count) %>% scales::comma()

myData_sum <- myData_sum %>%
  slice_max(order_by = count, n = 5) %>%
  mutate(favorite = if_else(favorite == "Regular drip coffee", "Drip Coffee", favorite), 
         favorite = factor(favorite, 
                           levels = c("Pourover", "Latte", "Drip Coffee", 
                                      "Cappuccino", "Espresso")
                           )
         ) %>%
  drop_na()

top5_response_count <- sum(myData_sum$count) %>% scales::comma()

myData_sum$count_10 <- round(myData_sum$count/10)

# Record Plot Making------------------------------------------------------------
# gg_record(
#   dir = here("2024-05-14/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )

# gg_stop_recording()


# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer::paletteer_d("rockthemes::faithnomore")

back_colour =  lighten(myPal[1],0.1)
strong_text = "black"
weak_text = lighten(strong_text, 0.2)

# Fonts

# Main Font
font_add(family = "Poppins", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Poppins-Regular.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Poppins-Bold.ttf")


# Symbols
font_add(family = "Font Awesome 6 Brands",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Brands-Regular-400.otf")

# Solid symbols
font_add(family = "fa-solid",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Free-Solid-900.otf")

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


my_caption <- glue("<b>Data: </b>  rmckeon.medium.com/great-american-coffee-taste-test-breakdown-7f3fdcc3c41d  ",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #000000'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #000000'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #000000'>{linkedin_username}</span>")

my_subtitle <- paste("In October 2023, world champion barista James Hoffmann and 
                      coffee company Cometeer held the Great American Coffee Taste Test on YouTube, 
                      during which viewers were asked to fill out a survey. One of the questions
                      asked the viewers favourite type of coffee drink. This figure shows the five
                      most common responses.", top5_response_count, "out of", total_response_count,
                     "respondents selected one of these five options. Each mug represents 10 responses.")

# The Actual Plot --------------------------------------------------------------

ggplot(data = myData_sum) + 
  geom_pictogram(aes(label = favorite, values = count_10),
                 flip = TRUE,
                 n_rows = 5,
                 size = 4,
                 color = myPal[2],
                 family = "fa-solid") +
  scale_label_pictogram(
    name = NULL,
    values = c("Pourover" = "mug-hot", "Latte" = "mug-hot", 
               "Regular drip coffee" = "mug-hot", "Cappuccino" = "mug-hot")
  ) +
  facet_wrap(~favorite, nrow = 1, strip.position = "bottom") + 
  labs(title = "Coffee Fanatics' Favourite Types of Coffee", 
       subtitle = my_subtitle, 
       caption  = my_caption) + 
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
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
                                        margin = margin(8, 0, 10, 8)),
    plot.subtitle = element_textbox_simple(size = rel(1),
                                           family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 0, 10, 8)), 
    strip.text = element_textbox_simple(size = rel(1),
                                        family = main_font,
                                        colour = weak_text,
                                        margin = margin(4, 4, 4, 4)),
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    plot.caption = element_markdown(size = rel(0.7),
                                    colour = strong_text,
                                    family = main_font,
                                    hjust = c(0), 
                                    margin = margin(5,0,5,0))
  )






# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-05-14/2024-05-14.png"), height = 6, width = 8)



gg_playback(
  name = here("2024-05-14/2024-05-14_recording.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)