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
  glue            # glue together formatted text
)  


# Load and wrangle data---------------------------------------------------------
myData<- tidytuesdayR::tt_load(2024, week = 40)$chess


# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer_d("LaCroixColoR::Orange")

back_colour =  lighten("#EFEFEF", 0.75)
strong_text = lighten("black",0.15)
weak_text = lighten(strong_text, 0.25)

# Fonts

# Main Font
font_add(family = "Cabin", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Cabin-VariableFont_wdth,wght.ttf")


# Social Media Symbols
font_add(family = "Font Awesome 6 Brands",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Brands-Regular-400.otf")

# Solid symbols
font_add(family = "fa-solid",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Free-Solid-900.otf")

# Make the fonts work
showtext_auto()

main_font = "Cabin"
title_font = "Cabin"


# Save Some Stuff for the plot -------------------------------------------------

github_icon <- "&#xf09b"
github_username <- "GregoryVdvinne  "

twitter_icon <- "\uf099"
twitter_username <- "@GregoryVdvinne  "

linkedin_icon <- "\uf08c"
linkedin_username <- "Gregory Vander Vinne"


circle <- "\uf111"


my_caption <- glue("<b>Data: </b> Kaggle ",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #3B3B3B'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #3B3B3B'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #3B3B3B'>{linkedin_username}</span>")

my_subtitle <- "A Description of the Plot"


# # Record Plot Making------------------------------------------------------------
# gg_record(
#   dir = here("2024-10-01/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )



# The Actual Plot --------------------------------------------------------------






# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-10-01/2024-10-01.png"), height = 6, width = 8)





# gg_playback(
#   name = here("2024-10-01/2024-10-01_recording.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = "white"
# )


