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
tuesdata <- tidytuesdayR::tt_load(2024, week = 35)

myData <- tuesdata$power_rangers_episodes



# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer::paletteer_d("PNWColors::Shuksan2")

back_colour =  lighten(myPal[3], 0.5)
strong_text = lighten("black",0.2)
weak_text = lighten(strong_text, 0.25)

# Fonts

# Main Font
font_add(family = "Poppins", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Poppins-Regular.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Poppins-Bold.ttf")


# Social Media Symbols
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


circle <- "\uf111"


my_caption <- glue("<b>Data: </b> Kaggle Power Ranger Dataset  ",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #3B3B3B'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #3B3B3B'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #3B3B3B'>{linkedin_username}</span>")

my_title <- glue( 
  "A ",
  "<b><span style='color:", myPal[1], "'> Clourful </span></b>",
  "<b><span style='color:", myPal[2], "'> Title </span></b>",
)

my_subtitle <- "A Description of the plot"


# # Record Plot Making------------------------------------------------------------
gg_record(
  dir = here("2024-08-27/recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 100
)

# gg_stop_recording()







# # For ggsave text sizing
# showtext_opts(dpi = 300)
# # Save plot
# ggsave(here("2024-08-27/2024-08-27.png"), height = 6, width = 8)




# 
# gg_playback(
#   name = here("2024-08-20/2024-08-20_recording.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = "white"
# )