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

# Load and wrangle data --------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, week = 20)



# Record Plot Making------------------------------------------------------------
gg_record(
  dir = here("2024-05-14/recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 100
)

# gg_stop_recording()


# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer::paletteer_d("MoMAColors::Fritsch")[c(2,1,4)]

back_colour =  paletteer::paletteer_d("MoMAColors::Fritsch")[3]
strong_text = "white"
weak_text = darken(strong_text, 0.2)

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


my_caption <- glue("<b>Data: </b> Rolling Stone 500  ",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #c6c6c6'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #c6c6c6'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #c6c6c6'>{linkedin_username}</span>")


# The Actual Plot --------------------------------------------------------------

ggplot(data = myData )





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