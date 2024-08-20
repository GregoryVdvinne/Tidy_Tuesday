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
  ggpointdensity, # point density plots
  gganimate,      # animated ggplots
  ggflags,        # Flags in ggplots
  countrycode,    # get two digit codes for countries to match w/ ggflags
  glue            # glue together formatted text
)  

# Load and wrangle data---------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 34)

myData <- tuesdata$english_monarchs_marriages_df |>
  mutate(
    year_of_marriage = as.numeric(year_of_marriage),
    king_age = as.numeric(king_age), 
    consort_age = as.numeric(consort_age),
    age_difference = king_age - consort_age
    ) |>
  filter(year_of_marriage >= 1100)
  


ggplot(myData) + 
  geom_point(aes(x = year_of_marriage, y =  consort_age), 
             color = "blue")

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


my_caption <- glue("<b>Data: </b> IanVisits List of Monarchs by Marriage  ",
                " \n <b>Graphic: </b>",
                "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #3B3B3B'>{github_username}</span>   ",
                "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #3B3B3B'>{twitter_username}  </span>   ",
                "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #3B3B3B'>{linkedin_username}</span>")

my_title <- glue( 
  "The Ages of ",
  "<b><span style='color:", myPal[1], "'> English Rulers </span></b>",
  "<span style='font-family:\"fa-solid\"; color: #5D74A5FF;'>{circle};</span>", # Circle
  " and ", 
  "<b><span style='color:", myPal[5], "'> Their Spouses </span></b>",
  "<span style='font-family:\"fa-solid\"; color: #A8554EFF ;'>{circle};</span>", # Circle
  )

my_subtitle <- "Each point on this plot represents either an English monarch or 
their spouse, and shows their ages and the year when they got married. The lines connect points
representing two people who got married and the lenght of a line shows a couple's
age gap."


# # Record Plot Making------------------------------------------------------------
# gg_record(
#   dir = here("2024-08-20/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )

# gg_stop_recording()





ggplot(myData) + 
  geom_segment(aes(x = year_of_marriage, y = king_age, yend = consort_age), 
               color = "grey30") +
  geom_point(aes(x = year_of_marriage, y =  consort_age), 
             color = myPal[5], alpha = 1, size = 2.25) + 
  geom_point(aes(x = year_of_marriage, y =  king_age), 
             color = myPal[1], alpha = 1, size = 2.25) +
  theme_classic(base_size = 9) +
  labs(title = my_title,
       y = "Age", 
       x = "Year",
       caption = my_caption, 
       subtitle = my_subtitle) +
  theme(
    legend.position = "top",
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
                                        margin = margin(8, 0, 8, 8)),
    plot.subtitle = element_textbox_simple(size = rel(1.1),
                                           family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 0, 12, 8)),
    axis.line = element_line(color = weak_text), 
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = rel(1.1),
                                family = main_font,
                                colour = weak_text,
                                margin = margin(8, 0, 0, 0)),
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
                                    margin = margin(8,0,0,8))
  )




# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-08-20/2024-08-20.png"), height = 6, width = 8)




# 
# gg_playback(
#   name = here("2024-08-20/2024-08-20_recording.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = "white"
# )
