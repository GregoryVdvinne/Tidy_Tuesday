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
  ggmosaic,       # mosaic plot
  glue            # glue together formatted text
)  

# Load and Wrangle Data

tuesdata <- tidytuesdayR::tt_load(2024, week = 19)

myData <- tuesdata$rolling_stone %>%
  mutate(decade = case_when(
      release_year %in% 1950:1959 ~ "1950s", 
      release_year %in% 1960:1969 ~ "1960s",
      release_year %in% 1970:1979 ~ "1970s",
      release_year %in% 1980:1989 ~ "1980s",
      release_year %in% 1990:1999 ~ "1990s",
      release_year %in% 2000:2009 ~ "2000s",
      release_year %in% 2010:2019 ~ "2010s", 
      TRUE ~ "?"
    )
  ) %>%
  select(decade, artist_gender) %>%
  drop_na()


# Record Plot Making------------------------------------------------------------
# gg_record(
#   dir = here("2024-05-07/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )

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

ggplot(data = myData ) + 
  geom_mosaic(aes(x = product(decade), fill = artist_gender)) + 
  # geom_mosaic_text(aes(x = product(decade), fill = artist_gender)) + 
  labs(title = "Rolling Stone's Top 500 Albums of All Time by Decade and Artist Gender", 
       subtitle = paste("This mosaic plot shows that the 1970s account for a
                        greater share of the Rolling Stone's top 500 albums than
                        any other decade. Furthermore,", 
                        "<b><span style='color:", myPal[1], "'>solo female artists and all-female groups</span></b>",
                        "have surged in popularity, while",
                        "<b><span style='color:", myPal[3], "'>bands with both males and females</span></b>",
                        "have ceased to be as popular in the past two decades.",
                        "<b><span style='color:", myPal[2], "'>Male artists and all-male groups</span></b>",
                        "have slowly accounted for a smaller share over the decades."),
       captions = my_caption) + 
  scale_fill_manual(values = myPal) +
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
                                        margin = margin(8, 0, 12, 8)),
    plot.subtitle = element_textbox_simple(size = rel(1),
                                           family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 0, 12, 8)), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = rel(1),
                             family = main_font,
                             colour = strong_text,
                             face = "bold",
                             margin = margin(0, -10, 0, 16)),
    axis.text.x = element_text(size = rel(1),
                               family = main_font,
                               colour = strong_text,
                               face = "bold",
                               margin = margin(-5,0,5,0)),
    plot.caption = element_markdown(size = rel(0.7),
                                    colour = weak_text,
                                    family = main_font,
                                    hjust = c(0), 
                                    margin = margin(5,0,5,0)),
    text = element_text(colour = weak_text, lineheight = 1.1)
  )







# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-05-07/2024-05-07.png"), height = 6, width = 8)



# gg_playback(
#   name = here("2024-05-07/2024-05-07_recording.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = "white"
# )


