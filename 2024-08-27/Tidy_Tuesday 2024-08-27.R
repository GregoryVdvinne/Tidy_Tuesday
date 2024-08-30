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

# Little bit of cleansing
myData <- myData |>
  group_by(season_title) |>
  mutate(min_date = min(air_date)) |>
  ungroup() |>
  mutate(season_number =  as.numeric(factor(min_date))) |>
  filter(episode_num > 0) # Remove "lost" version of premiere episode


# Median Rating by Season
med_ratings <- myData |> 
  group_by(season_number) |>
  summarise(median_rating = median(IMDB_rating))
  
highest_rating <-  max(med_ratings$ median_rating)

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

my_subtitle <- "Points represent individual episodes"

label_one <- paste0("The final season's \n",
                    "episodes had the highest  \n", 
                    "median IMDB rating at 8.9.")

label_two <- paste0("The first season had a",
                    "\n whopping 60 episodes.")


# # Record Plot Making------------------------------------------------------------
# gg_record(
#   dir = here("2024-08-27/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )

# gg_stop_recording()

ggplot() + 
  geom_point(data = myData, aes(x = season_number, y = episode_num, 
                          color = IMDB_rating), 
              size = 1.25) +
  geom_label(data = myData,
             x = 17, 
             y = -40, 
             label = label_one, 
             family = main_font,
             colour = weak_text,
             size = 3.25,
             lineheight = 1, 
             hjust = 0) +
  geom_curve(data = data.frame(x1 = 24.5,
                               x2 = 27,
                               y1 = 40,
                               y2 = 23),
             aes(x = x1, xend = x2,
                 y = y1, yend = y2),
             color = weak_text,
             curvature = 0.5,
             angle = 40,
             arrow = arrow(length = unit(0.03, "npc")),
             linewidth = 0.5) +
  geom_label(data = myData,
             x = 9, 
             y = -51.5, 
             label = label_two, 
             family = main_font,
             colour = weak_text,
             size = 3.25,
             lineheight = 1, 
             hjust = 0) +
  geom_curve(data = data.frame(x1 = 12.25,
                               x2 = 1.5,
                               y1 = 55.5,
                               y2 = 59.9),
             aes(x = x1, xend = x2,
                 y = y1, yend = y2),
             color = weak_text,
             curvature = -0.15,
             angle = 40,
             arrow = arrow(length = unit(0.03, "npc")),
             linewidth = 0.5) +

  scale_color_gradientn(colours = myPal,
                       name = paste0("IMDB", "\n", "Rating")) +
  scale_y_reverse(limits = c(max(myData$episode_num), 1),
                  breaks = c(1, 20, 40, 60)) +
  scale_x_continuous(limits = c(1,max(myData$season_number)),
                     breaks = c(1, 5, 10, 15, 20, 25)) +
  labs(title = "IMDB Ratings of Power Rangers Episodes", 
       x = "Season", 
       y = "Episode of Season", 
       subtitle = my_subtitle, 
       caption = my_caption) + 
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = back_colour,
                                    color = back_colour),
    plot.background = element_rect(fill = back_colour, 
                                   colour = back_colour),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = rel(2.5),
                                        family = main_font,
                                        color = strong_text,
                                        margin = margin(4, 0, 10, 4)),
    plot.subtitle = element_textbox_simple(size = rel(1.25),
                                           family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 4, 6, 4)), 
    axis.title.y = element_text(size = rel(1.2),
                                family = main_font,
                                colour = strong_text, 
                                margin = margin(0, 6 , 0, 4)),
    axis.title.x = element_text(size = rel(1.2),
                                family = main_font,
                                colour = strong_text, 
                                margin = margin(6, 0 , 2, 0)),
    axis.text = element_text(size = rel(1.1),
                             family = main_font,
                             colour = weak_text, 
                             margin = margin(0, 0, 0, 6)),
    plot.caption = element_textbox_simple(size = rel(0.8),
                                          colour = weak_text,
                                          family = main_font,
                                          hjust = 0.5, # Seems to be ignored
                                          margin = margin(4,0,2,4)),
    legend.title = element_text(size = rel(1),
                                family = main_font,
                                colour = strong_text), 
    legend.text = element_text(size = rel(0.9),
                               family = main_font,
                               colour = weak_text)
  )








# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-08-27/2024-08-27.png"), height = 6, width = 8)





# gg_playback(
#   name = here("2024-08-27/2024-08-27_recording.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = "white"
# )