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
  glue,           # glue together formatted text
  ggHoriPlot,
  ggthemes
)  


# Load and wrangle data---------------------------------------------------------
my_data <- tidytuesdayR::tt_load(2025, week = 16)$daily_accidents

my_data <- my_data |>
  mutate(year = year(date), 
         date = as.Date(format(my_data$date, "2025-%m-%d")))

quantiles <- quantile(my_data$fatalities_count, probs = seq(0, 1, by = 1/5))



# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

my_pal <- rev(paletteer::paletteer_d("RColorBrewer::OrRd")[2:7])

back_colour =  "#FAFAFF"
strong_text = "black"
weak_text = lighten(strong_text, 0.1)
line_colour = weak_text

# # Fonts
# 
# # Main Font
# font_add(family = "Cabin", 
#          regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Cabin-VariableFont_wdth,wght.ttf")
# 
# 
# # Social Media Symbols
# font_add(family = "Font Awesome 6 Brands",
#          regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Brands-Regular-400.otf")
# 
# # Solid symbols
# font_add(family = "fa-solid",
#          regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Free-Solid-900.otf")
# 
# # Make the fonts work
# showtext_auto()
# 
# main_font = "Cabin"
# title_font = "Cabin"


# Save Some Stuff for the plot -------------------------------------------------

github_icon <- "&#xf09b"
github_username <- "GregoryVdvinne  "

twitter_icon <- "\uf099"
twitter_username <- "@GregoryVdvinne  "

linkedin_icon <- "\uf08c"
linkedin_username <- "Gregory Vander Vinne"



my_caption <- glue("<b>Data: </b>  Amazon's Annual Reports")

my_subtitle <- glue( "An informative subtitle ",
                    " \n ", 
                    " \n <b>Graphic: </b>", 
                    "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #3B3B3B'>{github_username}</span>   ",
                    "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #3B3B3B'>{twitter_username}  </span>   ",
                    "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #3B3B3B'>{linkedin_username}</span>")


# Record Plot Making------------------------------------------------------------
gg_record(
  dir = here("2025-04-22/recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 100
)



# The Actual Plot --------------------------------------------------------------

cutpoints  <- tibble(
  cuts = quantiles,
  names = c('ypos1', 'ypos2', 'ypos3', 'yneg1', 'yneg2', 'yneg3'), 
  color = my_pal) %>% 
  mutate(names = factor(names, rev(names))) %>% 
  arrange(names)

ori <- 142


# Plot
ggplot(my_data, aes(x = date, y = fatalities_count, fill = ..Cutpoints..)) + 
  geom_horizon(origin = ori, horizonscale = quantiles) +
  scale_fill_manual(values = my_pal) +
  facet_grid(year~.) + 
  scale_x_date(expand=c(0,0), 
               date_breaks = "1 month", 
               date_labels = "%b") +
  labs(title = "Daily Fatal Car Crashes in The USA") + 
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 
  # Theme
  # theme_minimal(base_size = 10) +
  # theme(
  #   legend.position = "bottom",
  #   panel.grid = element_blank(),
  #   panel.background = element_rect(fill = back_colour,
  #                                   color = back_colour),
  #   plot.background = element_rect(fill = back_colour,
  #                                  colour = back_colour),
  #   plot.caption.position = "plot",
  #   plot.title.position = "plot",
  #   plot.title = element_textbox_simple(size = rel(2.2),
  #                                       # family = main_font,
  #                                       face = "bold",
  #                                       color = strong_text,
  #                                       margin = margin(8, 0, 10, 0)),
  #   plot.subtitle = element_markdown(size = rel(1.1),
  #                                          # family = main_font,
  #                                          colour = weak_text,
  #                                          margin = margin(0, 0, 10, 0)),
  #   axis.title.x = element_blank(),
  #   axis.title.y = element_blank(),
  #   axis.text = element_text(),
  #   plot.caption = element_markdown(size = rel(0.8),
  #                                   colour = weak_text,
  #                                   # family = main_font,
  #                                   hjust = c(0),
  #                                   margin = margin(10,0,0,0))
  # )



# # For ggsave text sizing
# showtext_opts(dpi = 300)
# # Save plot
# ggsave(here("2025-04-22/2025-04-22.png"), height = 6, width = 8)

gg_playback(
  name = here("2025-04-22/2025-04-22_recording.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)


