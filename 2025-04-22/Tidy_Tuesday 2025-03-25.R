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
  ggrepel,
  glue            # glue together formatted text
)  


# Load and wrangle data---------------------------------------------------------
my_data <- tidytuesdayR::tt_load(2025, week = 12)$report_words_clean


first_period_second_period <- my_data |>
  filter(year %in% c(2013, 2023)) |>
  mutate(first_second_period = if_else(year == 2013, "first_period", "second_period")) |>
  group_by(first_second_period, word) |>
  summarise(word_count = n()) |>
  pivot_wider(names_from = first_second_period, values_from = word_count) 

# Get total word counts for first nine and last ten reports
first_period_count <- sum(first_period_second_period$first_period, na.rm = TRUE) |>
  as.numeric()
second_period_count <- sum(first_period_second_period$second_period, na.rm = TRUE) |>
  as.numeric()
# Get ratio of total word counts in first vs second 'period'
first_second_ratio = first_period_count / second_period_count

# Data.frame for the scatterplot
scatter_data <- first_period_second_period |>
  mutate(
         # second_period = second_period * first_second_ratio, # Normalize second period to account for more words
         difference = abs(first_period - second_period)
         ) 




# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

my_pal <- paletteer::paletteer_d("ggsci::alternating_igv")

back_colour =  my_pal[1]
strong_text = "white"
weak_text = darken(strong_text, 0.1)
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
# gg_record(
#   dir = here("2025-03-25/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )



# The Actual Plot --------------------------------------------------------------


# Plot
ggplot(scatter_data, aes(x = first_period, y = second_period)) + 
  geom_point(color = my_pal[2], alpha = 0.65, size = 2.5) + 
  geom_text_repel(label = if_else(scatter_data$first_period >= 170 | scatter_data$second_period >= 170 | scatter_data$difference >= 45, scatter_data$word, ""), 
                  color = weak_text, 
                  size = 4) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = line_colour) +  # 45-degree line
  labs(title = "Comparing Word Frequencies Across Two Time Periods", 
       subtitle = "Note that the frequencies of words used in the 2014 to 2023
       period are adjusted to account for a higher total word count in that period.",
       x = "2005 to 2013 Annual Reports", y = "2014 to 2023 Annual Reports") +
  # Theme
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
    plot.title = element_textbox_simple(size = rel(2.2),
                                        # family = main_font,
                                        face = "bold",
                                        color = strong_text,
                                        margin = margin(8, 0, 10, 0)),
    plot.subtitle = element_markdown(size = rel(1.1),
                                           # family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 0, 10, 0)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    plot.caption = element_markdown(size = rel(0.8),
                                    colour = weak_text,
                                    # family = main_font,
                                    hjust = c(0),
                                    margin = margin(10,0,0,0))
  )



# # For ggsave text sizing
# showtext_opts(dpi = 300)
# # Save plot
# ggsave(here("2025-03-25/2025-03-25.png"), height = 6, width = 8)

# gg_playback(
#   name = here("2025-03-25/2025-03-25_recording.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = "white"
# )
# 

