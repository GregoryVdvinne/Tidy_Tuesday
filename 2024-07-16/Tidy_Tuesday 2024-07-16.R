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
  ggrain,         # Raincloud plots in R
  glue            # glue together formatted text
)  

# Load and wrangle data---------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 29)

myData <- tuesdata$ewf_matches |>
  filter(season %in% c("2021-2022", "2022-2023", "2023-2024"), 
         division %in% c("Women's Super League (WSL)", "FA Women's Super League (WSL)"))
  
# Check
myData |>
  group_by(season) |>
  summarise(med_atten = median(attendance))



# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer::paletteer_d("ltc::trio1")

back_colour =  "White" 
strong_text = lighten("Black", 0.2)
weak_text = lighten(strong_text, 0.1)

# Fonts

# Main Font
font_add(family = "Roboto", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Roboto-Regular.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Roboto-Bold.ttf")


# Symbols
font_add(family = "Font Awesome 6 Brands",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Brands-Regular-400.otf")

# Solid symbols
font_add(family = "fa-solid",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Free-Solid-900.otf")

# Make the fonts work
showtext_auto()

main_font = "Roboto"


# Save Some Stuff for the plot -------------------------------------------------

github_icon <- "&#xf09b"
github_username <- "GregoryVdvinne  "

twitter_icon <- "\uf099"
twitter_username <- "@GregoryVdvinne  "

linkedin_icon <- "\uf08c"
linkedin_username <- "Gregory Vander Vinne"


my_caption <- glue("<b>Data: </b> The English Women's Football Database",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #000000'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #000000'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #000000'>{linkedin_username}</span>")

my_subtitle <- paste(subtitle = "This plot shows the distribution of attendance at
                     matches in the Women's Super League in England over the 
                     past three seasons. Note that the y-axis is log-scaled, and
                     while the two most attended games had an attendance over 60,000, 
                     median attendance remained below 3,500 in 2023-24.")


# Record Plot Making------------------------------------------------------------
gg_record(
  dir = here("2024-07-16/recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 100
)

# gg_stop_recording()


# The Actual Plot --------------------------------------------------------------

ggplot(myData, aes(y = attendance, x = season, color = season, fill = season)) + 
  geom_rain(alpha = 0.4) + 
  scale_y_log10(label = scales::comma, 
                breaks = c(500, 5000, 50000)) + 
  labs(title = "Attendance of the Women's Super League is Growing", 
       subtitle = my_subtitle,
       y = "Match Attendance", 
       caption = my_caption) +
  scale_color_manual(values = myPal) + 
  scale_fill_manual(values = myPal) + 
  theme_classic(base_size = 10) +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = back_colour,
                                      color = back_colour),
      plot.background = element_rect(fill = back_colour, 
                                     colour = back_colour),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = rel(2),
                                          family = main_font,
                                          face = "bold",
                                          color = strong_text,
                                          margin = margin(8, 0, 10, 8)),
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
                                  margin = margin(0, 8, 0, 8)),
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
  



# # For ggsave text sizing
# showtext_opts(dpi = 300)
# # Save plot
# ggsave(here("2024-07-16/2024-07-16.png"), height = 6, width = 8)


gg_playback(
  name = here("2024-07-16/2024-07-16_recording.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
