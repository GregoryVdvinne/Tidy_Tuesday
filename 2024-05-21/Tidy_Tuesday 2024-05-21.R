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
  ggstream,
  glue            # glue together formatted text
)  

# Load and wrangle data---------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 21)

emissions <- tuesdata$emissions

emissions_agg <- emissions %>%
  mutate(commodity = ifelse(str_detect(commodity, "Coal"), "Coal", commodity)) %>%
  group_by(year, commodity, parent_type) %>%
  summarise(emissions = sum(total_emissions_MtCO2e)) %>%
  filter(year >= 1923)


# Record Plot Making------------------------------------------------------------
gg_record(
  dir = here("2024-05-21/recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 100
)

# gg_stop_recording()


# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer::paletteer_d("calecopal::fire")

back_colour =  darken(myPal[5],0.3)
strong_text = "white"
weak_text = darken(strong_text, 0.15)

# Fonts

# Main Font
font_add(family = "Poppins", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Poppins-Regular.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Poppins-Bold.ttf"
         )


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


my_caption <- glue("<b>Data: </b> carbonmajors.org",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #FFFFFF'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #FFFFFF'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #FFFFFF'>{linkedin_username}</span>")

my_subtitle <- paste(subtitle = paste("This stream graph illustrates emissions from
                        122 of the world's largest",
                        # "<b><span style='color:", myPal[1], "'>cement,</span></b>", 
                        # "<b><span style='color:", myPal[2], "'>coal,</span></b>",
                        # "<b><span style='color:", myPal[3], "'>natural gas,</span></b>", 
                        # "and", 
                        # "<b><span style='color:", myPal[4], "'>oil</span></b>",
                        "cement, coal, gas, and oil",
                        "producers from 1923 to 2022 by parent entity type. A 
                        thicker 'stream' indicates more emissions. The large 
                        decrease in the Nation State category after 1991 is the 
                        result of the collapse of the USSR."))

text_df <- data.frame(emissions = 6250, 
                      year = 2001, 
                      parent_type = "Investor-owned Company")

# The Actual Plot --------------------------------------------------------------

ggplot(emissions_agg, aes(x = year, y = emissions, fill = commodity)) + 
  geom_stream() + 
  geom_text(data = text_df, inherit.aes = FALSE, 
            color = strong_text,
            label = paste("USSR Dissolution"),
            aes(x = year, y = emissions)) +
  geom_vline(xintercept = 1991, 
             color = weak_text) + 
  facet_wrap(parent_type ~., ncol = 1) + 
  scale_fill_manual(values = myPal, name = "") + 
  labs(title = "One Hundred Years of Emissions", 
       subtitle = my_subtitle, caption = my_caption) + 
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = back_colour,
                                    color = back_colour),
    plot.background = element_rect(fill = back_colour, 
                                   colour = back_colour),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = rel(1.75),
                                        family = title_font,
                                        face = "bold",
                                        color = strong_text,
                                        margin = margin(8, 0, 8, 4)),
    plot.subtitle = element_textbox_simple(size = rel(1.15),
                                           family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 0, 12, 4)), 
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = rel(1.1),
                               family = main_font,
                               face = "bold",
                               colour = strong_text, 
                               margin = margin(0, -8, 0, 10)),
    axis.text.y = element_blank(),
    plot.caption = element_markdown(size = rel(0.8),
                                    colour = weak_text,
                                    family = main_font,
                                    # family = "Bangers",
                                    hjust = 0.5, 
                                    margin = margin(8,0,0,8)),
    strip.text = element_text(family = main_font,
                              hjust = 0.5,
                              size = rel(1.25),
                              colour = strong_text),
    text = element_text(colour = strong_text, lineheight = 1.1)
  )


# # For ggsave text sizing
# showtext_opts(dpi = 300)
# # Save plot
# ggsave(here("2024-05-21/2024-05-21.png"), height = 6, width = 8)


gg_playback(
  name = here("2024-05-21/2024-05-21_recording.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)