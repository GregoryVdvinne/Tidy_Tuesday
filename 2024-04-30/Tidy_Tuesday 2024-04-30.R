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
  ggforce,        # for hull plot in this case 
  paletteer,      # color palettes
  glue            # glue together formatted text
)  

# Load and Wrangle Data

tuesdata <- tidytuesdayR::tt_load(2024, week = 18)

wwbi_data <- tuesdata$wwbi_data
wwbi_series <- tuesdata$wwbi_series
wwbi_country <- tuesdata$wwbi_country


income_groups <- wwbi_country$income_group %>% unique()

year_summary <- wwbi_data %>%
  group_by(year) %>%
  summarise(count = n())

myData <-  left_join(wwbi_data, wwbi_series, by = "indicator_code") %>%
  mutate(indicator_name = str_to_title(indicator_name)) %>%
  select(-indicator_code) %>%
  filter(year == 2017, 
         indicator_name %in% c("Median Age Of Public Paid Employees",
                               "Median Age Of Private Paid Employees")) %>%
  pivot_wider(names_from = indicator_name, values_from = value) %>%
  left_join(wwbi_country, by = "country_code") %>%
  filter(region %in% c("Europe & Central Asia",
                       "Latin America & Caribbean"))


# Record Plot Making------------------------------------------------------------
# gg_record(
#   dir = here("2024-04-30/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )

# gg_stop_recording()


# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer_d("trekcolors::starfleet")[c(1,3)]

back_colour =  lighten(paletteer_d("trekcolors::starfleet")[2],0.95)
strong_text = lighten("black",0.05)
weak_text = lighten(strong_text, 0.2)

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


my_caption <- glue("<b>Data: </b> World Bank Data Catalog: Worldwide Bureaucracy Indicators  ",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #3B3B3B'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #3B3B3B'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #3B3B3B'>{linkedin_username}</span>")






# THe Actual Plot---------------------------------------------------------------


ggplot(data = myData, aes(x = `Median Age Of Public Paid Employees`, 
                          y = `Median Age Of Private Paid Employees`,
                          color = region, shape = region)) + 
  geom_mark_hull(aes(fill = region), alpha = 0.2) + 
  geom_point(size = 2.5, alpha = 0.8) + 
  scale_fill_manual(values = myPal, name = "") + 
  scale_color_manual(values = myPal, name ="") +
  scale_shape_manual(values = c(16, 17), name = "") + 
  labs(title = "Median Ages of Public and Private Sector Workers", 
       subtitle = paste("Workers in", 
                        "<b><span style='color:", myPal[1], "'>Europe and Central Asia </span></b>",
                        "tend to be older than workers in",
                        "<b><span style='color:", myPal[2], "'>Latin America and The Carribean.</span></b>",
                        "Public sector workers tend to be older than private sector
                        workers in both regions."),
       caption = my_caption) + 
  xlim(33,53) +
  ylim(27,47) +
  theme_classic(base_size = 9) +
  theme(
    # legend.position = c(0.1,1.025),
    # legend.direction = "horizontal",
    # legend.margin = margin(8, 0, 8, 8),
    legend.position = c(0.15,0.9),
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
    axis.title.y = element_text(size = rel(1.2),
                                family = main_font,
                                colour = strong_text,
                                margin = margin(0, 8, 0, 8)),
    axis.title.x = element_text(size = rel(1.2),
                                family = main_font,
                                colour = strong_text,
                                margin = margin(8, 0, 4, 0)),
    axis.text = element_text(size = rel(1),
                             family = main_font,
                             colour = weak_text,
                             margin = margin(4, 4, 4, 4)),
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
# ggsave(here("2024-04-30/2024-04-30.png"), height = 6, width = 8)



gg_playback(
  name = here("2024-04-30/2024-04-30_recording.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
