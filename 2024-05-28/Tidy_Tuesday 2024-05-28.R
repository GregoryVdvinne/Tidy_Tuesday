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
  treemapify,     # Treemap
  glue            # glue together formatted text
)  

# Load and wrangle data---------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2024, week = 22)

myData <- tuesdata$planting_2021 %>%
  mutate(vegetable = str_to_title(vegetable),
         variety = str_to_title(variety))

topVeg <- myData %>% 
  group_by(vegetable) %>%
  summarise(seeds = sum(number_seeds_planted)) %>%
  arrange(desc(seeds)) %>%
  slice_max(order_by = seeds, n = 6)

myData <- myData %>% 
  group_by(vegetable, variety) %>%
  filter(vegetable %in% topVeg$vegetable) %>%
  summarise(seeds = sum(number_seeds_planted)) %>%
  group_by(vegetable) %>%
  arrange(vegetable, seeds)

# Record Plot Making------------------------------------------------------------
# gg_record(
#   dir = here("2024-05-28/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )

# gg_stop_recording()


# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- c("purple4", "orange", "yellow", "green3", "lightyellow", "darkgreen" )

back_colour =  "#EFEFEF"
strong_text = darken("darkgreen",0.7)
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


my_caption <- glue("<b>Data: </b> The gardeneR Package",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #000000'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #000000'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #000000'>{linkedin_username}</span>")

my_subtitle <- paste(subtitle = "A description of the plot")

# The Actual Plot --------------------------------------------------------------
ggplot(data = myData, aes(area = seeds, fill = vegetable, 
                          subgroup = vegetable)) + # Very weird to me that subgroup is not variety
  geom_treemap(aes(alpha = seeds)) + 
  geom_treemap_subgroup_border() + 
  geom_treemap_subgroup_text(place = "middle", grow = F, alpha = 0.7, colour =
                             "black", min.size = 0, family = main_font) +
  geom_treemap_text(aes(label = paste(variety, ":", seeds, "seeds")), 
                    colour = "black", alpha = 0.4, place = "bottomleft", 
                    min.size = 2, family = main_font) + 
  scale_fill_manual(values = myPal) + 
  scale_alpha_continuous(range = c(0.5,1)) + 
  labs(title = "Lisa's Most Seeded Vegetables", 
       subtitle = "This type of visualization is called a tree map. Here, the 
       area of each rectangle corresponds to the number of seeds planted of 
       each vegetable and sub-variety among the six most seeded vegetables
       by Lisa Lendway (@lisalendway on Twitter) in her garden in 2021.", 
       caption = my_caption) + 
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
    plot.title = element_textbox_simple(size = rel(2.1),
                                        family = main_font,
                                        face = "bold",
                                        color = strong_text,
                                        margin = margin(8, 0, 8, 0)),
    plot.subtitle = element_textbox_simple(size = rel(1.1),
                                           family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 0, 10, 0)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    plot.caption = element_markdown(size = rel(0.8),
                                    colour = weak_text,
                                    family = main_font,
                                    hjust = c(0), 
                                    margin = margin(10,0,0,0))
  )


# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-05-28/2024-05-28.png"), height = 6, width = 8)


# gg_playback(
#   name = here("2024-05-28/2024-05-28_recording.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = "white"
# )
