# Setup ------------------------------------------------------------------------

# Clear memory
rm(list = ls(all=T))

# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)

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
myData<- tidytuesdayR::tt_load(2024, week = 41)$most_visited_nps_species_data

# park_category <-  myData |>
#   group_by(ParkName, CategoryName) |>
#   summarise(count = n()) |>
#   ungroup() |>
#   group_by(CategoryName) |>
#   mutate(share = count / sum(count)*100) |>
#   select(-count) |>
#   pivot_wider(names_from = CategoryName, values_from = share) |>
#   select(ParkName, Bird, Fish, Mammal, Reptile)

myData <-  myData |>
  distinct(SciName, .keep_all = TRUE)

# spec_counts <-  myData |>
#   group_by(CategoryName) |>
#   summarise(count = n()) |>
#   arrange(desc(count))

vertebrates <- myData |>
  filter(CategoryName %in% c("Bird", "Mammal", "Reptile", "Amphibian", "Fish")) |>
  group_by(CategoryName, Order) |>
  summarise(n_species = n())


# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer_d("nationalparkcolors::Yellowstone")[c(1,3:6)]

back_colour =  lighten(paletteer_d("nationalparkcolors::Yellowstone")[2],0)
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


my_caption <- glue("<b>Data: </b>  NPSpecies - The National Park Service biodiversity database. irma.nps.gov/npspecies/.Accessed September 2nd, 2024.")

my_subtitle <- glue( "This visual shows how many unique confirmed species of 
                    vertebrates there are in the USA's national parks according ", 
                    " \n  to the National Park Service. Species are categorized first by
                    class (e.g., Mammal) and then by order (e.g., Carnivora). ",
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
  dir = here("2024-10-08/recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 100
)



# The Actual Plot --------------------------------------------------------------



ggplot(data = vertebrates, aes(area = n_species, fill = CategoryName, 
                          subgroup = CategoryName)) + # Very weird to me that subgroup is not order
  geom_treemap(aes(alpha = n_species)) + 
  geom_treemap_subgroup_border(color = "grey65") + 
  geom_treemap_subgroup_text(place = "middle", grow = F, alpha = 0.7, colour =
                               "black", min.size = 0, family = main_font) +
  geom_treemap_text(aes(label = paste(Order, ":", n_species, "Species")), 
                    colour = "black", alpha = 0.4, place = "bottomleft", 
                    min.size = 2, family = main_font) + 
  scale_fill_manual(values = myPal) +
  scale_alpha_continuous(range = c(0.5,1)) +
  labs(title = "Species of Vertebrates in American National Parks",
       subtitle = my_subtitle,
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
    plot.title = element_textbox_simple(size = rel(2.2),
                                        family = main_font,
                                        face = "bold",
                                        color = strong_text,
                                        margin = margin(8, 0, 10, 0)),
    plot.subtitle = element_markdown(size = rel(1.1),
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





# # For ggsave text sizing
# showtext_opts(dpi = 300)
# # Save plot
# ggsave(here("2024-10-08/2024-10-08.png"), height = 6, width = 8)

gg_playback(
  name = here("2024-10-08/2024-10-08_recording.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)


