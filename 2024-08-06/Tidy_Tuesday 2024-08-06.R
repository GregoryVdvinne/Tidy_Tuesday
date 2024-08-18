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
tuesdata <- tidytuesdayR::tt_load(2024, week = 32)

myData <- tuesdata$olympics

winter <- myData |>
  filter(season == "Winter") |>
  mutate(team = str_replace(team, "-\\d+", "")) # replace dash number with nothing

winter_years <- winter$year |>
  unique()

winter_golds <- winter |>
  group_by(team, year ) |> 
  filter(medal == "Gold") |> # So team sports count once
  distinct(year, event) |>
  summarise(golds = n()) |>
  group_by(team) |>
  complete(year = winter_years) |> # create years with missing value for gold for years a country didn't win any
  mutate(golds = if_else(is.na(golds), 0, golds)) |> # replace NAs with 0 in golds
  arrange(year) |>
  mutate(total_golds = cumsum(golds)) |>
  ungroup() |>
  group_by(year) |>
  mutate(gold_rank = rank(-total_golds, ties.method = "first")) |>
  filter(gold_rank <= 25 & total_golds > 0) 
  # # Not used b/c too many NAs
  # |> mutate(country_code = countrycode(sourcevar = team, 
  #                                   origin = "country.name", destination = "iso2c" ))
  
  


# # Ice Hockey was in the summer olympics!?
# summer_hockey <- summer |>
#   filter(sport == "Ice Hockey")




# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- c("#00A651", "#0081C8")

back_colour =  myPal[2]
strong_text = "White"
weak_text = darken(strong_text, 0.1)

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


my_caption <- c("Data: Kaggle Olympic History Data", "Graphic: Gregory Vander Vinne")



# # Record Plot Making------------------------------------------------------------
# gg_record(
#   dir = here("2024-08-06/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )

# gg_stop_recording()







# The Actual Plot --------------------------------------------------------------

p <- ggplot(winter_golds, aes(y = gold_rank)) + 
  # Geom col did not place nice w/ gganimate so I use geom_rect. Cred to Aditay Dahia
  geom_rect(aes(ymin = gold_rank - 0.45,
                ymax = gold_rank + 0.45,
                xmax = total_golds, 
                xmin = 0,
                group = team), 
            fill = myPal[1]) + 
  geom_text(aes(x = total_golds, 
                label = if_else(total_golds > 20 , as.character(total_golds), "")), 
            hjust = "left", 
            family = main_font, color = strong_text) +
  geom_text(aes(x = 0, label = team), hjust = "left", 
            family = main_font, color = strong_text) +
  labs(title = "Winter Olympic Cumulative Gold Medals", 
       subtitle = '{closest_state}', 
       caption = my_caption, 
       x = "Total Gold Medals") + 
  transition_states(year) + 
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = back_colour,
                                    color = back_colour),
    plot.background = element_rect(fill = back_colour, 
                                   colour = back_colour),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_text(size = rel(2.5),
                              family = main_font, 
                              face = "bold",
                              color = strong_text,
                              margin = margin(4, 0, 12, 4)),
    plot.subtitle = element_textbox_simple(size = rel(1.85),
                                           family = "Roboto",
                                           face = "bold",
                                           colour = strong_text,
                                           margin = margin(0, 4, 12, 4)), 
    axis.title.x = element_text(size = rel(1.25),
                                family = "Roboto",
                                colour = strong_text,
                                margin = margin(0, 8, 0, 8)),
    axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.text.x = element_text(size = rel(1.25),
                               family = "Roboto",
                               colour = strong_text,
                               margin = margin(8, 0, 8, 0)), # Axis text does not appear. Unresolved on stack overflow
    plot.caption = element_text(size = rel(0.8),
                                colour = weak_text,
                                family = "Roboto",
                                hjust = c(0,1),
                                margin = margin(8,4,0,0)),
    legend.position = "none"
  )







# Animate the ggplot object with specified settings
animate(p,
        duration = 30,
        fps = 10,
        width = 800,
        height = 500,
        start_pause = 10, 
        end_pause = 30)




# Save it
anim_save(here("2024-08-06/2024-08-06.gif"))




