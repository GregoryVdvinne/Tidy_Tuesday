
# ggtext seems to be incompatible with gganimate. That is why there is a lot of commented-out code.  
  # I would appreciate insight if anyone has some!

# Clear memory
rm(list = ls(all=T))


# remotes::install_github("Kazink36/cfbplotR")

# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  colorspace,     # fancy stuff with colors 
  janitor,        # some efficient data cleaning stuff
  gganimate,      # animation in ggplot2
  sf,             # maps
  maps,           # to get city populations
  camcorder,      # record the making of the plot into a gif
  glue            # glue together formatted text
)  


# Load and Wrangle Data --------------------------------------------------------

# Read data set(s) directly from github
eclipse_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_total_2024.csv')
# eclipse_partial_2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_partial_2024.csv')

# Combine them and indicate which are partial vs total
# eclipse_data <- eclipse_data
# %>%
#   mutate(partial_total = "total") %>%
#   rbind(
#     eclipse_partial_2024 %>%
#       mutate(partial_total = "partial", 
#              eclipse_6 = NA)
#   )

# Get some city populations (Thanks Georgios Karamanis for sharing)
eclipse_data <- maps::us.cities %>% 
  mutate(
    name = substr(name, 1, nchar(name) - 2) %>% trimws()) %>%
  select(name, pop) %>%
  inner_join(eclipse_data, by = "name") %>%
  mutate(city_state = name, 
         enter_time = as.numeric(hms::as_hms(eclipse_3)-as.numeric(hms::as_hms(eclipse_1))),
         exit_time = as.numeric(hms::as_hms(eclipse_6)-as.numeric(hms::as_hms(eclipse_4))), 
         middle_time = as.numeric(hms::as_hms(eclipse_3))) %>%
  distinct(city_state, .keep_all = TRUE)
  

# Get borders of  USA
borders <- rnaturalearth::ne_states("united states of america") %>%
  filter(!(name %in% c("Hawaii", "Alaska")))


# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer::paletteer_d("futurevisions::atomic_orange")

back_colour =  myPal[3]
strong_text = lighten("black",0.1)
weak_text = lighten(strong_text, 0.25)

# Fonts

# Main Font
font_add(family = "Roboto", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-REGULAR.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-BOLD.ttf")


# # Symbols
# font_add(family = "Font Awesome 6 Brands",
#          regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Brands-Regular-400.otf")

# Make the fonts work
showtext_auto()

main_font = "Roboto"

# Save Some Stuff for the plot -------------------------------------------------

# github_icon <- "&#xf09b"
# github_username <- "GregoryVdvinne  "
# 
# twitter_icon <- "\uf099"
# twitter_username <- "@GregoryVdvinne  "
# 
# linkedin_icon <- "\uf08c"
# linkedin_username <- "Gregory Vander Vinne"
# 
# 
# my_caption <- glue("<b>Data: </b> NASA Scientific Visualization Stuido   ",
#                    " \n <b>Graphic: </b>",
#                    "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
#                    <span style='color: #3B3B3B'>{github_username}</span>   ",
#                    "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
#                    <span style='color: #3B3B3B'>{twitter_username}  </span>   ",
#                    "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
#                    <span style='color: #3B3B3B'>{linkedin_username}</span>")

my_caption <- (c("Data: NASA Scientific Visualization Studio", "Graphic: Gregory Vander Vinne"))


# # Record Plot Making (My first time doing so)-----------------------------------
# gg_record(
#   dir = here("2024-04-09/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 300
# )

# gg_stop_recording()


# The Actual Plot --------------------------------------------------------------

p <- ggplot() + 
  geom_sf(data = borders, fill = myPal[1], color = myPal[3]) + 
  geom_point(data = eclipse_data, 
             aes(x = lon, y = lat, size = pop, 
                 group = name,
                 ), 
             alpha = 0.75,
             color = myPal[2])+
  transition_components(eclipse_data$middle_time,
                        enter_length = mean(eclipse_data$enter_time),
                        exit_length = mean(eclipse_data$exit_time)
                        ) +
  enter_fade() +
  exit_fade() +
  labs(title = "2024's Total Solar Eclipse in American Cities", 
       caption  = my_caption) +
  scale_y_continuous(limits = c(min(eclipse_data$lat) - 2.5, max(eclipse_data$lat) + 2)) +
  scale_x_continuous(limits = c(min(eclipse_data$lon) - 2, max(eclipse_data$lon) + 0.1)) + 
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
    plot.subtitle = element_textbox_simple(size = rel(1.25),
                                           family = "Roboto",
                                           face = "bold",
                                           colour = weak_text,
                                           margin = margin(0, 4, 12, 4)), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    # plot.caption = element_textbox_simple(size = rel(0.8),
    #                                       colour = weak_text,
    #                                       family = "Roboto",
    #                                       margin = margin(8,4,0,0)),
    plot.caption = element_text(size = rel(0.8),
                                colour = weak_text,
                                family = "Roboto",
                                hjust = c(0,1),
                                margin = margin(8,4,0,0)),
    legend.position = "none"
  )

# free up some memory to speed up
gc()

# Better FPS etc
animate(p, duration = 7, fps  = 20,
        height = 700, width = 1000)

# Save it
anim_save(here("2024-04-09/2024-04-09.gif"))


