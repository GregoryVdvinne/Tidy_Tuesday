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
  ggraph,         # network graphs and such
  tidygraph,      # network graphs and such
  glue            # glue together formatted text
)  


# Load and wrangle data --------------------------------------------------------

# Load data using the Tidy Tuesday package
tuesdata <- tidytuesdayR::tt_load(2024, week = 16)

shiny_revdeps <- tuesdata$shiny_revdeps
package_details <- tuesdata$package_details

rm(tuesdata)

# Top Parents
top_parents <- shiny_revdeps %>%
  filter(dependency_type == "depends") %>%
  group_by(parent) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count >= 5)

# Rename for ggraph
myData <- shiny_revdeps %>%
  filter(dependency_type == "depends",
         child %in% top_parents$parent & parent %in% top_parents$parent) %>%
  left_join(top_parents, by = "parent") %>%
  rename("from" = "parent", "to" = "child")


top_top_parents <- myData %>%
  group_by(to) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 1)

myData <- myData %>%
  filter(to %in% top_top_parents$to)


# Record Plot Making (My first time doing so)-----------------------------------
# gg_record(
#   dir = here("2024-04-16/recording"),
#   device = "png",
#   width = 7,
#   height = 5,
#   units = "in",
#   dpi = 100
# )

# gg_stop_recording()





# Set Up Some Aesthetic Elements -----------------------------------------------

# Save color palette

myPal <- paletteer::paletteer_d("NineteenEightyR::miami1")

back_colour =  myPal[2]
strong_text = lighten("black",0.2)
weak_text = lighten(strong_text, 0.25)

# Fonts

# Main Font
font_add(family = "Roboto", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-REGULAR.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-BOLD.ttf")


# Symbols
font_add(family = "Font Awesome 6 Brands",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Brands-Regular-400.otf")

# Make the fonts work
showtext_auto()

main_font = "Roboto"
title_font = "Roboto"



# Save Some Stuff for the plot -------------------------------------------------

github_icon <- "&#xf09b"
github_username <- "GregoryVdvinne  "

twitter_icon <- "\uf099"
twitter_username <- "@GregoryVdvinne  "

linkedin_icon <- "\uf08c"
linkedin_username <- "Gregory Vander Vinne"


my_caption <- glue("<b>Data: </b> Shiny on CRAN  ",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #3B3B3B'>{github_username}</span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #3B3B3B'>{twitter_username}  </span>   ",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #3B3B3B'>{linkedin_username}</span>")



# The Actual Plot --------------------------------------------------------------

# Get the data into a form that ggraph can use
set_graph_style(plot_margin = margin(0,0,0,0))
graph <- as_tbl_graph(myData) %>% 
  activate(nodes) %>%
  mutate(popularity = centrality_degree(mode = "in", weights = count))

a <- grid::arrow(type = "open", length = unit(0.075, "inches"))

# Plot
ggraph(graph, layout = 'kk') + 
  geom_edge_density(fill = myPal[3]) + 
  geom_edge_bend(color = myPal[4], 
                 end_cap = circle(3, 'mm'), # avoid overlapping w/ node_text
                 arrow = a) +
  geom_node_label(aes(label = name, size = popularity), 
                  color = darken(myPal[5],0), 
                  label.padding = unit(0.09, "lines"), 
                  label.r = unit(0.25, "lines")) + 
  scale_size_continuous(range = c(1.75,4.5)) +
  labs(title = "Relationships Between R Packages", 
       subtitle = "This graph visualizes parent-child relationships between 
       select R packages that are connected to R Shiny. Larger text indicates that
       an R package is a 'parent' to a greater number of other R packages.", 
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
    plot.title = element_textbox_simple(size = rel(2),
                                        family = title_font,
                                        face = "bold",
                                        color = strong_text,
                                        margin = margin(8, 0, 12, 8)),
    plot.subtitle = element_textbox_simple(size = rel(1.1),
                                           family = main_font,
                                           colour = weak_text,
                                           margin = margin(0, 0, 12, 8)), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    plot.caption = element_markdown(size = rel(0.7),
                                    colour = weak_text,
                                    family = main_font,
                                    hjust = c(0), 
                                    margin = margin(-5,0,0,8)),
    text = element_text(colour = weak_text, lineheight = 1.1)
  )

# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-04-16/2024-04-16.png"), height = 7, width = 9)



gg_playback(
  name = here("2024-04-16/2024-04-16_recording.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
