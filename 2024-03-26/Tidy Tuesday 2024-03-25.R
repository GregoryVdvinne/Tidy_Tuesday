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
  glue,           # glue together formated text
  cfbplotR        # NCAA logos
)  


# Load and Wrangle Data --------------------------------------------------------

# Read data set(s) directly from github
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/team-results.csv')


myData <- myData %>%
  clean_names() %>%
  mutate(team = clean_school_names(team),
         # TEAM = str_replace(TEAM, "St\\.", "State"),
         abs_pase = abs(pase)) %>% # To make logos work in plot
  filter(r64 >= 5) %>%
  mutate(abs_pase_rank = rank(-abs_pase), 
         above_below = if_else(pase > 0, "above", "below")) %>%
  filter(abs_pase_rank %in% 1:20)

butler_y <- myData %>% 
  filter(team == "Butler") %>%
  pull(pase)

# Set Up Some Aesthetic Elements -----------------------------------------------

## Save color palette


myPal <- c("#009CDE", "#DE4200")

back_colour =  "#efefef"
strong_text = "black"
weak_text = lighten(strong_text, 0.25)


## Fonts


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

# Save Some Stuff for the plot -------------------------------------------------

github_icon <- "&#xf09b"
github_username <- "GregoryVdvinne"

twitter_icon <- "\uf099"
twitter_username <- "@GregoryVdvinne" 

linkedin_icon <- "\uf08c"
linkedin_username <- "Gregory Vander Vinne"


my_caption <- glue("<b>Data: </b> kaggle.com/datasets/nishaanamin/march-madness-data   ",
                   " \n <b>Graphic: </b>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
                   <span style='color: #3B3B3B'>{github_username}  </span>", 
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
                   <span style='color: #3B3B3B'>{twitter_username}  </span>",
                   "<span style='font-family:\"Font Awesome 6 Brands\";'>{linkedin_icon};</span>
                   <span style='color: #3B3B3B'>{linkedin_username}</span>")


# The Actual Plot --------------------------------------------------------------

ggplot() + 
  geom_col(data = myData, 
           aes(x = reorder(team, -abs_pase), y = pase, fill = above_below), 
           width = 0.1) + 
  geom_point(data = myData, 
             aes(x = reorder(team, -abs_pase), y = pase),
             size = 12, fill = back_colour, colour = weak_text, shape = 21) +
  geom_cfb_logos(data = myData, 
                 aes(x = reorder(team, -abs_pase), y = pase, team = team),
                 width = 0.05, height = 0.065, alpha = 1) + 
  geom_hline(yintercept = 0, color = lighten(weak_text,0.3)) + 
  labs(subtitle = paste("This chart shows the NCAA men's basketball teams that have", 
                     "<b><span style='color:", myPal[1], "'>Over Performed </span></b>",
                     "and",
                     "<b><span style='color:", myPal[2], "'>Under Performed</span></b>",
                     "the most in March Madness since 2008, relative to where 
                     they have been seeded. Teams must have made at least five 
                     tournament appearances to be included."), 
       title = "Absolute Madness", 
       caption = my_caption) + 
  ylab("Performance Above Seed Expectation") + 
  xlab("") + # Make Some Space
  scale_fill_manual(values = myPal) + 
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = back_colour,
                                    color = back_colour),
    plot.background = element_rect(fill = back_colour, 
                                   colour = back_colour),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = rel(2.5),
                                        family = main_font,
                                        face = "bold",
                                        color = strong_text,
                                        margin = margin(4, 0, 12, 4)),
    plot.subtitle = element_textbox_simple(size = rel(1.25),
                                           family = "Roboto",
                                           # face = "bold",
                                           colour = weak_text,
                                           margin = margin(0, 4, 12, 4)), 
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = rel(1.2),
                                family = "Roboto",
                                colour = weak_text, 
                                margin = margin(0, 4 , 0, 4)),
    axis.text.y = element_text(size = rel(1.1),
                               family = "Roboto",
                               colour = weak_text, 
                               margin = margin(0, 0, 0, 10)),
    axis.text.x = element_blank(),
    plot.caption = element_textbox_simple(size = rel(0.8),
                                    colour = weak_text,
                                    family = "Roboto",
                                    # family = "Bangers",
                                    hjust = 0.5, # Seems to be ignored
                                    margin = margin(8,4,0,0)),
    strip.text = element_text(family = main_font,
                              hjust = 1,
                              size = rel(1.8),
                              colour = strong_text),
    legend.position = "none"
    )


# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-03-26/2024-03-26.png"))  
  
  
  
  
