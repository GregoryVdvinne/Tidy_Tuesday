# Clear memory
rm(list = ls(all=T))


# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  # tidytuesdayR,   # get Tidy Tuedday data
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtect,         # fancy text in plots
  # RColorBrewer,   # color palettes
  colorspace      # fancy stuff with colors  
)


######## Load and Wrangle Data ########

# Read data sets directly from github
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv')

# Height and weight by position and year
myData <- myData %>%
  mutate(position = case_when(
    position_code == "D" ~ "Defenceman",
    position_code == "G" ~ "Goalie",
    position_code %in% c("L", "R", "C") ~ "Winger",
    TRUE ~ NA
    )
  ) %>%
  group_by(position, season) %>%
  summarise(height = mean(height_in_centimeters, na.rm = TRUE),
            weight = mean(weight_in_kilograms, na.rm = TRUE))



######## Setup for Plotting ########

# Save Caption
myCaption <- c("Data: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv ", 
               "Created by Gregory Vander Vinne")

# Define a color palette
myPal = lighten(c("#57467b", "#558c8c", "#a53f2b", "#726e60"),0.25)
back_color = lighten("#fffef5",0.4)
light_text = "grey35" 
dark_text = "grey5"
line_colour = "grey35"
  

#Define Theme
my_theme <- function(base_size = 10) {
  theme_classic(base_size = base_size)+
    theme(
      axis.line = element_line(color = line_colour ),
      axis.ticks.y = element_line(color = line_colour ),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.margin = margin(-12, 0, -8, 0),
      legend.background = element_rect(fill = back_color),
      panel.background = element_rect(fill = back_color),
      plot.background = element_rect(fill = back_color),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox_simple( face = "bold",
                                                   size = rel(1.3),
                                                   # family = "Arial",
                                                   color = dark_text,
                                                   margin = margin(5, 0, 12, 0)),
      plot.subtitle = ggtext::element_textbox_simple(size = rel(1),
                                                     # family = "Arial",
                                                     colour = light_text,
                                                     margin = margin(-6, 0, 16, 0)), 
      axis.title.y = element_text(size = rel(1),
                                           # family= "Arial",
                                           colour = light_text,
                                           margin = margin(0, 5, 0, 0)),
      # axis.title.x = element_text(size = rel(1),
      #                                      # family= "Arial",
      #                                      colour = light_text,
      #                                      margin = margin(5, 0, 0, 0)),
      axis.title.x = element_blank(),
      axis.text = element_text(size = rel(0.9),
                                        # family = "Arial",
                                        colour = light_text),
      plot.caption = element_text(size = rel(0.8),
                                           colour = light_text,
                                           hjust = 0),
      legend.text = element_text(size = rel(1),
                                          # family = "Arial",
                                          colour = light_text),
      legend.title = element_text(size = rel(1),
                                           # family = "Arial",
                                           colour = light_text),
      text = element_text(colour = light_text, lineheight = 1.1)
    )
}



######## Plot ########

p <- ggplot(myData, aes(x = season, y = height, color = position)) + 
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = myPal) + 
  labs(title = "NHL Players Are Much Taller Than They Were a Century Ago", 
       caption = myCaption) + 
  ylab("Mean Height (Centimeters)") +
  my_theme()

p


