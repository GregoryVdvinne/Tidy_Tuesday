# Clear memory
rm(list = ls(all=T))


# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  ggpubr,         # arrange multiple ggplots together
  colorspace      # fancy stuff with colors  
)


######## Load and Wrangle Data ########

# Read data sets directly from github
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv')

# Height and weight by position and year
myData <- myData %>%
  mutate(position = case_when(
    position_code == "D" ~ "Defencemen",
    position_code == "G" ~ "Goalies",
    position_code %in% c("L", "R", "C") ~ "Forwards",
    TRUE ~ NA
    ),
    season = substr(as.character(season), nchar(as.character(season))-3, nchar(as.character(season))) %>%
      as.integer()
  ) %>%
  group_by(position, season) %>%
  summarise(height = mean(height_in_centimeters, na.rm = TRUE),
            weight = mean(weight_in_kilograms, na.rm = TRUE)) %>%
  pivot_longer(cols = c("height", "weight"), 
               values_to = "value", names_to = "ht_wt")



######## Setup for Plotting ########

#Add Signika Negative font family
font_add(family = "Signika_Negative",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/SignikaNegative-VariableFont_wght.ttf")
showtext_auto()


# Save Caption
myCaption <- c("Data: NHL API", "Created by Gregory Vander Vinne")


# Define a color palette
myPal = c("#57467b", "#558c8c", "#a53f2b", "#726e60")
back_color = lighten("#fffef5",0.0)
light_text = "grey35" 
dark_text = "grey5"
line_colour = "grey35"

#Required b/c strip text is used as verticle axis labels  
textData <- data.frame(
  label = c("Average Height", "AverageWeight"),
  ht_wt   = c("height", "weight") 
)


#Define Theme
my_theme <- function(base_size = 10) {
  theme_classic(base_size = base_size)+
    theme(
      axis.line = element_line(color = line_colour ),
      axis.ticks.y = element_line(color = line_colour ),
      axis.ticks.x = element_blank(),
      legend.position = c(0.085,1.04),
      legend.direction = "horizontal",
      legend.margin = margin(8, 0, 8, 0),
      legend.background = element_rect(fill = back_color),
      panel.background = element_rect(fill = back_color),
      plot.background = element_rect(fill = back_color),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_text( 
                                size = rel(1.75),
                                family = "Signika_Negative",
                                color = dark_text,
                                margin = margin(5, 0, 10, 0)),
      plot.subtitle = ggtext::element_textbox_simple(size = rel(1.1),
                                                     family = "Signika_Negative",
                                                     colour = dark_text,
                                                     margin = margin(0, 0, 28, 0)), 
      axis.title = element_blank(),
      axis.text = element_text(size = rel(0.9),
                                        family = "Signika_Negative",
                                        colour = light_text),
      plot.caption = element_text(size = rel(0.8),
                                           colour = light_text,
                                           hjust = c(0,1)),
      legend.text = element_text(size = rel(1),
                                          family = "Signika_Negative",
                                          colour = dark_text),
      legend.title = element_text(size = rel(1),
                                           family = "Signika_Negative",
                                           colour = light_text),
      text = element_text(colour = light_text, lineheight = 1.1), 
      strip.background = element_blank(), 
      strip.placement = "outside", 
      strip.text = element_text(size = rel(1),
                                family = "Signika_Negative",
                                colour = light_text)
    )
}



######## Plot ########

p <- ggplot(myData) + 
  facet_wrap(~ ht_wt, scales = "free_y", 
             strip.position = "left", 
             labeller = as_labeller(c(height = "Mean Height (CM)",
                                      weight = "Mean Weight (KG)")
                                    )
             ) + 
  geom_line(linewidth = 0.9, aes(x = season, y = value, color = position)) +
  geom_text(data = textData, 
            # family = "Signika_Negative", 
            mapping = aes(x = 1971, y = Inf, label = label),
            vjust   = 1.5, 
            size = 11.5/.pt,
            family = "Signika_Negative", 
            color = dark_text) + 
  scale_color_manual(values = lighten(myPal, 0.2), name = "") + 
  labs(title = "The Size of NHL Players Over Time", 
       subtitle = paste("Players are much larger than they were a century ago.",
                        "<span style='color:", myPal[3], "'>Goalies </span>", 
                        "used to be shorter than", 
                        "<span style='color:", myPal[2], "'>Forwards </span> and", 
                        "<span style='color:", myPal[1], "'>Defencemen </span>",
                        "but rapid increases in the average  height of netminders 
                        since the 80s has resulted in them being the tallest 
                        position group today. All three position groups increased 
                        in average weight until around the turn of the century, 
                        when forwards and defencemen started to get leaner." ),
       caption = myCaption) + 
  my_theme()

p

