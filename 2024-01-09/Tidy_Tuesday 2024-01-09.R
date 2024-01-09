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
            weight = mean(weight_in_kilograms, na.rm = TRUE))



######## Setup for Plotting ########

# #Add Signika Negative font family
# font_add(family = "Signika_Negative", 
#          regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/SignikaNegative-VariableFont_wght.ttf")
# showtext_auto()


# Define a color palette
myPal = lighten(c("#57467b", "#558c8c", "#a53f2b", "#726e60"),0.25)
back_color = lighten("#fffef5",0.0)
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
      legend.position = c(0.2,0.9),
      # legend.direction = "verticle",
      legend.margin = margin(8, 0, 8, 0),
      legend.background = element_rect(fill = back_color),
      panel.background = element_rect(fill = back_color),
      plot.background = element_rect(fill = back_color),
      plot.caption.position = "plot",
      # plot.title.position = "plot",
      plot.title = element_text( 
                                size = rel(1.5),
                                # family = "Signika_Negative",
                                color = dark_text,
                                margin = margin(5, 0, 5, 0),
                                hjust = 0.5),
      plot.subtitle = ggtext::element_textbox_simple(size = rel(1),
                                                     # family = "Signika_Negative",
                                                     colour = light_text,
                                                     margin = margin(-6, 0, 16, 0)), 
      axis.title.y = element_text(size = rel(1),
                                           # family= "Signika_Negative",
                                           colour = light_text,
                                           margin = margin(0, 5, 0, 0)),
      # axis.title.x = element_text(size = rel(1),
      #                                      # family= "Signika_Negative",
      #                                      colour = light_text,
      #                                      margin = margin(5, 0, 0, 0)),
      axis.title.x = element_blank(),
      axis.text = element_text(size = rel(0.9),
                                        # family = "Signika_Negative",
                                        colour = light_text),
      plot.caption = element_text(size = rel(0.8),
                                           colour = light_text,
                                           hjust = 1),
      legend.text = element_text(size = rel(1),
                                          # family = "Signika_Negative",
                                          colour = light_text),
      legend.title = element_text(size = rel(1),
                                           # family = "Signika_Negative",
                                           colour = light_text),
      text = element_text(colour = light_text, lineheight = 1.1)
    )
}



######## Plot ########

p1 <- ggplot(myData, aes(x = season, y = height, color = position)) + 
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = myPal, name = "") + 
  labs(title = "Player Height", 
       subtitle = "\n", #space for legend in top-left
       caption = "Data: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09") + 
  ylab("Mean Height (Centimeters)") +
  my_theme()

p1

p2 <- ggplot(myData, aes(x = season, y = weight, color = position)) + 
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = myPal, name = "") + 
  labs(title = "Player Weight", 
       subtitle = "\n", #space for legend in top-left
       caption = "Created by Gregory Vander Vinne") + 
  ylab("Mean Weight (Kilograms)") +
  my_theme() + 
  theme(
    plot.caption = element_text(size = rel(0.8),
                                colour = light_text,
                                hjust = 0),
  )

p2


plots <- ggarrange(p1, p2)

annotate_figure(
  annotate_figure(plots,
                  top=text_grob("Subtitle", 
                                hjust = 18.3),
  ),
  top=text_grob("Main title",
                hjust = 7.5,
                size = 24)
) + 
  bgcolor(back_color)

