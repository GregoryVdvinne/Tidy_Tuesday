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
  ggbeeswarm,     # beeswarm plot
  glue,           # text concatanation w/ formatting
  ggside          # plots in margins of main plot 
)


######## Load and Wrangle Data ########

# Read data set directly from github
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')

myData <- myData %>%
  filter(income_flag %in% c( "Lower deprivation towns", "Mid deprivation towns", "Higher deprivation towns")) %>%
  mutate(income_flag = str_to_title(income_flag), 
         income_flag = factor(income_flag,
                              levels = c(  "Lower Deprivation Towns", "Mid Deprivation Towns", "Higher Deprivation Towns"))
         )



######## Set Up For Plotting ########

# Save color palette that looks like RStudio's cobalt

myPal <- c("#BF40BF", "white")
myCaption <- glue("<b> Data: </b> UK Office", " ", "for National Statistics",
                   "  \n <b> Graphic: </b> Gregory Vander Vinne")

back_colour = lighten("black",0.05)
line_colour = "grey20"
strong_text = "#D2D8DD"
weak_text = darken(strong_text, 0.2)


# #Add Roboto font family
# font_add(family = "Roboto", 
#          regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-REGULAR.ttf",
#          bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-BOLD.ttf")
# showtext_auto()

font_add_google("Roboto", "roboto")
showtext_auto()


# For centering  x axis title under main plot
my_scale <- 0.135
adjust <- ((1 / my_scale) / ((1 / my_scale) +1)) / 2

# Define Theme
my_theme <- function(base_size = 10) {
  theme_classic(base_size = base_size)+
    theme(
      axis.line = element_blank(),
      panel.grid.major.y = element_line(color = line_colour),
      axis.ticks = element_blank(),
      legend.position = c(0.1,1.025),
      legend.direction = "horizontal",
      legend.margin = margin(8, 0, 8, 0),
      legend.background = element_rect(fill = back_colour),
      panel.background = element_rect(fill = back_colour),
      plot.background = element_rect(fill = back_colour),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = rel(1.75),
                                          family = "Roboto",
                                          face = "bold",
                                          color = strong_text,
                                          margin = margin(5, 0, 10, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = "Roboto",
                                             colour = strong_text,
                                             margin = margin(0, 0, 28, 0)), 
      axis.title.x = element_text(size = rel(1.1),
                                family = "Roboto",
                                colour = strong_text, 
                                margin = margin(16, 0, 4, 0), 
                                hjust = adjust),
      axis.title.y = element_text(size = rel(1.1),
                                  family = "Roboto",
                                  colour = strong_text, 
                                  margin = margin(0, 6, 0, 2)),
      axis.text= element_text(size = rel(0.9),
                               family = "Roboto",
                               colour = weak_text),
      plot.caption = element_markdown(size = rel(0.8),
                                  colour = weak_text,
                                  family = "Roboto",
                                  hjust = 0),
      legend.text = element_text(size = rel(1),
                                 family = "Roboto",
                                 colour = strong_text),
      legend.title = element_text(size = rel(1),
                                  family = "Roboto",
                                  colour = weak_text),
      text = element_text(colour = weak_text, lineheight = 1.1)
    )
}




######## The Actual Plot ########

ggplot(myData, aes(x = income_flag, y = education_score, 
                   color = coastal, fill = coastal)) + 
  geom_beeswarm(alpha = 0.6, size = 1.5) + 
  geom_ysidedensity(aes(x=after_stat(density)), alpha = 0.5, show.legend = FALSE) +
  scale_ysidex_continuous(breaks = NULL) +
  scale_color_manual(values = myPal, name = "", labels = c("Coastal Towns", "Non-Coastal Towns")) +
  scale_fill_manual(values = myPal, name = "", labels = c("Coastal Towns", "Non-Coastal Towns")) +
  labs(title = "Income Deprivation and Educational Acheivement of Youth in English Towns", 
       subtitle = "In this beeswarm plot, each point represents a town in the
       England. The towns are grouped according to their income deprivation category
       along the x-axis, while the towns education scores fo towns are
       given on the y-axis. The width of each swarm at any given point on the 
       y-axis illustrates how many towns in that income deprevation category 
       have educational scores at that level. The marginal density
       plot on the side shows the distribution of education scores 
       for coastal and non-coastal towns. We see that
       towns with higher levels of income deprevation (poorer towns) tend to have
       lower educational acheivement scores and vice versa. Furthermore, coastal 
       towns are more likely to be in the higher income deprevation category and
       tend to have lower educational scores.",
       caption = myCaption) + 
  ylab("Education Score") + 
  xlab("Level of Income Deprivation") + 
  my_theme()

# Puts huge spaces between lines of subtitle. Saved manually
ggsave(here("2024-01-23/2024-01-23.png"))









