# Clear memory
rm(list = ls(all=T))


# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  colorspace      # fancy stuff with colors  
)


######## Load and Wrangle Data ########

# Read data set directly from github
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-16/polling_places.csv')

# # View election dates in data
# myData %>% 
#   group_by(election_date) %>%
#   summarise(Count = n())

#Get number of polling places in 2016 and 2020 for states with data in both years
myData <- myData %>%
  filter(as.character(election_date) %in% c("2016-11-08","2020-11-03")) %>%
  mutate(year = year(election_date)) %>%
  group_by(state, year) %>%
  summarise(n_places = n()) %>%
  pivot_wider(values_from = n_places, names_from = year) %>%
  mutate(change = `2020`-`2016`, latest = `2020`) %>%
  drop_na()

#Get mean across states
mean_places <- colMeans(myData[,-1]) %>%
  t() %>%
  as.data.frame() %>%
  mutate(state = "AVG")

#Combine with myData and add dummy indicating overall mean
myData <- rbind(myData, mean_places) %>%
  mutate(mean = if_else(state == "AVG", TRUE, FALSE)) %>% # must have data for both years
  pivot_longer(cols = c(`2016`,`2020`),
               names_to = "year", values_to = "n_places")



######## Set Up For Plotting ########

# Save color palette that looks like RStudio's cobalt

myPal <- c("#D48902","#1FA40E", "#A44A70", "#006BD1", "#4A5764")
myCaption <- c("Data: The Center for Public Integrity", "Created by Gregory Vander Vinne")

back_colour = "#002141"
line_colour = "#193753"
strong_text = "#D2D8DD"
weak_text = darken(strong_text, 0.2)


#Add Roboto font family
font_add(family = "Roboto", 
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-REGULAR.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-BOLD.ttf")
showtext_auto()

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
      axis.title = element_blank(),
      axis.text = element_text(size = rel(0.9),
                               family = "Roboto",
                               colour = weak_text),
      plot.caption = element_text(size = rel(0.8),
                                  colour = weak_text,
                                  family = "Roboto",
                                  hjust = c(0,1)),
      legend.text = element_text(size = rel(1),
                                 family = "Roboto",
                                 colour = strong_text),
      legend.title = element_text(size = rel(1),
                                  family = "Roboto",
                                  colour = weak_text),
      text = element_text(colour = weak_text, lineheight = 1.1)
    )
}

# A Dumbbell Plot

ggplot(myData, aes(x = reorder(state,latest), y = n_places)) + 
  geom_line(color = myPal[4], alpha = 0.5, linewidth = 3.5) +  # we want thick line
  geom_point(aes(color = year, shape = mean), size = 3.5, alpha = 0.7) +  # Big dots
  coord_flip() +
  scale_color_manual(values = myPal, name = "Election Year:") +
  scale_shape_manual(values = c(16,15), guide = "none") +
  scale_y_continuous(labels = scales::comma)+
  ggtitle(paste("Number of Polling Places by State in ",
                     "<span style='color:", myPal[1], "'>2016</span>", 
                     "and", 
                     "<span style='color:", myPal[2], "'>2020 </span>"), 
          subtitle = "This dumbbell plot shows the number of polling places in states for which data are available for the last 
                      two American presidential elections. Overall, the number of polling places was slightly down in these 
                      states. The overall decline was driven by large decreases in Minnesota, Maryland, and Indiana. Most states
                      saw very small changes.") +
  labs(caption = myCaption) + 
  my_theme()

# Puts huge spaces between lines of subtitle. Saved manually
# ggsave(here("2024-01-16/2024-01-16_Plot.png"))







