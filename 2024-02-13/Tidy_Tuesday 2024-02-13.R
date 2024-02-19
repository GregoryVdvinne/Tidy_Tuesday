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
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')

myData <- myData %>%
  pivot_longer(cols = Candy:GiftCards, 
               values_to = "Spending", 
               names_to = "Category") %>%
  mutate(Year = as.character(Year), 
         Category = snakecase::to_title_case(Category))

myData <- myData %>%
  select(-c(PercentCelebrating, PerPerson)) %>%
  pivot_wider(names_from = Year, values_from = Spending) %>%
  mutate(rank_22 = rank(`2022`)) %>%
  select(rank_22, Category) %>%
  right_join(myData, by = "Category")

######## Set Up For Plotting ########

# Title font
font_add(family = "LobsterTwo",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/LobsterTwo-REGULAR.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/LobsterTwo-Bold.ttf")
# Main Font
font_add(family = "OpenSans",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/openSans-VariableFont_wdth,wght.ttf")
# Hollow symbols
font_add(family = "fa-regular",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Free-Regular-400.otf")
# Solid symbols
font_add(family = "fa-solid",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Font Awesome 6 Free-Solid-900.otf")

# Make the fonts work
showtext_auto()


# Save color palette

myPal <- c("#f97e66", "#d03d00")
myCaption <- glue("<b> Data: </b> UK Office", " ", "for National Statistics",
                  "  \n <b> Graphic: </b> Gregory Vander Vinne")

back_colour = "#a5cbd8"
line_colour = lighten(back_colour, 0.2)
# line_colour = lighten(back_colour, 0.5)
strong_text = "grey15"
weak_text = lighten(strong_text, 0.2)


# Define Theme
my_theme <- function(base_size = 10) {
  theme_classic(base_size = base_size)+
    theme(
      axis.line = element_blank(),
      panel.grid.major = element_line(color = line_colour),
      axis.ticks = element_blank(),
      # legend.position = c(0.1,1.025),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.margin = margin(0, 0, 8, 0),
      legend.background = element_rect(fill = back_colour),
      panel.background = element_rect(fill = back_colour),
      plot.background = element_rect(fill = back_colour),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = rel(2.5),
                                          family = "LobsterTwo",
                                          face = "bold",
                                          color = strong_text,
                                          margin = margin(5, 0, 10, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = "OpenSans",
                                             colour = weak_text,
                                             margin = margin(0, 0, 8, 0)), 
      axis.title.y = element_blank(),
      axis.text= element_text(size = rel(0.9),
                              family = "OpenSans",
                              colour = weak_text),
      plot.caption = element_markdown(size = rel(0.8),
                                      colour = weak_text,
                                      family = "OpenSans",
                                      hjust = 0),
      legend.text = element_text(size = rel(1.1),
                                 family = "OpenSans",
                                 colour = weak_text),
      legend.title = element_text(size = rel(1.1),
                                  family = "OpenSans",
                                  colour = strong_text),
      text = element_text(colour = weak_text, lineheight = 1.1)
    )
}

ggplot(data = myData, aes(x = Year, y = reorder(Category, rank_22), size = Spending, color = Spending)) + 
  labs(title = "Valentine's Day Spending", 
       subtitle = "In this figure, the size and color of the hearts represent the 
       amount that the average person surveyed spent on each category of gift in
       the given year. Larger, darker hearts represent more spending, while
       smaller, lighter hearts represent less spending.",
       caption = paste("Data: US National Retail Federation",
                       "  \n Graphic: Gregory Vander Vinne")) + 
  xlab("") +
  geom_text(data = myData,
            family = "fa-solid",
            label = "\u2764",
            alpha = 0.5) +
  geom_text(
            family = "fa-regular",
            label = "\u2764") +
  scale_size_continuous(name = "Average Amount Spent", labels = scales::dollar) +
  scale_color_continuous(low = myPal[1], high = myPal[2], name = "Average Amount Spent", 
                         guide = "legend", labels = scales::dollar) +
  my_theme() 
              

showtext_opts(dpi = 300) # For ggsave  

ggsave(here("2024-02-13/2024-02-13.png"))
              
              
