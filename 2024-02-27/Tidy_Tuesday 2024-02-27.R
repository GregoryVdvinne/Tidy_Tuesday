# Clear memory
rm(list = ls(all=T))


# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  tidytext,       # text mining with tidy data formats
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  widyr,          # do some calcs without pivoting wider
  magrittr,       # for set_rownames
  ggcorrplot,     # correlogram
  glue, 
  colorspace      # fancy stuff with colors 
)


######## Load and Wrangle Data ########

# Read data set(s) directly from github
# events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/events.csv')
births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/births.csv')
# deaths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/deaths.csv')


# Tokenize the descriptions of the people born on Feb 29 
births <- births %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>% # remove words like "the" "is" "and"
  filter(!str_detect(word, "\\d")) %>% # remove words containing numbers
  mutate(word = str_to_title(word))

# Get the ten most common words
common_words <- births %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  slice_max(order_by = count, n=10)

# Correlations between words in descriptions
word_pair_cors <- births %>%
  pairwise_cor(word, person, sort = TRUE) %>%
  filter(item1 %in% common_words$word, 
         item2 %in% common_words$word)
# Turn it into a correlation matrix
cor_matrix <- as.data.frame.matrix(xtabs(word_pair_cors$correlation ~ ., word_pair_cors))
diag(cor_matrix) <- 1
cor_matrix <-  as.matrix(cor_matrix)  




# Save color palette

myPal <- c("#016a66" ,"#efb52b")

back_colour = "#EFEFEF"
strong_text = "grey15"
weak_text = lighten(strong_text, 0.2)


# Title font
font_add(family = "Roboto",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Roboto-Regular.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/Roboto-Bold.ttf")
# # Main Font
# font_add(family = "OpenSans",
#          regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/openSans-VariableFont_wdth,wght.ttf")

font_add_google("Open Sans", "OpenSans")

# Make the fonts work
showtext_auto()


# Define Theme
my_theme <- function(base_size = 10) {
  theme_classic(base_size = base_size)+
    theme(
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = back_colour),
      panel.background = element_rect(fill = back_colour),
      plot.background = element_rect(fill = back_colour),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = rel(2.25),
                                          family = "Roboto",
                                          face = "bold",
                                          color = strong_text,
                                          margin = margin(12, 0, 16, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = "OpenSans",
                                             colour = weak_text,
                                             margin = margin(8, 0, 16, 0)), 
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = rel(1),
                              family = "OpenSans",
                              colour = weak_text),
      axis.text.x = element_text(size = rel(1),
                                family = "OpenSans",
                                colour = weak_text, 
                                angle = 45),
      plot.caption = element_markdown(size = rel(0.8),
                                      colour = weak_text,
                                      family = "OpenSans",
                                      hjust = 0),
      legend.text = element_text(size = rel(1),
                                 family = "OpenSans",
                                 colour = weak_text),
      legend.title = element_text(size = rel(1),
                                  family = "OpenSans",
                                  colour = strong_text),
      text = element_text(colour = weak_text, lineheight = 1.1)
    )
}

# Save some of my text

myAnnotation <- paste("The words Coach, Footballer, and", 
                      "\n English are all highly correlated.",
                      "\n This is because many of the",
                      "\n English people on the page are",
                      "\n famous football (soccer) coaches", 
                      "\n and players")

# The Actual Plot  
ggcorrplot(cor_matrix, method = "circle", type = "upper",
           hc.order = TRUE) + 
  geom_text(
    x = "Politician",
    y = "Australian", 
    label = myAnnotation,
    family = "OpenSans",
    colour = weak_text,
    size = 3.25,
    lineheight = 1, 
    hjust = 0
  ) +
  geom_curve(x = "Footballer", xend = "Coach",
             y = "Australian", yend = "Coach",
             color = weak_text,
             curvature = 0.5,
             angle = 40,
             arrow = arrow(length = unit(0.03, "npc")),
             linewidth = 0.5) +
  scale_fill_gradient2(high = myPal[1], mid = back_colour, low = myPal[2],
                       name = paste0("Correlation",
                                    "\n",
                                    "Coefficient")) +
  labs(title = "Correlations Between Words Used to Describe Famous People Born on February 29th", 
       subtitle = "This figure illustrates how likely the most common words used 
       on Wikipedia to describe famous people born on leap day are to appear together in the
       description of the same person. A large green circle indicates that the
       word in the corresponsing row is likely to appear alongside the word in
       the corresponding column. A large orange circle indicates that the presence
       of one word decreases the likelihood of the other word being present in the
       same person's description.",
       caption = glue("Data: wikipedia.org/wiki/February_29",
                       "  \n Graphic: Gregory Vander Vinne")) +
  xlab("") + 
  my_theme()



showtext_opts(dpi = 300) # For ggsave

ggsave(here("2024-02-27/2024-02-27.png"))
