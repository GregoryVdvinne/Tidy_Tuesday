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
  paletteer,      # color palettes and more
  ggraph,         # Network graphs
  igraph,         # igraph yougraph weallgraph for icecream
  colorspace      # fancy stuff with colors 
)  


######## Load and Wrangle Data ########

# Read data set(s) directly from github
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')

myStopWords <- c("san", "fransisco", "pf", "pca", "3")

# Count bigrams 
myBigrams <- myData %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, myStopWords),
         !word2 %in% c(stop_words$word, myStopWords)) %>%
  count(word1, word2, sort = TRUE) %>%
  mutate(word1 = str_to_title(word1), 
         word2 = str_to_title(word2)) %>%
  drop_na()



######## Set Up Some Aesthetic Elements ########

## Save color palette

myPal <- paletteer::paletteer_d("ggthemes::wsj_dem_rep")[2:3]

back_colour =  paletteer::paletteer_d("ggthemes::wsj_dem_rep")[1] %>% darken(0.1)
strong_text = lighten(myPal[2],0.2)
weak_text = darken(strong_text, 0.1)


## Fonts

# Title font
font_add_google("Roboto Slab", "roboto_slab")
font_add_google("Roboto", "roboto")

# Make the fonts work
showtext_auto()

title_font = "Robot Slab"
main_font = "Roboto"

## Define Theme
my_theme <- function(base_size = 10) {
  theme_minimal(base_size = base_size)+
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = back_colour,
                                      color = back_colour),
      plot.background = element_rect(fill = back_colour, 
                                     colour = back_colour),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.title = element_textbox_simple(size = rel(2.25),
                                          family = title_font,
                                          face = "bold",
                                          color = strong_text,
                                          margin = margin(8, 0, 12, 8)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = main_font,
                                             colour = weak_text,
                                             margin = margin(8, 0, 12, 8)), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_blank(),
      plot.caption = element_markdown(size = rel(0.8),
                                      colour = weak_text,
                                      family = main_font,
                                      hjust = c(0), 
                                      margin = margin(-5,0,0,0)),
      text = element_text(colour = weak_text, lineheight = 1.1)
    )
}


## The Actual Plot


set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

myBigrams %>%
  filter(n > 6) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = log(n)),
                 color = weak_text,
                 show.legend = FALSE, 
                 arrow = a) +
  geom_node_point(color = myPal[1], size = 5) +
  geom_node_text(aes(label = name),
                 family = main_font,
                 color = strong_text,
                 size = 3,
                 vjust = 2, hjust = 0.5) +
  labs(title = "Sequential Words in Fiscal Sponsor Descriptions", 
       subtitle = "T", 
       caption = paste("<b> Data: </b> Fiscal Sponsor Directory",
                   " \n <b> Graphic: </b> Gregory Vander Vinne")) +
  my_theme()



## Save the plot
# 
# # For ggsave text sizing
# showtext_opts(dpi = 300)
# # Save plot
# ggsave(here("2024-03-12/2024-03-12.png"))


