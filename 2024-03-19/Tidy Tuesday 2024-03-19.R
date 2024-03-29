# Clear memory
rm(list = ls(all=T))

# install.packages("ggchicklet", repos = "https://cinc.rud.is")

# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  paletteer,      # color palettes and more
  ggchicklet,     # Rounded corners or bar graph
  colorspace      # fancy stuff with colors 
)  


#Load and Wrangle Data ---------------------------------------------------------

# Read data set(s) directly from github
myData <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-19/mutant_moneyball.csv')

# Make all of the columns with '%' '$' etc to proper numerics
myData <- myData %>%
  mutate(across(2:45, ~ if(is.character(.)) parse_number(.) else .)) %>%
  filter(Member %in% c("scottSummers", "charlesXavier")) %>%
  select(Member, PPI60s_ebay, PPI70s_ebay, PPI80s_ebay, PPI80s_ebay, PPI90s_ebay) %>%
  pivot_longer(cols = -Member, names_to = "Decade", values_to = "ppi_ebay") %>%
  mutate(
    Decade  =  case_when(
      Decade == "PPI60s_ebay" ~ "60s",
      Decade == "PPI70s_ebay" ~ "70s",
      Decade == "PPI80s_ebay" ~ "80s",
      Decade == "PPI90s_ebay" ~ "90s"
      ),
    Member = if_else(Member == "scottSummers", "Cyclops", "Professor X")
  )



# Set Up Some Aesthetic Elements -----------------------------------------------

## Save color palette


colColour <- "#ffb400"

back_colour =  "grey10"
strong_text = "#a30303"
weak_text = darken(strong_text, 0.05)


## Fonts

# Title font
font_add_google("Bangers", "Bangers")

#Caption Font
font_add(family = "Roboto",
         regular = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-REGULAR.ttf",
         bold = "C:/USERS/GVAND/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/ROBOTO-BOLD.ttf")

# Make the fonts work
showtext_auto()

main_font = "Bangers"


# The Actual Plot --------------------------------------------------------------

ggplot(myData, aes(x = Decade, y = ppi_ebay)) +
  geom_chicklet(stat = "identity", position = "dodge", 
           fill = colColour, color = colColour, 
           radius = grid::unit(0.5, "mm")) + # Very Slightly Rounded
  coord_flip() + 
  facet_wrap(Member ~ ., ncol = 1) +
  labs(title = "Mutant Moneyball", 
       subtitle = "Do X-Men Comics Featuring Cyclops or Professor X Sell for More on Ebay?",
       caption = paste(" <b>Data: </b> Mutant Moneyball Data",
       " \n <b>Graphic: </b> Gregory Vander Vinne")) +
  ylab("Average Price Per Issue Sold on Ebay") + 
  xlab("Decade in Which the Issue Was Released") + 
  geom_richtext(aes(label= scales::dollar(round(ppi_ebay))), 
                hjust = 1,
                nudge_y = if_else(myData$ppi_ebay > 18, 0, 18),
                color = if_else(myData$ppi_ebay > 15, back_colour, colColour),
                fill = NA,
                label.colour = NA,
                family = main_font,
                fontface = "bold",
                size = 4,
  ) + 
  theme_minimal(base_size = 10)+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = back_colour,
                                    color = back_colour),
    plot.background = element_rect(fill = back_colour, 
                                   colour = back_colour),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = rel(4),
                                        family = main_font,
                                        face = "bold",
                                        color = strong_text,
                                        margin = margin(8, 0, 12, 4)),
    plot.subtitle = element_textbox_simple(size = rel(1.4),
                                           family = "Roboto",
                                           face = "bold",
                                           colour = weak_text,
                                           margin = margin(0, 0, 12, 4)), 
    axis.title.x = element_text(size = rel(1.3),
                                family = "Roboto",
                                colour = strong_text, 
                                face = "bold",
                                margin = margin(4, 0, 4, 0)),
    axis.title.y = element_text(size = rel(1.3),
                                family = "Roboto",
                                face = "bold",
                                colour = strong_text, 
                                margin = margin(0, 0 , 0, 4)),
    axis.text.y = element_text(size = rel(1.1),
                               family = "Roboto",
                               face = "bold",
                               colour = weak_text, 
                               margin = margin(0, -8, 0, 10)),
    axis.text.x = element_blank(),
    plot.caption = element_markdown(size = rel(0.8),
                                    colour = weak_text,
                                    family = "Roboto",
                                    # family = "Bangers",
                                    hjust = 1, 
                                    margin = margin(8,0,0,8)),
    strip.text = element_text(family = main_font,
                              hjust = 0.5,
                              size = rel(1.8),
                              colour = strong_text),
    legend.position = "none",
    text = element_text(colour = strong_text, lineheight = 1.1)
  )


# For ggsave text sizing
showtext_opts(dpi = 300)
# Save plot
ggsave(here("2024-03-19/2024-03-19.png"))
