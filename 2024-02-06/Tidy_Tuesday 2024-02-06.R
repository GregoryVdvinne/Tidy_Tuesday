# Clear memory
rm(list = ls(all=T))

devtools::install_github("jimjam-slam/ggflags")

# Load packages and install if not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # grammar of data and graphics
  here,           # relative file pathways
  showtext,       # custom fonts
  ggtext,         # fancy text in plots
  colorspace,     # fancy stuff with colors
  glue,           # text concatenation w/ formatting
  ggalluvial,     # flows between bars etc, 
  ggflags         # points that look like flags
)


######## Load and Wrangle Data ########

# Read Tidy Tuesday data set directly from github
myData_wide <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')
myData_wide <- myData_wide %>% 
  mutate(abv = c("NO", "DK", "SE"), chg = `2022`-`2004`)

# Re-shape the data and get year-specific rank f/ number of sites
myData_long <- myData_wide %>%
  pivot_longer(cols = c(`2004`,`2022`), 
               values_to = "sites", names_to = "year") %>%
  group_by(year) %>%
  mutate(rank = rank(-sites)) %>%
  arrange(desc(rank)) %>%
  mutate(label_height = cumsum(sites)-(sites/2), 
         year = as.numeric(year))


######## Set Up For Plotting ########

# Set up a color scheme
myPal <- c( "#EE5440", "#283250","#3274D8")
flagPal <-  c("#ED4C5C","#418BC1","#FFE62E" )
back_colour = "white"
line_colour = "#D7DDE1"
weak_text = "#7A8092"
strong_text = darken(weak_text, 0.5)


# Add custom font family
font_add_google("Roboto", "roboto")
showtext_auto()


# Define Theme
my_theme <- function(base_size = 24) {
  theme_minimal(base_size = base_size)+
    theme(
      legend.position = "none",
      plot.caption.position = "plot",
      plot.title.position = "plot",
      panel.background = element_rect(fill = back_colour, color = back_colour),
      plot.background = element_rect(fill = back_colour),
      plot.title = element_textbox_simple(size = rel(2.25),
                                          family = "roboto",
                                          # face = "bold",
                                          color = strong_text,
                                          margin = margin(5, 0, 8, 0)),
      plot.subtitle = element_textbox_simple(size = rel(1.1),
                                             family = "roboto",
                                             colour = weak_text,
                                             margin = margin(0, 0, 20, 0), 
                                             lineheight=0.4),
      line = element_blank(),
      axis.title = element_blank(),
      axis.text= element_blank(),
      plot.caption = element_markdown(size = rel(0.8),
                                      colour = weak_text,
                                      family = "roboto",
                                      hjust = 0),
      legend.text = element_text(size = rel(1),
                                 family = "roboto",
                                 colour = strong_text),
      legend.title = element_text(size = rel(1),
                                  family = "roboto",
                                  colour = weak_text),
      text = element_text(colour = weak_text, lineheight = 0.5)
    )
}

######## The Alluvial Plot ########

ggplot(myData_long, 
       aes(x = year, y = sites,
           stratum = rank, 
           alluvium = country, 
           fill = country,
           colour = country,
           label = sites)) +
 
  geom_segment(x = 2004, xend = 2004,
               y = 0, yend = 35, 
               color = line_colour, linewidth = 0.75) + 
  geom_segment(x = 2022, xend = 2022,
               y = 0, yend = 35, 
               color = line_colour, linewidth = 0.75) + 
  geom_stratum(width = 0.2, color = "white") +
  geom_flow(stat = "alluvium",
            knot.pos = 0.25, # dictates how curvey it is
            width = -0.2, # dictates distance between flow and stratum
            alpha = 0.9,
            color = "white") +
  geom_text(
    data = data.frame(
      x = c(2004, 2022),
      y = c(36,36)
    ),
    inherit.aes = FALSE,
    mapping = aes(x = x, y = y, label = x),
    family = "roboto",
    colour = strong_text,
    size = 6.5*2.4,
    lineheight = 0.3
  ) +
  geom_text(inherit.aes = FALSE,
            aes(y = label_height, label = if_else(year == 2022, abv, "")), x = 2020,
            family = "roboto",
            colour = "white",
            size = 6*2.4) + 
  geom_text(inherit.aes = FALSE,
            aes(y = label_height, x = if_else(year == min(year), year-0.7, year+0.7),
                label = sites, color = country),
            family = "roboto",
            # colour = weak_text,
            size = 6*2.4) + 
  scale_fill_manual(values = myPal) +
  scale_color_manual(values = myPal) +
  scale_x_continuous(limits = c(2002,2024)) +
  labs(title = "Number of Unesco World Heritage Sites in Denmark, Norway, and Sweden", 
       caption = glue("<b> Data: </b> Unesco.org",
                      "  \n <b> Graphic: </b> Gregory Vander Vinne")) + 
  my_theme()

ggsave(here("2024-02-06/2024-02-06_alluvial.png"))


######## The ?Staircase? Plot ########

steps <- data.frame(steps = seq(0.5,18, by = 1), 
                    numbers  = seq(1,18, by = 1))

ggplot() + 
  geom_flag(data = myData_long, aes(x = sites, y = sites, 
                                    country = tolower(abv)),
            size = 5.7) + 
  geom_curve(data = myData_wide, 
             aes(x = `2004`, xend = `2022`-0.3,  
                 y = `2004` + 0.75, yend = `2022` + 0.75, 
                 color = country), 
             curvature = -0.75, 
             angle = 60,
             arrow = arrow(length = unit(0.03, "npc")),
             linewidth = 0.7) +
  geom_step(data = steps, aes(x = steps, y=steps), color = strong_text) +
  geom_text(data = steps, aes(x = numbers, y = numbers-1.25, label =  numbers),
            fontface = if_else(steps$numbers %in% myData_wide$`2022`, "bold", "plain"),
            color = strong_text, size = 6*2.4, family = "Roboto") + 
  labs(title = "Number of Unesco World Heritage Sites in 2004 and 2022",
       caption = glue("<b> Data: </b> Unesco.org",
                      "  \n <b> Graphic: </b> Gregory Vander Vinne")) + 
  scale_color_manual(values = flagPal) +
  my_theme()


ggsave(here("2024-02-06/2024-02-06_staircase.png"))

