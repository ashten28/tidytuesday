# libraries
library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(ggtext) # remotes::install_github("wilkelab/ggtext")
library(maps)

# read data
volcano   <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events    <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

# define my pallete
my_palette <-
  c(darkgrey = "#575965", grey = "#868d99", lightgrey = "#e2e2e2", yellow = "#f5cc5a", white = "#fefefe", black = "#2e2528", black2 = "#0A0A0A", lightgrey2 = "#5b5b5b", green = "#3986A6")

# display my pallete
scales::show_col(my_palette)

# get map data - polygon
world_map <- 
  map_data("world") %>% 
  mutate(paint = if_else(region == "Malaysia", "YES", "NO"))

# ggplot
ggplot(data = volcano) +
  # geometrics (polygon - country border, point - volcano)
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group, fill = factor(paint)), colour = paste0(my_palette[[1]], 80), size = 0.1) + 
  geom_point(aes(y = latitude, x = longitude), shape = 17, colour = "darkred", size = 2) +
  scale_fill_manual(values = c(paste0(my_palette[[9]], "30"), my_palette[[9]])) +
  coord_sf(xlim = c(90.74, 129.5), ylim = c(-9.9, 23), expand = FALSE) + # crop map to desired area
  labs(
    title = "**Volcanos around <span style='color:#3986A6;'>Malaysia</span>.**",
    caption = "Source: The Smithsonian Institution  |  Github: ashten28"
  ) +
  theme_bw() + 
  # theme
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_markdown(lineheight = 1.3, size = 21),
    plot.subtitle = element_markdown(lineheight = 2, size = 15),
    plot.caption = element_text(hjust = 0, size = 11.5, colour = my_palette[[6]]),
    plot.background = element_rect(fill = my_palette[[3]]),
    panel.background = element_rect(fill= my_palette[[3]]),
    panel.border = element_rect(colour = my_palette[[2]]),
    panel.grid = element_blank() 
  )

# save ggplot
ggsave(filename = here("2020-05-12/volcano_plot.png"), width = 9, height = 9)
