# libraries
library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(ggtext) # remotes::install_github("wilkelab/ggtext")

# read data
cocktails <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv') %>% 
  mutate(alcoholic_fixed = if_else(alcoholic == "Non alcoholic", "Non Alcoholic", alcoholic))

# define my pallete
my_palette <-
  c(darkgrey = "#575965", grey = "#868d99", lightgrey = "#e2e2e2", yellow = "#f5cc5a", white = "#fefefe", black = "#2e2528", black2 = "#0A0A0A", lightgrey2 = "#5b5b5b", green = "#3986A6")

# display my pallete
scales::show_col(my_palette)

# preliminary analysis
skimr::skim(cocktails %>% mutate_if(is.character, as.factor()))

# ggplot
ggplot(
  data = 
    cocktails %>% 
    filter(alcoholic_fixed %in% c("Alcoholic", "Non Alcoholic"))
  ) +
  geom_bar(
    mapping = aes(x = category)
  ) + 
  facet_grid(~alcoholic_fixed) + 
  theme_bw() + 
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_text(angle = 90),
    axis.line = element_blank(),
    # axis.ticks = element_blank(),
    strip.text = element_text(colour = my_palette[[9]], face = "bold", hjust = 0, vjust = 1, size = 13),
    strip.background = element_blank(),
    plot.title = element_markdown(lineheight = 1.3, size = 21),
    plot.subtitle = element_markdown(lineheight = 2, size = 15),
    plot.caption = element_text(hjust = 0, size = 11.5, colour = my_palette[[6]]),
    plot.background = element_rect(fill = my_palette[[3]]),
    panel.background = element_rect(fill= my_palette[[3]]),
    panel.border = element_rect(colour = my_palette[[2]]),
    panel.grid = element_blank() 
  )
  
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
