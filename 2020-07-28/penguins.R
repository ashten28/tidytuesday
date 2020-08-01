# libraries
library(here)
library(readr)
library(dplyr)
library(forcats)
library(stringr)
library(ggplot2)
library(ggbump)
library(ggtext) # remotes::install_github("wilkelab/ggtext")

# read data
penguins <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv') %>% 
  mutate_if(is.character, as.factor)

# define my pallete
my_palette <-
  c(
    "#575965", "#868d99", "#e2e2e2", "#f5cc5a", "#fefefe", "#2e2528", "#0A0A0A", "#5b5b5b", "#3986A6",
    "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99" ,"#999933", "#882255", 
    "#661100", "#6699CC", "#888888"
    
    )

# display my pallete
scales::show_col(my_palette)

# ggplot
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(
    mapping = aes(x = bill_depth_mm, y = bill_length_mm, colour = sex)
  ) + 
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method='lm', formula= y~x, se= FALSE) +
  facet_wrap(~species) + 
  labs(
    title = "**<span style='color:#3986A6;'>Palmer Penguins</span>**",
    subtitle = "Generally, <span style='color:#999933;'>male</span> penguins have longer and deeper bills then <span style='color:#AA4499;'>female</span> penguins",
    caption = "Source:  Dr. Kristen Gorman  |  Github: ashten28"
  )  + 
  xlab("Bill Depth (mm)") + 
  ylab("Bill Length (mm)") +
  scale_colour_manual(
    values = c(my_palette[[15]], my_palette[[17]])
  ) +
  theme_bw() + 
  theme(
    legend.position = "none",
    # axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    # axis.ticks = element_blank(),
    strip.text = element_text(colour = my_palette[[9]], face = "bold", hjust = 0, vjust = 1, size = 13),
    strip.background = element_blank(),
    plot.title = element_markdown(lineheight = 1.3, size = 21),
    plot.subtitle = element_markdown(lineheight = 2, size = 15),
    plot.caption = element_text(hjust = 0, size = 11.5, colour = my_palette[[6]]),
    plot.background = element_rect(fill = my_palette[[3]]),
    panel.background = element_rect(fill= my_palette[[3]]),
    panel.border = element_rect(colour = my_palette[[2]])
    # panel.grid = element_blank() 
  )

# save ggplot
ggsave(filename = here("2020-07-28/penguins_plot.png"), width = 16, height = 9)
