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
marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

# define my pallete
my_palette <-
  c(darkgrey = "#575965", grey = "#868d99", lightgrey = "#e2e2e2", yellow = "#f5cc5a", white = "#fefefe", black = "#2e2528", black2 = "#0A0A0A", lightgrey2 = "#5b5b5b", green = "#3986A6")

# display my pallete
scales::show_col(my_palette)

# preliminary analysis
skimr::skim(marbles %>% mutate_if(is.character, as.factor))

# summarize and rank by team name and races
marbles_rank <-
  marbles %>% 
  filter(str_detect(race, "R")) %>% 
  group_by(race, team_name) %>% 
  summarize(points = last(points)) %>% 
  ungroup() %>% 
  group_by(team_name) %>% 
  arrange(team_name, race) %>% 
  mutate(
    points_sum = sum(points),
    points_cum = cumsum(points)
  ) %>%
  ungroup() %>% 
  group_by(race) %>% 
  arrange(-points_cum, points_sum) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  mutate(
    race_num = as.numeric(str_remove(race, "S1R")),
    team_name = fct_reorder(team_name, -points_sum)
  )

marbles_rank_firstrace <- 
  marbles_rank %>% 
  filter(race_num == 1) %>% 
  select(team_name) %>%  
  mutate(half  = if_else(row_number() <= 8, "tophalf", "bottomhalf"))

marbles_rank_add <-
  left_join(
    x = marbles_rank, 
    y = marbles_rank_firstrace,
    b = "team_name"
  )
    
gen_pal <- colorRampPalette(colors = my_palette[c(9, 6)])

# ggplot
ggplot(
  data = marbles_rank_add
) +
  geom_bump(
    mapping = aes(x = race_num, y = rank, colour = team_name),
    size = 2
  ) + 
  geom_text(
    data = . %>% filter(race_num == min(race_num)),
    mapping = aes(x = 0.9, y = rank, colour = team_name, label = paste0(team_name, " (", rank, ")"), hjust = 1)
  ) + 
  geom_text(
    data = . %>% filter(race_num == max(race_num)),
    mapping = aes(x = 8.1, y = rank, colour = team_name, label = paste0("(", rank, ") ", team_name), hjust = 0)
  ) + 
  geom_point(
    data = . %>% filter(race_num %in% c(1,8)), 
    mapping = aes(x = race_num, y = rank, shape = half), 
    size = 4
  ) +
  scale_x_continuous(
    limits = c(0, 9), 
    breaks = 1:8,
    labels = c(glue::glue("Race {1:8}")),
    ) +
  scale_color_manual(values = c("#88CCEE","#CC6677","#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99" ,"#999933", "#882255", "#661100", "#6699CC", "#888888", my_palette[[1]], "#000000", my_palette[[8]], my_palette[[9]])) +
  # scale_colour_manual(values = gen_pal(16)) + 
  scale_y_reverse(
    # expand = c(.03, .03),
    breaks = 1:16
  ) +
  labs(
    title = "**<span style='color:#3986A6;'>Marbula One</span> race ranking over time**",
    subtitle = "Raspberry Racers and Team Momo are the only two teams to finish in different rank halves than they started in.",
    caption = "Source: Jelle's Marble Run  |  Github: ashten28"
  )  + 
  theme_bw() + 
  theme(
    legend.position = "none",
    axis.title = element_blank(),
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
    panel.border = element_rect(colour = my_palette[[2]]),
    panel.grid = element_blank() 
  )
  

# save ggplot
ggsave(filename = here("2020-06-02/marble_plot.png"), width = 16, height = 9)
