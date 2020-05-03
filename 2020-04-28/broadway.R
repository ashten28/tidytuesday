# libraries
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggtext) # remotes::install_github("wilkelab/ggtext")

# read data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

# define my pallete
my_palette <-
  c(darkgrey = "#575965", grey = "#868d99", lightgrey = "#e2e2e2", yellow = "#f5cc5a", white = "#fefefe", black = "#2e2528", black2 = "#0A0A0A", lightgrey2 = "#5b5b5b", green = "#3986A6")

# display my pallete
scales::show_col(my_palette)

# get top 10 shows in a vector
top_alltime_grosses <-
  grosses %>% 
  group_by(show) %>% 
  summarize(sum_weekly_gross = sum(weekly_gross, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(sum_weekly_gross)) %>% 
  top_n(10) %>% 
  pull(show) 

# make plot data 
top_grosses_trend <-
  grosses %>% 
  filter(show %in% top_alltime_grosses) %>%
  arrange(show, week_ending) %>% 
  group_by(show) %>% 
  # calculate cumulative weekly gross
  mutate(cumsum_weekly_gross = cumsum(weekly_gross)) %>% 
  ungroup() %>% 
  # duplicate dataset for each show (for facet),
  mutate(show_facet = list(top_alltime_grosses)) %>% 
  select(show, week_ending, cumsum_weekly_gross, show_facet) %>% 
  unnest(cols = show_facet) %>% 
  mutate(
    show = fct_relevel(show,top_alltime_grosses),
    show_facet = fct_relevel(show_facet, top_alltime_grosses),
    # create column to identify which show to highlight and size
    highlight = if_else(show == show_facet, "YES", "NO"),
    line_size = if_else(show == show_facet, 1, 0.5)
    ) %>% 
  mutate(
    show_facet = fct_recode(
      show_facet,
      "The Phantom\nof the Opera" = "The Phantom of the Opera",
      "The Book\nof Mormon"  = "The Book of Mormon"
    )
  )

# ggplot
ggplot(top_grosses_trend) +
  # make lines for each show
  geom_line(aes(x = week_ending, y = cumsum_weekly_gross, group = show, colour = highlight, size = line_size)) + 
  # make individual plots for each show
  facet_wrap(~show_facet, nrow = 2, as.table = T) +
  # custom colours applied to highlight
  scale_colour_manual(values = c(paste0(my_palette[[2]], "50"), my_palette[[9]])) +
  # specify the line size range
  scale_size(range = c(1, 1.5)) +
  # clean up y-axis labels
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6), position = "right") +
  # show only min and max years on x axis
  scale_x_date(breaks = c(min(top_grosses_trend$week_ending), max(top_grosses_trend$week_ending)), date_labels = "%Y") +
  theme_bw() + 
  # add lables: this requires ggtext
  labs(
    title = "**Cumulative weekly gross of top 10 grossing Broadway <span style='color:#3986A6;'>shows</span>.**",
    subtitle = "The Lion King has the largest gross and The Phantom of the Opera has the longest run",
    # y = "Cumulative weekly gross\n",
    caption = "Data: www.playbill.com  |  Github: ashten28"
  ) +
  # theme
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    # axis.title.y = element_text(colour = my_palette[[7]], hjust = 1, size = 12),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = my_palette[[8]]),
    strip.text = element_text(colour = my_palette[[9]], face = "bold", hjust = 0, vjust = 1, size = 13),
    strip.background = element_blank(),
    plot.title = element_markdown(lineheight = 1.3, size = 21),
    plot.subtitle = element_markdown(lineheight = 2, size = 15),
    plot.caption = element_text(hjust = 0, size = 11.5, colour = my_palette[[6]]),
    # plot.caption = element_markdown(hjust = 0.5, size = 11.5, colour = my_palette[[6]]),
    axis.text = element_text(colour = my_palette[[8]], size = 11),
    plot.background = element_rect(fill = my_palette[[3]]),
    panel.background = element_rect(fill= my_palette[[3]]),
    axis.line.x = element_line(colour = my_palette[[2]]),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.x = unit(4, "mm") 
  )

# save ggplot
ggsave(filename = here("2020-04-28/broadway_plot.png"), width = 16, height = 9)
