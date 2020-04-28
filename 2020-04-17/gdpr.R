# libraries
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)

# read data
gdpr_violations <- 
  readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

# get fine amounts by country, recode a few levels, add a column for price_labels
countries_prices <- 
  gdpr_violations %>% 
  group_by(name) %>% 
  summarize(
    price = sum(price, na.rm = TRUE),
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    name = recode(name, `Czech Republic` = "Czech", `United Kingdom` = "UK"),
    price_label = paste0(format(round(price / 1e3, 0), nsmall=0, big.mark=","), "k")
    
    ) %>% 
  filter(price > 0)

# get proportion of Google Inc. contribution in France figures
france_google_prop <-
  gdpr_violations %>% 
  filter(name == "France") %>% 
  group_by(name) %>% 
  mutate(prop = (price/sum(price)*100)) %>% 
  filter(controller == "Google Inc.") %>% 
  pull(prop)

france_google_prop
# [1] 97.84736

# relevel country level in the order of the fine count
countries_prices_relevel <-
  countries_prices %>% 
  mutate(
    name = fct_relevel(name, countries_prices$name[order(countries_prices$n)])
    )

# define my pallete
my_palette <-
  c(darkgrey = "#575965", grey = "#868d99", lightgrey = "#e2e2e2", yellow = "#f5cc5a", white = "#fefefe", black = "#2e2528", black2 = "#0A0A0A", lighgrey2 = "#5b5b5b", green = "#3986A6")

# ggplot
ggplot(countries_prices_relevel, aes(x =name, y = n)) +
  # dotted grey lines to point to total fine amount by country
  geom_segment(
    aes(x = name, xend = name, y = n + 2, yend = 69.4),
    colour = my_palette[[8]],
    linetype = "dashed",
    alpha = 0.3
  ) +
  # lolipop sticks connected to bubb;e
  geom_segment(
    aes(x = name, xend = name, y = 0, yend = n),
    colour = my_palette[[7]]
  ) +
  # bubble position representing fine count and its size represent total fine amount
  geom_point(
    mapping = aes(size = price),
    color  = my_palette[[7]],
    fill   = alpha(my_palette[[9]], 1),
    shape  = 21, 
    stroke = 1.5
  ) + 
  # total fine amount figure on the right of the plot
  geom_text(
    mapping = aes(y = 70, label = price_label),
    nudge_y = 4.3,
    colour = my_palette[[9]],
    hjust = 1,
    size = 6
  ) + 
  # add title and subtitle (ggtext)
  labs(
    title = "**GDPR <span style='color:#3986A6;'> total fine amount (€)</span> and counts by country**",
    subtitle = "France recorded the largest total fine amount and Spain recorded the highest fine count",
    y = "Fine count"
  ) +
  # comment box regarding Google Inc. fine proportion in France
  annotate(
    "richtext",
    x = 8.5, y = 13,
    hjust = 0, vjust = 1,
    label = 
      "<span style='font-size:19pt'>France suffered the largest total fine amount where <br>
       <span style='color:#4285f4;'>G</span><span style='color:#ea4335;'>o</span><span style='color:#fbbc05;'>o</span><span style='color:#4285f4;'>g</span><span style='color:#34a853;'>l</span><span style='color:#ea4335;'>e</span>
       was responsible for 98% of it"
  ) + 
  # line pointer
  annotate(
    "curve",
    x = 8.5,
    xend = 10,
    y = 13,
    yend = 7.5,
    curvature = 0.25,
    size = 0.9,
    arrow = arrow(length = unit(2, "mm"))
    ) +
  # set limits and range of fine count axis
  scale_y_continuous(breaks = seq(0, 80, b = 10), expand = c(0,0), limits = c(0, 75)) +
  scale_size(range = c(2, 15)) +
  # flip x axis and y axis
  coord_flip() +
  theme_bw() + 
  # theme
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_text(colour = my_palette[[7]], hjust = 0.5, size = 16),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = my_palette[[8]]),
    plot.title = element_markdown(lineheight = 1.3, size = 21),
    plot.subtitle = element_markdown(lineheight = 1.3, size = 20),
    axis.text = element_text(colour = my_palette[[8]], size = 16),
    plot.background = element_rect(fill = my_palette[[3]]),
    panel.background = element_rect(fill= my_palette[[3]]),
    axis.line.x = element_line(colour = my_palette[[2]]),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )

# save ggplot
ggsave(filename = "2020-04-17/gdpr_plot.png", width = 16, height = 11)
