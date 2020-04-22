library(tidyverse)

gdpr_violations <- 
  readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv') %>% 
  arrange(-price)

# get countries in vector
gdpr_countries <- 
  gdpr_violations %>% 
  distinct(name) %>% 
  pull()


# save ggplot
ggsave(, dpi = 320, width = 14, height = 11)

