library(tidyverse)
theme_set(theme_minimal(base_family = 'Harlow Solid Italic'))

board_games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

top_8_cat <-
  board_games$category %>%
  map(~ str_split(.x, pattern = ',')) %>%
  unlist() %>% 
  tibble(cat = .) %>%
  count(cat) %>%
  arrange(desc(n)) %>%
  slice(1:8) %>%
  pull(cat) %>%
  str_c(collapse = ')|(') %>%
  str_c('(', ., ')')

board_games %>%
  mutate(cat_top_8 = str_extract_all(category, top_8_cat)) %>%
  select(name, average_rating, cat_top_8) %>%
  unnest() %>%
  filter(!is.na(cat_top_8)) %>%
  group_by(cat_top_8) %>%
  mutate(med = median(average_rating)) %>%
  ungroup() %>%
  mutate(cat_top_8 = cat_top_8 %>% reorder(med)) %>%
  ggplot(aes(
    x = cat_top_8,
    y = average_rating,
    color = cat_top_8
  )) +
  geom_jitter(
    alpha = 0.2,
    size = 0.2
  ) +
  geom_boxplot(
    alpha = 0.8,
    size = 0.3,
    outlier.shape = NA
  ) +
  ylab('') +
  xlab('') +
  ggtitle('Average ratings of games from 8 most frequent categories') +
  coord_flip() +
  scale_color_brewer(palette = 'Dark2') +
  theme(
    legend.position = 'none',
    plot.background = element_rect(fill = '#f8f1f1', color = '#f8f1f1')
  )

ggsave(
  filename = 'board_games.png',
  width = 15,
  height = 10,
  units = 'cm',
  dpi = 600
)
