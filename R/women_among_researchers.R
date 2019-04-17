library(tidyverse)
library(shadowtext)
library(extrafont)

loadfonts(device = "win")

women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

palete = c(
  rgb(55, 133, 162, maxColorValue = 255),
  rgb(187, 34, 77, maxColorValue = 255),
  rgb(45, 189, 23, maxColorValue = 255),
  rgb(222, 112, 60, maxColorValue = 255),
  "#433751"
)

font = "Tw Cen MT"

women_research %>%
  filter(field != "Women inventores") %>%
  rename(pct = percent_women) %>%
  group_by(country) %>%
  mutate(pct_sum = sum(pct)) %>%
  spread(field, pct) %>%
  ungroup() %>%
  arrange(pct_sum) %>%
  mutate(
    ymax = cumsum(pct_sum),
    ymin = ymax - pct_sum,
    ylab = (ymax + ymin) / 2
  ) %>%
  gather(field, pct, 3:6) %>%
  group_by(country) %>%
  arrange(desc(pct)) %>%
  mutate(
    field = field %>% tolower() %>% reorder(desc(pct)),
    pct_n = pct / sum(pct),
    xmax = cumsum(pct_n),
    xmin = xmax - pct_n,
    xlab = xmax - pct_n / 2,
    bg_col = case_when(
      field == "health sciences" ~ palete[1],
      field == "engineering" ~ palete[2],
      field == "computer science, maths" ~ palete[3],
      field == "physical sciences" ~ palete[4]
    )
  ) %>%
  ungroup() %>%
  ggplot() +
  geom_rect(aes(
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = max(ymax)
    ),
    fill = "white"
  ) +
  geom_rect(aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = field,
      alpha = pct
    ),
    size = .2,
    color = "white"
  ) +
  geom_shadowtext(aes(
      x = xlab,
      y = ylab,
      label = round(pct * 100) %>% str_c("%"),
      bg.color = bg_col
    ),
    family = font,
    size = 2.2
  ) +
  geom_text(aes(
      x = -.03,
      y = ylab,
      label = country
    ),
    size = 2.6,
    family = font,
    color = "white",
    hjust = 1,
    check_overlap = TRUE
  ) +
  geom_text(aes(
      x = 0,
      y = max(ymax) * 1.26,
      label = "Still a man's world"
    ),
    color = "white",
    hjust = 0,
    family = font,
    size = 4.3
  ) +
  geom_text(aes(
      x = 0,
      y = max(ymax) * 1.15,
      label = "Women among researchers with papers published 2011-2015\n% of total in:"
    ),
    lineheight = .9,
    color = "white",
    hjust = 0,
    family = font,
    size = 2.8
  ) +
  scale_fill_manual(values = palete[1:4]) +
  scale_alpha_continuous(range = c(.4, 1)) +
  guides(alpha = FALSE) +
  theme_void() +
  theme(
    text = element_text(color = "white", size = 8, family = font),
    plot.background = element_rect(fill = palete[5]),
    legend.title = element_blank(),
    legend.position = c(.524, .81),
    legend.direction = "horizontal",
    legend.spacing.x = unit(1.5, "mm"),
    plot.margin = margin(t = 5),
    legend.key.size = unit(2.5, "mm")
  ) +
  xlim(-.2, NA)

ggsave(
  filename = "plots/women_research.png",
  width = 12,
  height = 8,
  units = "cm",
  dpi = 600
)
