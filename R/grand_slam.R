# setup ----

library(tidyverse)
library(shadowtext)
library(magrittr)
library(wrapr)

extrafont::loadfonts(device = "win")

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

font <- "Tw Cen MT"
paleta <- c(
  "#e4508f",
  "#556fb5",
  "#fffeec",
  "#aeddcd",
  rgb(40, 98, 78, maxColorValue = 255)
)

# Plot ----

grand_slam_clean <-
  grand_slam_timeline %>%
  filter(outcome %in% c("Won", "Finalist")) %>%
  group_by(player) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    won = map_int(data, ~ sum(.x$outcome == "Won")),
    gender = map_chr(data, ~ unique(.x$gender))
  ) %>%
  top_n(10, n) %>%
  mutate(
    pct = won / n,
    pct_pos = pct * max(n) * 1. + max(n) * .6,
    player = player %>% str_replace("Evonne Goolagong", "E. G.") %>% reorder(n)
  ) %>%
  arrange(player) %>%
  mutate(xpos = row_number())

legend <- 
  grand_slam_clean %$%
    tibble(
      x_pos = seq(
        mean(xpos) - 1.8,
        mean(xpos) + 1.8,
        length.out = 5
      ),
      y_pos_sym = max(pct_pos) * 1.18,
      y_pos_txt = max(pct_pos) * 1.23,
      labels = c("Female", "Male", "Finals", "Finals won", "Win rate") %>% rev(),
      shape = c(16, rep(15, 4)),
      alpha = c(1, 1, .5, 1, 1)
    )

grand_slam_clean %.>%
  ggplot(data = ., aes(
    x = xpos,
    fill = gender
  )) +
  geom_col(
    aes(y = n),
    alpha = .5
  ) +
  geom_col(
    aes(y = won),
    width = .9
  ) +
  geom_point(
    aes(y = pct_pos),
    color = paleta[3],
    shape = 21,
    size = 8,
    stroke = .8    
  ) +
  geom_text(aes(
      y = pct_pos,
      label = round(pct * 100)
    ),
    family = font,
    color = paleta[3]
  ) +
  geom_shadowtext(aes(
      y = pct_pos + max(pct_pos) * .065,
      color = gender,
      label = "%"
    ),
    family = font,
    bg.color = paleta[3]
  ) +
  geom_rect(
    data = legend,
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = min(y_pos_sym) * .95,
      ymax = Inf
    ),
    fill = paleta[3],
    inherit.aes = FALSE
  ) +
  geom_point(
    data = legend,
    aes(
      x = x_pos,
      y = y_pos_sym,
      shape = shape,
      alpha = alpha
    ),
    color = c(paleta[c(1, 2, 5, 5, 5)]) %>% rev(),
    size = 3.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = legend,
    aes(
      x = x_pos,
      y = y_pos_txt,
      label = labels
    ),
    family = font,
    size = 3.2,
    hjust = 0,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = .$xpos,
    labels = .$player
  ) +
  geom_shadowtext(
    data = legend,
    aes(
      x = min(x_pos),
      y = y_pos_sym * 1.025,
      label = "%"
    ),
    inherit.aes = FALSE,
    color = paleta[5],
    family = font,
    size = 2,
    bg.color = paleta[3],
    check_overlap = TRUE
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    labels = function(x) if_else(x > 30, "", as.character(x)),
    expand = expand_scale(mult = c(0, .2))
  ) +
  ggtitle(
    label = "Top 10 Grand Slam finalists",
    subtitle = "(number of appearances)"
  ) +
  scale_shape_identity() +
  scale_alpha_identity() +
  scale_fill_manual(values = paleta[1:2]) +
  scale_color_manual(values = paleta[1:2]) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(margin = margin(r = 5)),
    panel.grid = element_line(colour = alpha(paleta[4], .3)),
    panel.grid.minor.y = element_blank(),
    text = element_text(family = font),
    legend.position = "none",
    axis.title = element_blank(),
    plot.background = element_rect(fill = paleta[3])
  ) +
  
ggsave(
  filename = "plots/grand_slam.png",
  width = 13,
  height = 8,
  units = "cm",
  dpi = 600
)

