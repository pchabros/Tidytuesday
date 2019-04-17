library(tidyverse)
library(wrapr)

combined_data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

combined_data %>%
  group_by(state) %>%
  summarise(
    stop_rate = mean(stop_rate, na.rm = TRUE),
    arrest_rate = mean(arrest_rate, na.rm = TRUE)
  ) %>%
  drop_na() %>%
  mutate(
    delta = stop_rate - arrest_rate,
    state = state %>% reorder(delta)
  ) %.>%
  ggplot(
    data = .,
    aes(x = state)
  ) +
  geom_linerange(aes(
    ymin = arrest_rate,
    ymax = stop_rate,
    color = log(delta)
  )) +
  geom_point(
    data = gather(., rate, value, stop_rate:arrest_rate) -> gathered_data,
    aes(
      y = value,
      fill = rate
    ),
    shape = 21,
    color = 'white',
    size = 7
  ) +
  geom_text(
    data = mutate(gathered_data, lab = round(value, 2) %.>%
      if_else(. == 0.11, ' .11', as.character(.))),
    aes(
      y = value,
      label = lab
    ),
    family = 'Segoe UI Semilight',
    color = 'white',
    size = 2.2
  ) +
  coord_flip() +
  ggtitle(
    label = 'The Stanford Open Policing Project',
    subtitle = 'Arrest rate vs stop rate'
  ) +
  scale_fill_manual(values = c('#f9499e', '#68bde1')) +
  scale_color_gradient(low = '#f9499e', high = '#68bde1') +
  theme(
    axis.title = element_blank(),
    legend.position = 'none',
    text = element_text(family = 'Segoe UI Semilight'),
    panel.background = element_rect(fill = 'white'),
    panel.grid = element_line(color = '#f2f2f2'),
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )

ggsave(
  filename = 'SOPP.png',
  width = 16,
  height = 10,
  units = 'cm',
  dpi = 600
)
