for (font in fonts()) {
  ggplot(data = tibble(x = 1)) +
    geom_text(aes(
      x = 0,
      y = 0,
      label = 'Paweł Chabros'
    ), family = font
    ) +
    ggtitle(font) +
    theme_void() -> plot
  print(plot)
}

fonts <- c(
  "Viner Hand ITC",
  "Tw Cen MT",
  "Tempus Sans ITC",
  "Segoe Script",
  "Script MT Bold",
  "Rockwell Condensed",
  "Rage Italic",
  "Monotype Corsivia",
  "Lucida Calligraphy",
  "Leelawadee UI Semilight",
  ""
)
