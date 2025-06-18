pacman::p_load(
  tidyverse,
  ggplot2,
  plotly,
  shiny,
  readxl
)

tabla <- read_excel("data/tavid2000-2150.xls")
tabla <- as.data.frame(lapply(tabla, as.numeric), stringsAsFactors = FALSE)