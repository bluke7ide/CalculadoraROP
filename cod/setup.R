pacman::p_load(
  tidyverse,
  ggplot2,
  plotly,
  shiny,
  readxl,
  bslib,
  shinycssloaders,
  shinydashboard
  
)

tabla <- read_excel("data/tavid2000-2150.xls")
tabla <- as.data.frame(lapply(tabla, as.numeric), stringsAsFactors = FALSE)

source("cod/retiro_programado.R")
source("cod/renta_permanente.R")
source("cod/intereses.R")