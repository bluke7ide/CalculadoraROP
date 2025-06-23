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

tabla <- read_excel("tavid2000-2150.xls")
tabla <- as.data.frame(lapply(tabla, as.numeric), stringsAsFactors = FALSE)

source("retiro_programado.R")
source("renta_permanente.R")
source("intereses.R")