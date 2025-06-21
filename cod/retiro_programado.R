vanuTemporal <- function(edad_inicio, sexo, tabla, r = 0.036, m = 12, anno_objetivo = 2025) {
  # Obtener esperanza de vida a los 65 años
  fila_ex65 <- tabla[tabla$edad == edad_inicio & tabla$sex == sexo, ]
  annos_disp <- unique(fila_ex65$year)
  anno_base <- annos_disp[which.min(abs(annos_disp - anno_objetivo))]
  ex65 <- fila_ex65$ex[fila_ex65$year == anno_base][1]
  resultados <- data.frame()
  for (edad in edad_inicio:90) {
    n <- ceiling(ex65) - (edad - edad_inicio)
    if (n <= 0) {
      resultados <- rbind(resultados, data.frame(Edad = edad, VANU_a_ex65 = 0))
      next
    }
    px <- numeric(n + 1)
    px[1] <- 1
    for (t in 1:n) {
      edad_t <- edad + t - 1
      anno_t <- anno_base + t - 1
      qx_t <- tabla$qx[tabla$edad == edad_t & tabla$sex == sexo & tabla$year == anno_t]
      qx_t <- ifelse(length(qx_t) == 0 || is.na(qx_t), 1, qx_t)
      px[t + 1] <- px[t] * (1 - qx_t)
    }
    v <- 1 / (1 + r)
    parte1 <- sum(px[1:n] * v^(0:(n - 1)))
    parte2 <- (m - 1) / (2 * m) * (1 - px[n + 1] * v^n)
    vanu <- (parte1*12 - parte2*12)
    resultados <- rbind(resultados, data.frame(Edad = edad, VANU_a_ex65 = round(vanu, 9)))
  }
  return(resultados)
}

vanuCompleta <- function(edad_inicio, sexo, tabla, r = 0.036, m = 12, anno_objetivo = 2025) {
  resultados <- data.frame()
  for (edad in edad_inicio:120) {
    fila_ex <- tabla[tabla$edad == edad & tabla$sex == sexo, ]
    annos_disp <- unique(fila_ex$year)
    anno_base <- annos_disp[which.min(abs(annos_disp - anno_objetivo))]
    ex <- fila_ex$ex[fila_ex$year == anno_base][1]
    n <- 120 - edad
    if (n <= 0) {
      resultados <- rbind(resultados, data.frame(Edad = edad, VANU = 0))
      next
    }
    px <- numeric(n + 1)
    px[1] <- 1
    for (t in 1:n) {
      edad_t <- edad + t - 1
      anno_t <- anno_base + t - 1
      qx_t <- tabla$qx[tabla$edad == edad_t & tabla$sex == sexo & tabla$year == anno_t]
      qx_t <- ifelse(length(qx_t) == 0 || is.na(qx_t), 1, qx_t)
      px[t + 1] <- px[t] * (1 - qx_t)
    }
    v <- 1 / (1 + r)
    parte1 <- sum(px[1:n] * v^(0:(n - 1)))
    parte2 <- (m - 1) / (2 * m) * (1 - px[n + 1] * v^n)
    vanu <- parte1 * m - parte2 * m
    resultados <- rbind(resultados, data.frame(Edad = edad, VANU = round(vanu, 9)))
  }
  return(resultados)
}

graficos <- function(resultados) {
  # 1. Gráfico de Reserva Final (como gráfico de barras)
  g1 <- ggplot(resultados, aes(x = Edad, y = Reserva_Final)) +
    geom_col(width = 0.7, fill = "steelblue") +
    labs(
      title = "Reserva de Pensión",
      x = "Edad",
      y = "Reserva Final (colones)"
    ) +
    theme_minimal()
  
  # 2. Gráfico de Pensión Mensual
  g2 <- ggplot(resultados, aes(x = Edad, y = Pension_Mensual)) +
    geom_line(color = "steelblue", size = 1.2) +
    labs(
      title = "Pensión Mensual Pagada por Año",
      x = "Edad",
      y = "Pensión Mensual (colones)"
    ) +
    theme_minimal()
  
  return(list(
    Reserva = ggplotly(g1),
    Pension = ggplotly(g2)
  ))
}

simular_reserva <- function(reserva_inicial, vanus, tasas, minimo_ivm = 30638.35, edad_retiro = 65) {
  n <- min(length(tasas), nrow(vanus))
  
  resultados <- data.frame(
    Edad = edad_retiro:(edad_retiro + n - 1),
    Reserva_Ini = numeric(n),
    Pension_Mensual = numeric(n),
    TasaRend = tasas[1:n],
    Rendimientos = numeric(n),
    Reserva_Final = numeric(n)
  )
  
  # Año 1
  resultados$Reserva_Ini[1] <- reserva_inicial
  resultados$Pension_Mensual[1] <- max(minimo_ivm, reserva_inicial / vanus$VANU[1])
  pension_anual <- 12 * resultados$Pension_Mensual[1]
  tasa1 <- resultados$TasaRend[1]
  rend1 <- reserva_inicial * tasa1 - (pension_anual * tasa1 / 2)
  resultados$Rendimientos[1] <- rend1
  resultados$Reserva_Final[1] <- max(0, reserva_inicial + rend1 - pension_anual)
  
  # Años siguientes
  for (i in 2:n) {
    resultados$Reserva_Ini[i] <- resultados$Reserva_Final[i - 1]
    resultados$Pension_Mensual[i] <- max(minimo_ivm, resultados$Reserva_Ini[i] / vanus$VANU[i])
    pension_anual <- 12 * resultados$Pension_Mensual[i]
    tasa <- resultados$TasaRend[i]
    rend <- resultados$Reserva_Ini[i] * tasa - (pension_anual * tasa / 2)
    resultados$Rendimientos[i] <- rend
    resultados$Reserva_Final[i] <- max(0, resultados$Reserva_Ini[i] + rend - pension_anual)
  }
  
  return(resultados)
}

intereses <- function(x){
  set.seed(2025)
  tasas <- sapply(x:115, function(x)
    ifelse(runif(1) < 0.05,
           qnorm(runif(1), mean = -0.05, sd = 0.01),
           qnorm(runif(1), mean = 0.09, sd = 0.03)))
  return(tasas)
}

simular_reserva_temporal <- function(reserva_inicial, vanus_temporal, tasas, minimo_ivm = 30638.35) {
  n <- nrow(vanus_temporal)
  resultados <- data.frame(
    Edad = vanus_temporal$Edad,
    Reserva_Ini = numeric(n),
    Pension_Mensual = numeric(n),
    TasaRend = tasas[1:n],
    Rendimientos = numeric(n),
    Reserva_Final = numeric(n)
  )
  
  resultados$Reserva_Ini[1] <- reserva_inicial
  resultados$Pension_Mensual[1] <- max(minimo_ivm, reserva_inicial / vanus_temporal$VANU_a_ex65[1])
  pension_anual <- 12 * resultados$Pension_Mensual[1]
  tasa1 <- resultados$TasaRend[1]
  rend1 <- reserva_inicial * tasa1 - (pension_anual * tasa1 / 2)
  resultados$Rendimientos[1] <- rend1
  resultados$Reserva_Final[1] <- max(0, reserva_inicial + rend1 - pension_anual)
  
  for (i in 2:n) {
    resultados$Reserva_Ini[i] <- resultados$Reserva_Final[i - 1]
    resultados$Pension_Mensual[i] <- max(minimo_ivm, resultados$Reserva_Ini[i] / vanus_temporal$VANU_a_ex65[i])
    pension_anual <- 12 * resultados$Pension_Mensual[i]
    tasa <- resultados$TasaRend[i]
    rend <- resultados$Reserva_Ini[i] * tasa - (pension_anual * tasa / 2)
    resultados$Rendimientos[i] <- rend
    resultados$Reserva_Final[i] <- max(0, resultados$Reserva_Ini[i] + rend - pension_anual)
  }
  
  return(resultados)
}

