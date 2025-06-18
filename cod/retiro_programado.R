vanuTemporal <- function(edad_inicio, sexo, tabla, r = 0.036, m = 12, anno_objetivo = 2025) {
  # Obtener esperanza de vida a los 65 años
  fila_ex65 <- tabla[tabla$edad == 65 & tabla$sex == sexo, ]
  annos_disp <- unique(fila_ex65$year)
  anno_base <- annos_disp[which.min(abs(annos_disp - anno_objetivo))]
  ex65 <- fila_ex65$ex[fila_ex65$year == anno_base][1]
  resultados <- data.frame()
  for (edad in edad_inicio:90) {
    n <- ceiling(ex65) - (edad - 65)
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
