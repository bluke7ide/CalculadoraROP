intereses <- function(x) {
  tasas <- sapply(x:115, function(x)
    ifelse(runif(1) < 0.05,
           qnorm(runif(1), mean = -0.05, sd = 0.01),
           qnorm(runif(1), mean = 0.09, sd = 0.03)))
  return(tasas)
}

set.seed(2024)
n_simulaciones <- 10000
edad_inicial <- 65
anios <- edad_inicial:115
n_anios <- length(anios)
resultados <- matrix(NA, nrow = n_simulaciones, ncol = n_anios)
for (i in 1:n_simulaciones) {
  resultados[i, ] <- intereses(edad_inicial)
}

library(dplyr)
library(tidyr)
library(ggplot2)

df <- as.data.frame(resultados)
colnames(df) <- paste0("Edad_", anios)
df$Simulacion <- 1:n_simulaciones
df_long <- df %>%
  pivot_longer(-Simulacion, names_to = "Edad", values_to = "Tasa") %>%
  mutate(Edad = as.numeric(gsub("Edad_", "", Edad)))
resumen <- df_long %>%
  group_by(Edad) %>%
  summarise(
    p5 = quantile(Tasa, 0.05),
    p25 = quantile(Tasa, 0.25),
    p50 = quantile(Tasa, 0.5),
    p75 = quantile(Tasa, 0.75),
    p95 = quantile(Tasa, 0.95)
  )
ggplot(resumen, aes(x = Edad)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "steelblue", alpha = 0.4) +
  geom_line(aes(y = p50), color = "blue", size = 1) +
  geom_line(aes(y = p5), linetype = "dashed", color = "gray50") +
  geom_line(aes(y = p95), linetype = "dashed", color = "gray50") +
  labs(
    title = "Simulación Estocástica de Tasas de Interés",
    x = "Edad",
    y = "Tasa de interés"
  ) +
  theme_minimal()

