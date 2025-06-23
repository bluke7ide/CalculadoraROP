source("cod/intereses.R")

set.seed(2024)
n_simulaciones <- 1000
edad_inicial <- 20
anios <- edad_inicial:115
n_anios <- length(anios)
resultados <- matrix(NA, nrow = n_simulaciones, ncol = n_anios)
for (i in 1:n_simulaciones) {
  resultados[i, ] <- intereses(edad_inicial)
}

tiempo = 1:96
df_sim <- as.data.frame(t(resultados*100))
df_res <- data.frame(Tiempo = tiempo,
                     Media = sapply(tiempo, function(y) mean(as.double(df_sim[y,]))),
                     IC_inf = sapply(tiempo, function(y) quantile(as.double(df_sim[y,]), 0.05)),
                     IC_sup = sapply(tiempo, function(y) quantile(as.double(df_sim[y,]), 0.95)))
df_sim$Tiempo <- tiempo
df_long <- pivot_longer(df_sim, -Tiempo, names_to = "Simulacion", values_to = "Interes")

df_long <- df_long %>%
  mutate(Tipo = "Simulaciones")



df_res_lineas <- df_res %>% mutate(Tipo = "Media")

fig <- ggplot() +
  geom_line(data = df_long,
            aes(x = Tiempo, y = Interes, group = Simulacion, color = "Simulaciones"),
            alpha = 0.01, linewidth = 0.2) +
  geom_ribbon(data = df_res, 
              aes(x = Tiempo, ymin = IC_inf, ymax = IC_sup, fill = "IC"),
              alpha = 0.4) +
  geom_line(data = df_res_lineas, 
            aes(x = Tiempo, y = Media, color = "Media"),
            linewidth = 1) +
  scale_color_manual(name = "",
                     values = c(
                       "Media" = "red",
                       "Simulaciones" = "blue"
                     )) +
  scale_fill_manual(name = "", values = c("IC" = "darkgreen")) +
  labs(title = "Trayectorias simuladas de las tasas de interés aleatorias",
       x = "Tiempo", y = "Porcentaje de interés") +
  theme_minimal() 

ggsave("cod/rplot.png", fig)
write.csv(df_res$Media/100, "cod/media.csv")
