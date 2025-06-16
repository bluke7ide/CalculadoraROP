renta_permanente <- function(rendimientos, x, monto, año_jub){ # no se ocupa el sexo no hay mortalidad.
  renta_per <- sum(rendimientos)/36
  int <- rendimientos/monto
  mean_int <- mean(int)
  std_int <- sd(int)
}