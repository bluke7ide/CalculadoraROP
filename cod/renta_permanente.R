renta_permanente <- function(rendimientos, x, monto, año_jub, vec_rend){ # no se ocupa el sexo no hay mortalidad.
  renta_per <- sum(rendimientos)/36
  minpen <- 30638.35
  res <- data.frame(res_inicial = 0, edad = x+0:(115-x), pension = 0, tasa = vec_rend, rend = 0, res_final = 0)
  res$res_inicial[1] <- monto
  res$pension[1:3] <- max(renta_per, minpen)
  res$rend[1] <- (monto-6*res$pension[1])**res$tasa[1]
  res$res_final[1] <- monto + res$rend[1] - 12*res$pension[1]
  for(i in 2:(115-x+1)){
    if((i-1)%%3 == 0){
      
    }
  }
  return(res)
}

renta_permanente(rendimientos, 65, 100, 0, 1.07)
