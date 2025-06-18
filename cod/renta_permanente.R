renta_permanente <- function(rendimientos, x, monto, año_jub, vec_rend){ # no se ocupa el sexo no hay mortalidad.
  res <- data.frame(edad = x+0:(115-x),res_inicial = 0, pension = 0, 
                    tasa = vec_rend, rend = 0, res_final = 0)
  res$res_inicial[1] <- monto
  res$pension[1:3] <- max(sum(rendimientos)/36, 30638.35)
  res$rend[1] <- (monto-6*res$pension[1])*res$tasa[1]
  res$res_final[1] <- monto + res$rend[1] - 12*res$pension[1]
  for(i in 2:(115-x+1)){
    if((i-1)%%3 == 0){
      res$pension[1:3+(i-1)] <- max(sum(res$rend[1:3+(i-4)])/36, 30638.35)
    }
    res$res_inicial[i] <- res$res_final[i-1]
    res$rend[i] <- (res$res_inicial[i]-6*res$pension[i])*res$tasa[i]
    res$res_final[i] <- res$res_inicial[i] + res$rend[i] - 12*res$pension[i]
  }
  return(res)
}

graficos <- 