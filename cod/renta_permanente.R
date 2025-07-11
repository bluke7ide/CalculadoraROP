renta_permanente <- function(rendimientos, x, monto, año_jub, vec_rend){ # no se ocupa el sexo no hay mortalidad.
  res <- data.frame(Edad = x+0:(115-x),res_inicial = 0, Pension_Mensual = 0, 
                    tasa = vec_rend, Rendimientos = 0, Reserva_Final = 0)
  res$res_inicial[1] <- monto
  res$Pension_Mensual[1:3] <- max(sum(rendimientos)/36, 31938.44)
  res$Rendimientos[1] <- (monto-6*res$Pension_Mensual[1])*res$tasa[1]
  if(res$res_inicial[1]-12*res$Pension_Mensual[1] <0){
    res$Pension_Mensual[1] <- res$res_inicial[1]/12
  }
  res$Reserva_Final[1] <- monto + res$Rendimientos[1] - 12*res$Pension_Mensual[1]
  for(i in 2:(115-x+1)){
    if((i-1)%%3 == 0){
      if(115-x+1-i <3){
       res$Pension_Mensual[i:(115-x+1)] <- max(sum(res$Rendimientos[1:3+(i-4)])/36, 31938.44)
      } else {
        res$Pension_Mensual[1:3+(i-1)] <- max(sum(res$Rendimientos[1:3+(i-4)])/36, 31938.44)
      }
    }

    res$res_inicial[i] <- res$Reserva_Final[i-1]
    res$Rendimientos[i] <- (res$res_inicial[i]-6*res$Pension_Mensual[i])*res$tasa[i]
    res$Reserva_Final[i] <- res$res_inicial[i] + res$Rendimientos[i] - 12*res$Pension_Mensual[i]
    if(res$Reserva_Final[i] <0){
      res$Pension_Mensual[i:(115-x+1)] <- 0
      res$Pension_Mensual[i] <- res$res_inicial[i]/12
      res$Reserva_Final[i] <- 0
      res$Rendimientos[i] <- 0
      break
    }
  }
  return(res)
}