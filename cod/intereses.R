intereses <- function(x){
  set.seed(2025)
  tasas <- sapply(x:115, function(x)
  ifelse(runif(1) < 0.05,
         qnorm(runif(1), mean = -0.05, sd = 0.01),
         qnorm(runif(1), mean = 0.09, sd = 0.03)))
  return(tasas)
}