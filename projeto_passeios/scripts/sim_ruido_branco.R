############################################################
# RUÍDO BRANCO 
############################################################

simular_ruido_branco <- function(n, sigma2 = 1){
  # eps_t ~ N(0, sigma2)
  return(rnorm(n, mean = 0, sd = sqrt(sigma2)))
}

# MÚLTIPLAS TRAJETÓRIAS ----

simular_multiplas_trajetorias_ruido <- function(n, m, sigma2 = 1){
  
  trajetorias <- matrix(0, nrow = n, ncol = m)
  
  for(i in 1:m){
    trajetorias[, i] <- simular_ruido_branco(n, sigma2)
  }
  
  return(trajetorias)
}