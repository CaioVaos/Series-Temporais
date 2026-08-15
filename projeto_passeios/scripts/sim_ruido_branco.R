############################################################
# RUÍDO BRANCO 
############################################################

simular_ruido_branco <- function(n){
  # eps_t ~ N(0,1)
  return(rnorm(n, mean = 0, sd = sqrt(sigma2)))
}

# MÚLTIPLAS TRAJETÓRIAS ----

simular_multiplas_trajetorias_ruido <- function(n, m){
  
  trajetorias <- matrix(0, nrow = n, ncol = m)
  
  for(i in 1:m){
    trajetorias[, i] <- simular_ruido_branco(n)
  }
  
  return(trajetorias)
}
