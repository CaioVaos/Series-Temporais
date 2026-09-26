############################################################
# PASSEIO ALEATÓRIO
# X_t = X_{t-1} + eps_t
############################################################

simular_passeio_aleatorio <- function(n, sigma2 = 1){
  
  eps <- simular_ruido_branco(n, sigma2)
  
  X <- numeric(n)
  
  # condição inicial
  X[1] <- eps[1]
  
  # construção recursiva
  if(n >= 2){
    for(t in 2:n){
      X[t] <- X[t-1] + eps[t]
    }
  }
  
  return(X)
}

# MÚLTIPLOS PASSEIOS ALEATORIOS ----

simular_multiplos_passeio_aleatorio <- function(n, m, sigma2 = 1){
  
  trajetorias <- matrix(0, nrow = n, ncol = m)
  
  for(i in 1:m){
    trajetorias[, i] <- simular_passeio_aleatorio(n, sigma2)
  }
  
  return(trajetorias)
}