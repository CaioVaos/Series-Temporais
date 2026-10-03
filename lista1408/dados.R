############################################################
# dados.R
# RESPONSABILIDADE: simular os processos estocásticos.
# DEPENDÊNCIAS: nenhuma (os parâmetros chegam como argumentos).
#
# INTERFACE:
#
# simular_ruido_branco(n, sigma2)
#   Entrada : n (inteiro >= 1), sigma2 (numérico > 0)
#   Retorna : vetor numérico de tamanho n
#
# simular_passeio_aleatorio(n, sigma2)
#   Entrada : n (inteiro >= 1), sigma2 (numérico > 0)
#   Retorna : vetor numérico de tamanho n
#
# simular_ar1(n, phi, sigma2)
#   Entrada : n (inteiro >= 1), phi (numérico, |phi| < 1),
#             sigma2 (numérico > 0)
#   Retorna : vetor numérico de tamanho n
#
# Pré-condição: set.seed() já ter sido chamado (feito no config.R).
############################################################

simular_ruido_branco <- function(n, sigma2){
  
  eps <- numeric(n)
  
  for(t in 1:n){
    eps[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
  }
  
  return(eps)
}

# X_t = X_{t-1} + eps_t
simular_passeio_aleatorio <- function(n, sigma2){
  
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

# X_t = phi * X_{t-1} + eps_t
simular_ar1 <- function(n, phi, sigma2){
  
  eps <- simular_ruido_branco(n, sigma2)
  
  X <- numeric(n)
  
  # condição inicial
  X[1] <- eps[1]
  
  # construção recursiva
  if(n >= 2){
    
    for(t in 2:n){
      
      X[t] <- phi * X[t-1] + eps[t]
    }
  }
  
  return(X)
}
