############################################################
# dados.R
# RESPONSABILIDADE: simular ruído branco e a série com tendência.
# DEPENDÊNCIAS: nenhuma (parâmetros chegam como argumentos).
#
# INTERFACE:
#
# simular_ruido_branco(n, sigma)
#   Entrada : n (inteiro >= 1), sigma (numérico > 0)
#   Retorna : vetor numérico de tamanho n
#
# simular_serie_com_tendencia(beta, X, sigma)
#   Entrada : beta (vetor numérico com length = ncol(X)),
#             X (matriz numérica n x p), sigma (numérico > 0)
#   Retorna : vetor numérico de tamanho n (X %*% beta + ruído)
#
# Pré-condição: set.seed() já chamado (feito no config.R).
############################################################

simular_ruido_branco <- function(n, sigma){
  
  eps <- numeric(n)
  
  for(t in 1:n){
    eps[t] <-rnorm(1, mean = 0, sd = sigma)
  }
  
  return(eps)
}

simular_serie_com_tendencia <- function(beta, X, sigma){
  
  n <- nrow(X)
  
  eps <-simular_ruido_branco(n, sigma)
  
  Xt <- numeric(n)
  
  numero_colunas <- ncol(X)
  
  for(t in 1:n){
    
    valor <- 0
    
    for(j in 1:numero_colunas){
      valor <-valor + X[t,j] * beta[j]
    }
    
    Xt[t] <- valor + eps[t]
  }
  
  return(Xt)
}
