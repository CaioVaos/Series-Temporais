############################################################
# processamento.R
# RESPONSABILIDADE: cálculos estatísticos e média móvel
#                   (sem gráficos).
# DEPENDÊNCIAS: nenhuma.
#
# INTERFACE:
#
# calcular_media_movel(serie, q)
#   Entrada : serie (vetor numérico), q (inteiro >= 0, 2q+1 <= length(serie))
#   Retorna : vetor de tamanho n - 2q (o 1º valor é o centro t = q+1)
#
# calcular_media_amostral(serie)
#   Entrada : serie (vetor numérico, length >= 1)
#   Retorna : um número
#
# calcular_autocovariancia_empirica(serie, h)
#   Entrada : serie (vetor numérico), h (inteiro >= 0)
#   Retorna : um número (gamma_hat(h))
#
# calcular_autocovariancias(serie, max_lag)
#   Retorna : vetor de tamanho max_lag + 1 (posição h+1 = lag h)
#
# calcular_fac_empirica(serie, max_lag)
#   Pré-condição: série não constante (gamma0 != 0)
#   Retorna : vetor de tamanho max_lag + 1 (posição 1 = 1)
############################################################

calcular_media_movel <- function(serie,q){
  
  n <- length(serie)
  
  numero_medias <- n - 2*q
  
  medias <- numeric(numero_medias)
  
  if(numero_medias > 0){
    
    for(t in (q+1):(n-q)){
      
      soma <- 0
      
      for(j in (t-q):(t+q)){
        soma <-soma + serie[j]
      }
      medias[t-q] <-soma / (2*q + 1)
    }
  }
  
  return(medias)
}

calcular_media_amostral <- function(serie){
  
  n <- length(serie)
  
  soma <- 0
  
  for(t in 1:n){
    soma <-soma + serie[t]
  }
  
  media <-soma / n
  
  return(media)
}

calcular_autocovariancia_empirica <- function(serie,
                                              h){
  n <- length(serie)
  
  media <-calcular_media_amostral(serie)
  
  soma <- 0
  
  if(h == 0){
    
    for(t in 1:n){
      d <-serie[t] - media
      soma <-soma + d * d
    }
    
  } else {
    
    if(h < n){
      
      for(t in 1:(n-h)){
        d1 <-serie[t] - media
        d2 <-serie[t+h] - media
        soma <-soma + d1 * d2
      }
    }
  }
  
  autocovariancia <-soma / n
  
  return(autocovariancia)
}

calcular_autocovariancias <- function(serie,max_lag){
  
  autocovariancias <-numeric(max_lag + 1)
  
  for(h in 0:max_lag){
    autocovariancias[h+1] <-calcular_autocovariancia_empirica(serie,h)
  }
  
  return(autocovariancias)
}

calcular_fac_empirica <- function(serie, max_lag){
  
  autocovariancias <- calcular_autocovariancias(serie, max_lag)
  
  gamma0 <-autocovariancias[1]
  
  fac <-numeric(max_lag + 1)
  
  if(gamma0 != 0){
    
    for(h in 0:max_lag){
      fac[h+1] <-autocovariancias[h+1] /gamma0
    }
  }
  return(fac)
}
