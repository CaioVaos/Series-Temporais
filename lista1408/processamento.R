############################################################
# processamento.R
# RESPONSABILIDADE: cálculos estatísticos (sem gráficos).
# DEPENDÊNCIAS: nenhuma.
#
# INTERFACE:
#
# calcular_media_empirica(serie)
#   Entrada : serie (vetor numérico, length >= 1)
#   Retorna : um número (média)
#
# calcular_autocovariancia_empirica(serie, h)
#   Entrada : serie (vetor numérico), h (inteiro, 0 <= h < length(serie))
#   Retorna : um número (gamma_hat(h))
#
# calcular_autocovariancias(serie, max_lag)
#   Entrada : serie (vetor numérico), max_lag (inteiro < length(serie))
#   Retorna : vetor de tamanho max_lag + 1 (posição h+1 = lag h)
#
# calcular_fac_empirica(serie, max_lag)
#   Entrada : serie (vetor numérico, não constante), max_lag
#   Retorna : vetor de tamanho max_lag + 1 (posição 1 = 1)
#
# calcular_fac_teorica_ar1(phi, max_lag)
#   Entrada : phi (numérico, |phi| < 1), max_lag (inteiro >= 0)
#   Retorna : vetor de tamanho max_lag + 1 com phi^h
############################################################

calcular_media_empirica <- function(serie){
  
  n <- length(serie)
  
  soma <- 0
  
  for(t in 1:n){
    
    soma <- soma + serie[t]
  }
  
  media <- soma / n
  
  return(media)
}

# gamma_hat(h) = (1/n) * soma_{t=1}^{n-h} (X_t - X_bar)(X_{t+h} - X_bar)
calcular_autocovariancia_empirica <- function(serie, h){
  
  n <- length(serie)
  
  media <- calcular_media_empirica(serie)
  
  soma <- 0
  
  if(h == 0){
    
    for(t in 1:n){
      
      d <- serie[t] - media
      
      soma <- soma + d * d
    }
    
  } else {
    
    for(t in 1:(n-h)){
      
      d1 <- serie[t] - media
      
      d2 <- serie[t+h] - media
      
      soma <- soma + d1 * d2
    }
  }
  
  autocovariancia <- soma / n
  
  return(autocovariancia)
}

calcular_autocovariancias <- function(serie, max_lag){
  
  autocovariancias <- numeric(max_lag + 1)
  
  for(h in 0:max_lag){
    
    autocovariancias[h+1] <-
      calcular_autocovariancia_empirica(serie, h)
  }
  
  return(autocovariancias)
}

# rho_hat(h) = gamma_hat(h) / gamma_hat(0)
calcular_fac_empirica <- function(serie, max_lag){
  
  autocovariancias <-
    calcular_autocovariancias(serie, max_lag)
  
  gamma0 <- autocovariancias[1]
  
  fac <- numeric(max_lag + 1)
  
  fac[1] <- 1
  
  if(max_lag >= 1){
    
    for(h in 1:max_lag){
      
      fac[h+1] <-
        autocovariancias[h+1] / gamma0
    }
  }
  
  return(fac)
}

# rho(h) = phi^h
calcular_fac_teorica_ar1 <- function(phi, max_lag){
  
  fac_teorica <- numeric(max_lag + 1)
  
  for(h in 0:max_lag){
    
    fac_teorica[h+1] <- phi^h
  }
  
  return(fac_teorica)
}
