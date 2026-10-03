############################################################
# processamento.R
# RESPONSABILIDADE: OLS, média móvel, diagnóstico e previsão
#                   (sem gráficos e sem construir matrizes de modelo).
# DEPENDÊNCIAS: nenhuma. As matrizes X_tendencia e X_sazonal chegam
#               prontas por argumento (feitas no dados.R pelo main.R).
#
# INTERFACE:
#
# calcular_media_movel(serie, q)
#   Entrada : serie (vetor), q (inteiro, 2q+1 <= length(serie))
#   Retorna : vetor de tamanho n - 2q
#
# calcular_media_amostral(serie)
#   Retorna : um número
#
# multiplicar_matriz_vetor(X, beta)
#   Pré-condição: length(beta) = ncol(X)
#   Retorna : vetor de tamanho nrow(X)
#
# calcular_XtX(X)   Retorna : matriz p x p
# calcular_Xty(X, y)  Pré-condição: length(y) = nrow(X); Retorna: vetor p
#
# ajustar_ols(X, y)
#   Pré-condição: X'X invertível (colunas independentes)
#   Retorna : vetor beta_hat de tamanho p
#
# ajustar_modelo(y, X_tendencia, X_sazonal)
#   Entrada : y (vetor n), duas matrizes com n linhas
#   Retorna : lista com beta_hat, y_hat, tendencia, sazonalidade,
#             residuos, X_full
#
# diagnostico(y, residuos)
#   Retorna : lista com media, variancia, rmse, r2
#
# prever(modelo, X_tendencia_fut, X_sazonal_fut)
#   Entrada : modelo (lista de ajustar_modelo) e matrizes dos
#             instantes futuros (mesmas colunas do ajuste)
#   Retorna : vetor de previsões
############################################################

calcular_media_movel <- function(serie,q){
  
  n <- length(serie)
  
  numero_medias <- n - 2*q
  
  medias <- numeric(numero_medias)
  
  if(numero_medias > 0){
    
    for(t in (q+1):(n-q)){
      
      soma <- 0
      
      for(j in (t-q):(t+q)){
        
        soma <- soma + serie[j]
      }
      
      medias[t-q] <-soma / (2*q + 1)
    }
  }
  return(medias)
}

calcular_media_amostral <- function(serie){
  
  n <- length(serie)
  
  soma <- 0
  
  for(i in 1:n){
    soma <- soma + serie[i]
  }
  
  media <- soma / n
  
  return(media)
}

multiplicar_matriz_vetor <- function(X,beta){
  
  n <- nrow(X)
  
  numero_colunas <- ncol(X)
  
  resultado <- numeric(n)
  
  for(i in 1:n){
    
    valor <- 0
    
    for(j in 1:numero_colunas){
      
      valor <-valor + X[i,j] * beta[j]
    }
    
    resultado[i] <- valor
  }
  
  return(resultado)
}

calcular_XtX <- function(X){
  
  numero_linhas <- nrow(X)
  
  numero_colunas <- ncol(X)
  
  resultado <- matrix(0,nrow = numero_colunas,ncol = numero_colunas)
  
  for(i in 1:numero_colunas){
    
    for(j in 1:numero_colunas){
      
      soma <- 0
      
      for(k in 1:numero_linhas){
        
        soma <-soma + X[k,i] * X[k,j]
        
      }
      
      resultado[i,j] <- soma
    }
  }
  
  return(resultado)
}

calcular_Xty <- function(X,y){
  
  numero_linhas <- nrow(X)
  
  numero_colunas <- ncol(X)
  
  resultado <- numeric(numero_colunas)
  
  for(i in 1:numero_colunas){
    
    soma <- 0
    
    for(k in 1:numero_linhas){
      
      soma <-soma + X[k,i] * y[k]
    }
    
    resultado[i] <- soma
  }
  
  return(resultado)
}

# Resolve as equações normais (X'X) beta = X'y com solve()
ajustar_ols <- function(X,y){
  
  XtX <- calcular_XtX(X)
  
  Xty <- calcular_Xty(X,y)
  
  beta_hat <- solve(XtX,Xty)
  
  beta_hat <- as.numeric(beta_hat)
  
  return(beta_hat)
}

# Versão do script original que recebe as matrizes já construídas
# (assim este arquivo não depende do dados.R).
ajustar_modelo <- function(y, X_tendencia, X_sazonal){
  
  y <- as.numeric(y)
  
  X_full <-cbind(X_tendencia, X_sazonal)
  
  beta_hat <-ajustar_ols(X_full, y)
  
  y_hat <-multiplicar_matriz_vetor(X_full, beta_hat)
  
  # COEFICIENTES DA TENDÊNCIA
  numero_colunas_tendencia <-ncol(X_tendencia)
  
  beta_tendencia_hat <-numeric(numero_colunas_tendencia)
  
  for(i in 1:numero_colunas_tendencia){
    
    beta_tendencia_hat[i] <- beta_hat[i]
    
  }
  
  # COEFICIENTES DA SAZONALIDADE
  numero_colunas_sazonal <-
    ncol(X_sazonal)
  
  beta_sazonal_hat <-
    numeric(numero_colunas_sazonal)
  
  for(i in 1:numero_colunas_sazonal){
    
    beta_sazonal_hat[i] <-beta_hat[numero_colunas_tendencia + i]
  }
  
  tendencia_hat <-multiplicar_matriz_vetor(X_tendencia, beta_tendencia_hat)
  
  sazonalidade_hat <-multiplicar_matriz_vetor(X_sazonal, beta_sazonal_hat)
  
  # RESÍDUOS
  n <- length(y)
  
  residuos <- numeric(n)
  
  for(i in 1:n){
    
    residuos[i] <- y[i] - y_hat[i]
  }
  
  return(
    list(
      beta_hat = beta_hat,
      y_hat = y_hat,
      tendencia = tendencia_hat,
      sazonalidade = sazonalidade_hat,
      residuos = residuos,
      X_full = X_full
    )
  )
}

diagnostico <- function(y,residuos){
  
  n <- length(y)
  
  # SOMA DOS QUADRADOS DOS RESÍDUOS
  ss_res <- 0
  
  for(i in 1:n){
    ss_res <- ss_res + residuos[i]^2
  }
  
  media_y <- calcular_media_amostral(y)
  
  # SOMA TOTAL DOS QUADRADOS
  ss_tot <- 0
  
  for(i in 1:n){
    
    d <- y[i] - media_y
    
    ss_tot <-ss_tot + d^2
  }
  
  media_residuos <-calcular_media_amostral(residuos)
  
  # VARIÂNCIA DOS RESÍDUOS
  soma_quadrados <- 0
  
  for(i in 1:n){
    
    d <-residuos[i] - media_residuos
    
    soma_quadrados <- soma_quadrados + d^2
  }
  
  variancia_residuos <-soma_quadrados / (n - 1)
  
  rmse <-sqrt(ss_res / n)
  
  r2 <-1 - ss_res / ss_tot
  
  return(list(media = media_residuos, variancia = variancia_residuos, rmse = rmse, r2 = r2))
}

# Versão do script original que recebe as matrizes futuras prontas.
prever <- function(modelo, X_tendencia_fut, X_sazonal_fut){
  
  X_fut <-cbind(X_tendencia_fut, X_sazonal_fut)
  
  previsoes <- multiplicar_matriz_vetor(X_fut, modelo$beta_hat)
  
  return(previsoes)
}
