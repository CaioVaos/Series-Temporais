############################################################
# dados.R
# RESPONSABILIDADE: matrizes de modelo, ruído branco e série simulada.
# DEPENDÊNCIAS: nenhuma (tudo chega por argumentos).
#
# INTERFACE:
#
# criar_matriz_tendencia(t, grau)
#   Entrada : t (vetor numérico), grau (inteiro >= 0)
#   Retorna : matriz length(t) x (grau+1); coluna j+1 = t^j
#
# criar_matriz_fourier(t, periodo, K)
#   Entrada : t (vetor), periodo (inteiro >= 2), K (inteiro, K < periodo/2)
#   Retorna : matriz length(t) x 2K (cos1, sen1, cos2, sen2, ...)
#
# criar_matriz_dummies(t, periodo)
#   Entrada : t (vetor de inteiros), periodo (inteiro >= 2)
#   Retorna : matriz length(t) x (periodo-1); mês 1 é a referência
#
# simular_ruido_branco(n, sigma)
#   Entrada : n (inteiro >= 1), sigma (numérico > 0)
#   Retorna : vetor numérico de tamanho n
#
# simular_serie(beta_tendencia, X_tendencia, X_sazonal, beta_sazonal, sigma)
#   Pré-condição: length(beta_tendencia) = ncol(X_tendencia),
#                 length(beta_sazonal) = ncol(X_sazonal), mesmo nrow
#   Retorna : vetor de tamanho n (tendência + sazonalidade + ruído)
#
# Pré-condição geral: set.seed() já chamado (config.R).
############################################################

criar_matriz_tendencia <- function(t, grau){
  
  n <- length(t)
  
  numero_colunas <- grau + 1
  
  X <- matrix(0,nrow = n,ncol = numero_colunas)
  
  for(i in 1:n){
    
    for(j in 0:grau){
      X[i,j+1] <- t[i]^j
    }
  }
  return(X)
}

criar_matriz_fourier <- function(t, periodo, K){
  
  n <- length(t)
  
  numero_colunas <- 2 * K
  
  X <- matrix(0,nrow = n,ncol = numero_colunas)
  
  for(i in 1:n){
    
    for(k in 1:K){
      
      coluna_cos <- 2*k - 1
      coluna_sin <- 2*k
      
      X[i,coluna_cos] <-cos(2 * pi * k * t[i] / periodo)
      
      X[i,coluna_sin] <-sin(2 * pi * k * t[i] / periodo)
    }
  }
  return(X)
}

criar_matriz_dummies <- function(t, periodo){
  
  n <- length(t)
  
  numero_colunas <- periodo - 1
  
  X <- matrix(0,nrow = n,ncol = numero_colunas)
  
  for(i in 1:n){
    
    mes <- ((t[i] - 1) %% periodo) + 1
    
    if(mes != 1){
      
      coluna <- mes - 1
      
      X[i,coluna] <- 1
    }
  }
  
  return(X)
}

simular_ruido_branco <- function(n, sigma){
  
  eps <- numeric(n)
  
  for(i in 1:n){
    
    eps[i] <- rnorm(1,mean = 0,sd = sigma
    )
  }
  
  return(eps)
}

simular_serie <- function(beta_tendencia,
                          X_tendencia,
                          X_sazonal,
                          beta_sazonal,
                          sigma){
  
  n <- nrow(X_tendencia)
  
  numero_colunas_tendencia <-ncol(X_tendencia)
  
  numero_colunas_sazonal <-ncol(X_sazonal)
  
  eps <- simular_ruido_branco(n, sigma)
  
  Xt <- numeric(n)
  
  for(i in 1:n){
    
    valor_tendencia <- 0
    
    for(j in 1:numero_colunas_tendencia){
      
      valor_tendencia <-valor_tendencia +X_tendencia[i,j] *beta_tendencia[j]
    }
    
    valor_sazonal <- 0
    
    for(j in 1:numero_colunas_sazonal){
      
      valor_sazonal <-valor_sazonal +X_sazonal[i,j] *beta_sazonal[j]
    }
    
    Xt[i] <-valor_tendencia + valor_sazonal + eps[i]
  }
  return(Xt)
}
