############################################################
# AULA PRÁTICA – SÉRIE TEMPORAL
# TENDÊNCIA, MÉDIA MÓVEL E AUTOCORRELAÇÃO
############################################################

rm(list = ls())

set.seed(123)

############################################################
# 1. PARÂMETROS GERAIS
############################################################

n <- 90

sigma <- 15

q <- 3

max_lag <- 20

############################################################
# 2. SIMULAÇÃO
############################################################

############################################################
# SÉRIE TEMPORAL COM TENDÊNCIA POLINOMIAL
############################################################

############################################################
# RUÍDO BRANCO
############################################################

simular_ruido_branco <- function(n, sigma){
  
  eps <- numeric(n)
  
  for(t in 1:n){
    eps[t] <-rnorm(1, mean = 0, sd = sigma)
  }
  
  return(eps)
}

############################################################
# SÉRIE TEMPORAL COM TENDÊNCIA POLINOMIAL
############################################################

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


############################################################
# 3. MÉDIA MÓVEL
############################################################

############################################################
# MÉDIA MÓVEL CENTRADA DE RAIO q
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

############################################################
# 4. ESTATÍSTICAS
############################################################

############################################################
# MÉDIA AMOSTRAL
############################################################

calcular_media_amostral <- function(serie){
  
  n <- length(serie)
  
  soma <- 0
  
  for(t in 1:n){
    soma <-soma + serie[t]
  }
  
  media <-soma / n
  
  return(media)
}

############################################################
# AUTOCOVARIÂNCIA EMPÍRICA
############################################################

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

############################################################
# MÚLTIPLAS AUTOCOVARIÂNCIAS
############################################################

calcular_autocovariancias <- function(serie,max_lag){
  
  autocovariancias <-numeric(max_lag + 1)
  
  for(h in 0:max_lag){
    autocovariancias[h+1] <-calcular_autocovariancia_empirica(serie,h)
  }
  
  return(autocovariancias)
}

############################################################
# FUNÇÃO DE AUTOCORRELAÇÃO EMPÍRICA
############################################################

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

############################################################
# 5. PLOTS
############################################################

############################################################
# SÉRIE TEMPORAL
############################################################

plot_serie_temporal <- function(tempo, serie, titulo, cor = "blue"){
  
  n <- length(serie)
  
  plot(
    tempo,
    serie,
    type = "l",
    lwd = 2,
    col = cor,
    main = titulo,
    xlab = "Tempo (t)",
    ylab = "X_t"
  )
}

############################################################
# SÉRIE TEMPORAL + MÉDIA MÓVEL
############################################################

plot_serie_media_movel <- function(tempo, serie, media_movel, q){
  
  plot(
    tempo,
    serie,
    type = "l",
    col = "blue",
    lwd = 1.5,
    xlab = "Tempo (t)",
    ylab = "X_t",
    main = "Série Temporal com Média Móvel"
  )
  
  tempo_media <-
    (q+1):(length(serie)-q)
  
  lines(
    tempo_media,
    media_movel,
    col = "red",
    lwd = 2
  )
  
  legend(
    "topleft",
    legend = c(
      "Série Xt",
      paste0("Média móvel q=", q)
    ),
    col = c("blue", "red"),
    lwd = c(1.5, 2),
    bty = "n"
  )
}

############################################################
# FUNÇÃO DE AUTOCORRELAÇÃO EMPÍRICA
############################################################

plot_fac_empirica <- function(fac, titulo, cor = "skyblue"){
  
  max_lag <- length(fac) - 1
  
  barplot(
    fac,
    names.arg = 0:max_lag,
    col = cor,
    ylim = c(
      min(0, min(fac)),
      1
    ),
    main = titulo,
    xlab = "Lag (h)",
    ylab = expression(hat(rho)[h])
  )
  
  abline(
    h = 0,
    col = "gray",
    lty = 2
  )
}

############################################################
# 6. EXEMPLO DE USO
############################################################

############################################################
# VETOR DE TEMPO
############################################################

t <- 1:n

############################################################
# COEFICIENTES DO POLINÔMIO
############################################################

beta <- c(2, 0.01, 0.01)

############################################################
# MATRIZ DE REGRESSÃO
############################################################

X <- matrix(
  0,
  nrow = n,
  ncol = 3
)

for(i in 1:n){
  
  X[i,1] <- 1
  
  X[i,2] <- t[i]
  
  X[i,3] <- t[i]^2
}

############################################################
# SIMULAÇÃO DA SÉRIE
############################################################

Xt <-simular_serie_com_tendencia(beta, X, sigma)

############################################################
# MÉDIA AMOSTRAL
############################################################

media_Xt <-calcular_media_amostral(Xt)

cat("Média amostral de Xt:", media_Xt, "\n")

############################################################
# MÉDIA MÓVEL
############################################################

m_q <-calcular_media_movel(Xt,q)

############################################################
# AUTOCOVARIÂNCIA DE LAG 1
############################################################

gamma_1 <-calcular_autocovariancia_empirica(Xt,h = 1)

cat("Autocovariância empírica de defasagem 1:",gamma_1,"\n")

############################################################
# FUNÇÃO DE AUTOCORRELAÇÃO
############################################################

fac_Xt <-calcular_fac_empirica(Xt,max_lag)

cat("FAE (autocorrelação empírica) para lags 0 a",max_lag,":\n")

print(fac_Xt)

############################################################
# 7. GRÁFICOS
############################################################

par(mfrow = c(2,1),mar = c(4,4,2,2))

############################################################
# SÉRIE TEMPORAL + MÉDIA MÓVEL
############################################################

plot_serie_media_movel(tempo = t, serie = Xt, media_movel = m_q, q = q)

############################################################
# FUNÇÃO DE AUTOCORRELAÇÃO EMPÍRICA
############################################################

plot_fac_empirica(
  fac = fac_Xt,
  titulo = "Função de Autocorrelação Empírica (FAE)",
  cor = "skyblue"
)

par(mfrow = c(1,1))

############################################################
# 8. COMPARAÇÃO COM A FUNÇÃO BUILT-IN DO R
############################################################

acf(Xt, main = "ACF (R) – Série Temporal")

############################################################
# FIM
############################################################
