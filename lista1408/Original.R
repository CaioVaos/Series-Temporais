############################################################
# AULA PRÁTICA – PROCESSOS ESTOCÁSTICOS
############################################################

rm(list = ls())
set.seed(123)

############################################################
# 1. PARÂMETROS GERAIS
############################################################

n <- 200

max_lag <- 20

sigma2 <- 1

phi <- 0.7

############################################################
# 2. SIMULAÇÃO
############################################################

############################################################
# RUÍDO BRANCO
############################################################

simular_ruido_branco <- function(n){
  
  eps <- numeric(n)
  
  for(t in 1:n){
    eps[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
  }
  
  return(eps)
}

############################################################
# PASSEIO ALEATÓRIO
# X_t = X_{t-1} + eps_t
############################################################

simular_passeio_aleatorio <- function(n){
  
  eps <- simular_ruido_branco(n)
  
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

############################################################
# PROCESSO AR(1)
# X_t = phi * X_{t-1} + eps_t
############################################################

simular_ar1 <- function(n, phi){
  
  eps <- simular_ruido_branco(n)
  
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

############################################################
# 3. ESTATÍSTICAS
############################################################

############################################################
# MÉDIA EMPÍRICA
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

############################################################
# AUTOCOVARIÂNCIA EMPÍRICA
#
# gamma_hat(h) =
#
# (1/n) * soma_{t=1}^{n-h}
# (X_t - X_bar)(X_{t+h} - X_bar)
############################################################

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

############################################################
# MÚLTIPLAS AUTOCOVARIÂNCIAS
############################################################

calcular_autocovariancias <- function(serie, max_lag){
  
  autocovariancias <- numeric(max_lag + 1)
  
  for(h in 0:max_lag){
    
    autocovariancias[h+1] <-
      calcular_autocovariancia_empirica(serie, h)
  }
  
  return(autocovariancias)
}

############################################################
# FAC EMPÍRICA
#
# rho_hat(h) = gamma_hat(h) / gamma_hat(0)
############################################################

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

############################################################
# FAC TEÓRICA DO AR(1)
#
# rho(h) = phi^h
############################################################

calcular_fac_teorica_ar1 <- function(phi, max_lag){
  
  fac_teorica <- numeric(max_lag + 1)
  
  for(h in 0:max_lag){
    
    fac_teorica[h+1] <- phi^h
  }
  
  return(fac_teorica)
}

############################################################
# 4. PLOTS
############################################################

############################################################
# SÉRIE TEMPORAL
############################################################

plot_serie_temporal <- function(serie,
                                titulo,
                                ylab = "X_t",
                                cor = "gray"){
  
  n <- length(serie)
  
  plot(1:n,
       serie,
       type = "l",
       lwd = 2,
       col = cor,
       main = titulo,
       xlab = "Tempo (t)",
       ylab = ylab)
  
  abline(h = 0,
         col = "red",
         lty = 2)
}

############################################################
# FAC EMPÍRICA
############################################################

plot_fac_empirica <- function(fac,
                              titulo,
                              cor = "gray70"){
  
  max_lag <- length(fac) - 1
  
  barplot(fac,
          names.arg = 0:max_lag,
          col = cor,
          main = titulo,
          xlab = "Lag (h)",
          ylab = "rho_hat")
}

############################################################
# FAC EMPÍRICA + FAC TEÓRICA
############################################################

plot_fac_comparacao <- function(fac_empirica,
                                fac_teorica,
                                titulo){
  
  max_lag <- length(fac_empirica) - 1
  
  barplot(fac_empirica,
          names.arg = 0:max_lag,
          ylim = c(0, 1),
          col = "lightgreen",
          main = titulo,
          xlab = "Lag (h)",
          ylab = "rho(h)")
  
  lines(0:max_lag,
        fac_teorica,
        type = "b",
        pch = 16,
        col = "darkgreen",
        lwd = 2)
  
  legend("topright",
         legend = c("FAC Empírica",
                    "FAC Teórica"),
         col = c("lightgreen",
                 "darkgreen"),
         lwd = c(2, 2),
         pch = c(NA, 16),
         bty = "n")
}

############################################################
# 5. EXEMPLOS DE USO
############################################################

############################################################
# RUÍDO BRANCO
############################################################

eps <- simular_ruido_branco(n)

############################################################
# MÉDIA EMPÍRICA
############################################################

media_eps <-
  calcular_media_empirica(eps)

cat("Média empírica (ruído branco):",
    media_eps,
    "\n")

############################################################
# FAC EMPÍRICA
############################################################

fac_eps <-calcular_fac_empirica(eps, max_lag)

############################################################
# GRÁFICOS – RUÍDO BRANCO
############################################################

par(mfrow = c(2,1))

plot_serie_temporal(
  serie = eps,
  titulo = "Ruído Branco",
  ylab = "eps_t",
  cor = "gray30"
)

plot_fac_empirica(
  fac = fac_eps,
  titulo = "FAC Empírica – Ruído Branco",
  cor = "gray70"
)

par(mfrow = c(1,1))

############################################################
# PASSEIO ALEATÓRIO
############################################################

X <- simular_passeio_aleatorio(n)

############################################################
# MÉDIA EMPÍRICA
############################################################

media_X <-
  calcular_media_empirica(X)

cat("Média empírica (passeio aleatório):",
    media_X,
    "\n")

############################################################
# FAC EMPÍRICA
############################################################

fac_X <-
  calcular_fac_empirica(X, max_lag)

############################################################
# GRÁFICOS – PASSEIO ALEATÓRIO
############################################################

par(mfrow = c(2,1))

plot_serie_temporal(
  serie = X,
  titulo = "Passeio Aleatório",
  ylab = "X_t",
  cor = "blue"
)

plot_fac_empirica(
  fac = fac_X,
  titulo = "FAC Empírica – Passeio Aleatório",
  cor = "lightblue"
)

par(mfrow = c(1,1))

############################################################
# COMPARAÇÃO COM FUNÇÃO BUILT-IN DO R
############################################################

par(mfrow = c(2,1))

acf(eps,
    main = "ACF (R) – Ruído Branco")

acf(X,
    main = "ACF (R) – Passeio Aleatório")

par(mfrow = c(1,1))

############################################################
# PROCESSO AR(1)
############################################################

X_ar1 <-
  simular_ar1(n, phi)

############################################################
# MÉDIA EMPÍRICA
############################################################

media_ar1 <- calcular_media_empirica(X_ar1)

cat("Média empírica (AR(1)):",
    media_ar1,
    "\n")

############################################################
# FAC EMPÍRICA
############################################################

fac_ar1 <-
  calcular_fac_empirica(X_ar1, max_lag)

############################################################
# FAC TEÓRICA
############################################################

fac_ar1_teorica <-
  calcular_fac_teorica_ar1(phi, max_lag)

############################################################
# GRÁFICOS – AR(1)
############################################################

par(mfrow = c(2,1))

plot_serie_temporal(
  serie = X_ar1,
  titulo = paste("Processo AR(1) Estacionário (phi =",
                 phi,
                 ")"),
  ylab = "X_t",
  cor = "forestgreen"
)

plot_fac_comparacao(
  fac_empirica = fac_ar1,
  fac_teorica = fac_ar1_teorica,
  titulo = "FAC Empírica vs Teórica – AR(1)"
)

par(mfrow = c(1,1))

############################################################
# 6. RESUMO DOS RESULTADOS
############################################################

cat("\n")
cat("========================================\n")
cat("RESUMO DAS MÉDIAS EMPÍRICAS\n")
cat("========================================\n")

cat("Ruído branco:",
    media_eps,
    "\n")

cat("Passeio aleatório:",
    media_X,
    "\n")

cat("AR(1):",
    media_ar1,
    "\n")

############################################################
# FIM
############################################################
