# ESTATÍSTICAS

# MÉDIA AMOSTRAL

calcular_media_amostral <- function(serie){

  n <- length(serie)
  soma <- 0

  for(t in 1:n){
    soma <- soma + serie[t]
  }

  media <- soma / n

  return(media)
}

# VARIÂNCIA AMOSTRAL

calcular_variancia_amostral <- function(serie){

  n <- length(serie)
  media <- calcular_media_amostral(serie)
  soma <- 0

  for(t in 1:n){
    d <- serie[t] - media
    soma <- soma + d * d
  }

  variancia <- soma / (n-1)

  return(variancia)
}

# AUTOCOVARIÂNCIA E FAC EMPÍRICAS

calcular_autocovariancia_empirica <- function(serie, h){

  n <- length(serie)
  media <- calcular_media_amostral(serie)
  soma <- 0

  for(t in 1:(n-h)){
    d1 <- serie[t] - media
    d2 <- serie[t+h] - media
    soma <- soma + d1 * d2
  }

  autocovariancia <- soma / n

  return(autocovariancia)
}

calcular_fac_empirica <- function(serie, max_lag){

  gamma0 <- calcular_autocovariancia_empirica(serie, 0)
  fac <- numeric(max_lag+1)

  for(h in 0:max_lag){
    fac[h+1] <- calcular_autocovariancia_empirica(serie, h) / gamma0
  }

  return(fac)
}

# MÉDIA MÓVEL

calcular_media_movel <- function(serie, janela){

  n <- length(serie)
  medias <- rep(NA, n)

  for(t in janela:n){
    soma <- 0

    for(j in (t-janela+1):t){
      soma <- soma + serie[j]
    }

    medias[t] <- soma / janela
  }

  return(medias)
}

# MÉTRICAS

diagnostico <- function(y, residuos){

  n <- length(y)
  ss_res <- 0
  soma_absoluta <- 0

  for(i in 1:n){
    ss_res <- ss_res + residuos[i]^2
    soma_absoluta <- soma_absoluta + abs(residuos[i])
  }

  media_y <- calcular_media_amostral(y)
  ss_tot <- 0

  for(i in 1:n){
    d <- y[i] - media_y
    ss_tot <- ss_tot + d^2
  }

  rmse <- sqrt(ss_res / n)
  mae <- soma_absoluta / n
  r2 <- 1 - ss_res / ss_tot

  return(c(RMSE = rmse, MAE = mae, R2 = r2))
}

# DECOMPOSIÇÃO MENSAL

decompor_mensal <- function(serie){

  n <- length(serie)
  tendencia <- rep(NA, n)

  # Média móvel centrada de ordem 2 x 12
  for(t in 7:(n-6)){
    soma <- 0.5 * serie[t-6] + 0.5 * serie[t+6]

    for(j in (t-5):(t+5)){
      soma <- soma + serie[j]
    }

    tendencia[t] <- soma / 12
  }

  sem_tendencia <- serie - tendencia
  meses <- rep(1:12, length.out = n)
  medias_mensais <- numeric(12)

  for(mes in 1:12){
    valores <- sem_tendencia[meses == mes & !is.na(sem_tendencia)]
    medias_mensais[mes] <- calcular_media_amostral(valores)
  }

  medias_mensais <- medias_mensais - calcular_media_amostral(medias_mensais)
  sazonalidade <- rep(medias_mensais, length.out = n)
  ruido <- serie - tendencia - sazonalidade

  return(data.frame(Tendencia = tendencia,
                    Sazonalidade = sazonalidade,
                    Ruido = ruido))
}
