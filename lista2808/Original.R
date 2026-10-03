############################################################
# AULA PRÁTICA – DECOMPOSIÇÃO CLÁSSICA DE SÉRIES TEMPORAIS
# TENDÊNCIA E SAZONALIDADE
############################################################

rm(list = ls())

set.seed(42)

############################################################
# 1. PARÂMETROS GERAIS
############################################################

n_anos <- 10

s <- 12

n <- n_anos * s

sigma <- 5

############################################################
# 2. FUNÇÕES DE CONSTRUÇÃO DAS MATRIZES
############################################################

############################################################
# MATRIZ DE TENDÊNCIA POLINOMIAL
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

############################################################
# MATRIZ DE FOURIER
############################################################

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

############################################################
# MATRIZ DE DUMMIES SAZONAIS
############################################################

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

############################################################
# 3. RUÍDO BRANCO
############################################################

simular_ruido_branco <- function(n, sigma){
  
  eps <- numeric(n)
  
  for(i in 1:n){
    
    eps[i] <- rnorm(1,mean = 0,sd = sigma
    )
  }
  
  return(eps)
}

############################################################
# 4. SIMULAÇÃO DA SÉRIE TEMPORAL
############################################################

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

############################################################
# 5. MÉDIA MÓVEL
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
        
        soma <- soma + serie[j]
      }
      
      medias[t-q] <-soma / (2*q + 1)
    }
  }
  return(medias)
}

############################################################
# 6. MÉDIA AMOSTRAL
############################################################

calcular_media_amostral <- function(serie){
  
  n <- length(serie)
  
  soma <- 0
  
  for(i in 1:n){
    soma <- soma + serie[i]
  }
  
  media <- soma / n
  
  return(media)
}

############################################################
# 7. ESTIMAÇÃO OLS
############################################################

############################################################
# PRODUTO MATRIZ-VETOR
############################################################

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

############################################################
# MATRIZ X TRANSPOSTA VEZES X
############################################################

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

############################################################
# MATRIZ X TRANSPOSTA VEZES y
############################################################

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

############################################################
# ESTIMAÇÃO DOS COEFICIENTES POR OLS
############################################################

ajustar_ols <- function(X,y){
  
  XtX <- calcular_XtX(X)
  
  Xty <- calcular_Xty(X,y)
  
  beta_hat <- solve(XtX,Xty)
  
  beta_hat <- as.numeric(beta_hat)
  
  return(beta_hat)
}

############################################################
# 8. AJUSTE DO MODELO COMPLETO
############################################################

ajustar_modelo <- function(t, y, grau_tendencia = 2, periodo = 12, K = 2, metodo_sazonal = "fourier"){
  
  t <- as.numeric(t)
  
  y <- as.numeric(y)
  
  ##########################################################
  # MATRIZ DE TENDÊNCIA
  ##########################################################
  
  X_tendencia <- criar_matriz_tendencia(t, grau_tendencia)
  
  ##########################################################
  # MATRIZ DE SAZONALIDADE
  ##########################################################
  
  if(metodo_sazonal == "fourier"){
    
    X_sazonal <- criar_matriz_fourier(t, periodo, K)
    
  } else if(metodo_sazonal == "dummies"){
    
    X_sazonal <-criar_matriz_dummies(t, periodo)
    
  } else {
    
    stop("Método sazonal inválido")
  }
  
  ##########################################################
  # MATRIZ COMPLETA
  ##########################################################
  
  X_full <-cbind(X_tendencia, X_sazonal)
  
  ##########################################################
  # ESTIMAÇÃO DOS COEFICIENTES
  ##########################################################
  
  beta_hat <-ajustar_ols(X_full, y)
  
  ##########################################################
  # VALORES AJUSTADOS
  ##########################################################
  
  y_hat <-multiplicar_matriz_vetor(X_full, beta_hat)
  
  ##########################################################
  # NÚMERO DE COEFICIENTES DA TENDÊNCIA
  ##########################################################
  
  numero_colunas_tendencia <-ncol(X_tendencia)
  
  ##########################################################
  # COEFICIENTES DA TENDÊNCIA
  ##########################################################
  
  beta_tendencia_hat <-numeric(numero_colunas_tendencia)
  
  for(i in 1:numero_colunas_tendencia){
    
    beta_tendencia_hat[i] <- beta_hat[i]
    
  }
  
  ##########################################################
  # COEFICIENTES DA SAZONALIDADE
  ##########################################################
  
  numero_colunas_sazonal <-
    ncol(X_sazonal)
  
  beta_sazonal_hat <-
    numeric(numero_colunas_sazonal)
  
  for(i in 1:numero_colunas_sazonal){
    
    beta_sazonal_hat[i] <-beta_hat[numero_colunas_tendencia + i]
  }
  
  ##########################################################
  # TENDÊNCIA ESTIMADA
  ##########################################################
  
  tendencia_hat <-multiplicar_matriz_vetor(X_tendencia, beta_tendencia_hat)
  
  ##########################################################
  # SAZONALIDADE ESTIMADA
  ##########################################################
  
  sazonalidade_hat <-multiplicar_matriz_vetor(X_sazonal, beta_sazonal_hat)
  
  ##########################################################
  # RESÍDUOS
  ##########################################################
  
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
      X_full = X_full,
      grau_tendencia = grau_tendencia,
      periodo = periodo,
      K = K,
      metodo_sazonal = metodo_sazonal
    )
  )
}

############################################################
# 9. DIAGNÓSTICO
############################################################

diagnostico <- function(y,residuos){
  
  n <- length(y)
  
  ##########################################################
  # SOMA DOS QUADRADOS DOS RESÍDUOS
  ##########################################################
  
  ss_res <- 0
  
  for(i in 1:n){
    ss_res <- ss_res + residuos[i]^2
  }
  
  ##########################################################
  # MÉDIA DE y
  ##########################################################
  
  media_y <- calcular_media_amostral(y)
  
  ##########################################################
  # SOMA TOTAL DOS QUADRADOS
  ##########################################################
  
  ss_tot <- 0
  
  for(i in 1:n){
    
    d <- y[i] - media_y
    
    ss_tot <-ss_tot + d^2
  }
  
  ##########################################################
  # MÉDIA DOS RESÍDUOS
  ##########################################################
  
  media_residuos <-calcular_media_amostral(residuos)
  
  ##########################################################
  # VARIÂNCIA DOS RESÍDUOS
  ##########################################################
  
  soma_quadrados <- 0
  
  for(i in 1:n){
    
    d <-residuos[i] - media_residuos
    
    soma_quadrados <- soma_quadrados + d^2
  }
  
  variancia_residuos <-soma_quadrados / (n - 1)
  
  ##########################################################
  # RMSE
  ##########################################################
  
  rmse <-sqrt(ss_res / n)
  
  ##########################################################
  # R²
  ##########################################################
  
  r2 <-1 - ss_res / ss_tot
  
  return(list(media = media_residuos, variancia = variancia_residuos, rmse = rmse, r2 = r2))
}

############################################################
# 10. PREVISÃO
############################################################

prever <- function(modelo,t_futuro){
  
  t_futuro <- as.numeric(t_futuro)
  
  grau_tendencia <-modelo$grau_tendencia
  
  periodo <-modelo$periodo
  
  K <-modelo$K
  
  metodo_sazonal <-modelo$metodo_sazonal
  
  ##########################################################
  # MATRIZ DE TENDÊNCIA FUTURA
  ##########################################################
  
  X_tendencia_fut <-criar_matriz_tendencia(t_futuro,grau_tendencia)
  
  ##########################################################
  # MATRIZ SAZONAL FUTURA
  ##########################################################
  
  if(metodo_sazonal == "fourier"){
    
    X_sazonal_fut <-criar_matriz_fourier(t_futuro, periodo, K)
    
  } else {
    
    X_sazonal_fut <-criar_matriz_dummies(t_futuro, periodo)
  }
  
  ##########################################################
  # MATRIZ FUTURA COMPLETA
  ##########################################################
  
  X_fut <-cbind(X_tendencia_fut, X_sazonal_fut)
  
  ##########################################################
  # PREVISÕES
  ##########################################################
  
  previsoes <- multiplicar_matriz_vetor(X_fut, modelo$beta_hat)
  
  return(previsoes)
}

############################################################
# 11. PLOTS
############################################################

############################################################
# SÉRIE OBSERVADA + AJUSTADA
############################################################

plot_serie_ajustada <- function(t,serie,ajustada){
  
  plot(
    t,
    serie,
    type = "l",
    col = "blue",
    lwd = 2,
    xlab = "Tempo",
    ylab = "X_t",
    main = "Série Observada vs Ajustada"
  )
  
  lines(
    t,
    ajustada,
    col = "red",
    lwd = 2,
    lty = 2
  )
  
  legend(
    "topleft",
    legend = c(
      "Série observada",
      "Série ajustada"
    ),
    col = c(
      "blue",
      "red"
    ),
    lwd = c(
      2,
      2
    ),
    lty = c(
      1,
      2
    ),
    bty = "n"
  )
  
  grid()
}

############################################################
# TENDÊNCIA
############################################################

plot_tendencia <- function(t, tendencia_estimada, tendencia_real = NULL){
  
  plot(
    t,
    tendencia_estimada,
    type = "l",
    col = "red",
    lwd = 2,
    xlab = "Tempo",
    ylab = "Tendência",
    main = "Tendência"
  )
  
  if(!is.null(tendencia_real)){
    
    lines(
      t,
      tendencia_real,
      col = "blue",
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c(
        "Tendência estimada",
        "Tendência real"
      ),
      col = c(
        "red",
        "blue"
      ),
      lwd = 2,
      bty = "n"
    )
  }
  
  grid()
}

############################################################
# SAZONALIDADE
############################################################

plot_sazonalidade <- function(t, sazonalidade_estimada, sazonalidade_real = NULL){
  
  plot(
    t,
    sazonalidade_estimada,
    type = "l",
    col = "green",
    lwd = 2,
    xlab = "Tempo",
    ylab = "Sazonalidade",
    main = "Sazonalidade"
  )
  
  if(!is.null(sazonalidade_real)){
    
    lines(
      t,
      sazonalidade_real,
      col = "blue",
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c(
        "Sazonalidade estimada",
        "Sazonalidade real"
      ),
      col = c(
        "green",
        "blue"
      ),
      lwd = 2,
      bty = "n"
    )
  }
  
  grid()
}

############################################################
# RESÍDUOS
############################################################

plot_residuos <- function(t, residuos){
  
  plot(
    t,
    residuos,
    type = "l",
    col = "black",
    lwd = 2,
    xlab = "Tempo",
    ylab = "Resíduo",
    main = "Resíduos"
  )
  
  abline(
    h = 0,
    col = "red",
    lty = 2
  )
  
  grid()
}

############################################################
# 12. EXEMPLO DE USO
############################################################

############################################################
# VETOR DE TEMPO
############################################################

t <- 1:n

############################################################
# COEFICIENTES DA TENDÊNCIA
############################################################

beta_tendencia <-c(2,0.05,0.001)

grau_tendencia <- 2

############################################################
# MATRIZ DE TENDÊNCIA
############################################################

X_tendencia <-criar_matriz_tendencia(t, grau_tendencia)

############################################################
# COEFICIENTES SAZONAIS
############################################################

alpha <-c(2,1)

beta_h <-c(-1,0.5)

K <- 2

############################################################
# MATRIZ DE FOURIER
############################################################

X_sazonal <-criar_matriz_fourier(t, s, K)

############################################################
# VETOR DE COEFICIENTES SAZONAIS
############################################################

beta_sazonal <- numeric(2 * K)

for(k in 1:K){
  
  coluna_cos <- 2*k - 1
  coluna_sin <- 2*k
  
  beta_sazonal[coluna_cos] <-alpha[k]
  
  beta_sazonal[coluna_sin] <-beta_h[k]
}

############################################################
# SIMULAÇÃO DA SÉRIE
############################################################

Xt <-simular_serie(beta_tendencia = beta_tendencia,
    X_tendencia = X_tendencia,
    X_sazonal = X_sazonal,
    beta_sazonal = beta_sazonal,
    sigma = sigma)

############################################################
# TENDÊNCIA REAL
############################################################

tendencia_real <-multiplicar_matriz_vetor(X_tendencia, beta_tendencia)

############################################################
# SAZONALIDADE REAL
############################################################

sazonalidade_real <-multiplicar_matriz_vetor(X_sazonal, beta_sazonal)

############################################################
# MÉDIA MÓVEL
############################################################

q <- 5

tendencia_media_movel <-calcular_media_movel(Xt, q)

############################################################
# 13. AJUSTE DO MODELO
############################################################

modelo <-ajustar_modelo(t = t, y = Xt, grau_tendencia = 2, periodo = 12, K = 2, metodo_sazonal = "fourier")

############################################################
# 14. DIAGNÓSTICO
############################################################

resultado_diagnostico <-diagnostico(y = Xt ,residuos = modelo$residuos)

cat("Média dos resíduos:", resultado_diagnostico$media, "\n")

cat("Variância dos resíduos:", resultado_diagnostico$variancia, "\n")

cat("RMSE:", resultado_diagnostico$rmse, "\n")

cat("R²:",resultado_diagnostico$r2,"\n")

############################################################
# 15. COEFICIENTES ESTIMADOS
############################################################

cat("Coeficientes estimados:\n")

print(modelo$beta_hat)

############################################################
# 16. GRÁFICOS
############################################################

par(mfrow = c(4,1),mar = c(4,4,2,2))

############################################################
# SÉRIE OBSERVADA + AJUSTADA
############################################################

plot_serie_ajustada(t = t, serie = Xt, ajustada = modelo$y_hat)

############################################################
# TENDÊNCIA
############################################################

plot_tendencia(t = t, tendencia_estimada = modelo$tendencia, tendencia_real = tendencia_real)

############################################################
# SAZONALIDADE
############################################################

plot_sazonalidade(t = t, sazonalidade_estimada = modelo$sazonalidade, sazonalidade_real = sazonalidade_real)

############################################################
# RESÍDUOS
############################################################

plot_residuos(t = t, residuos = modelo$residuos)

par(mfrow = c(1,1))

############################################################
# 17. COMPARAÇÃO ENTRE MÉDIA MÓVEL E TENDÊNCIA ESTIMADA
############################################################

plot(
  t,
  Xt,
  type = "l",
  col = "blue",
  lwd = 1.5,
  xlab = "Tempo",
  ylab = "X_t",
  main = "Série Temporal, Média Móvel e Tendência"
)

tempo_media <-
  (q+1):(n-q)

lines(
  tempo_media,
  tendencia_media_movel,
  col = "red",
  lwd = 2
)

lines(
  t,
  modelo$tendencia,
  col = "green",
  lwd = 2
)

legend(
  "topleft",
  legend = c(
    "Série Xt",
    paste0("Média móvel q=",q),
    "Tendência estimada por OLS"
  ),
  col = c(
    "blue",
    "red",
    "green"
  ),
  lwd = c(
    1.5,
    2,
    2
  ),
  bty = "n"
)

grid()

############################################################
# 18. PREVISÃO
############################################################

############################################################
# NOVOS INSTANTES DE TEMPO
############################################################

t_futuro <- (n+1):(n+12)

############################################################
# PREVISÕES
############################################################

previsoes <-prever(modelo, t_futuro)

cat("Previsões para os próximos 12 períodos:\n")

print(previsoes)

############################################################
# FIM
############################################################
