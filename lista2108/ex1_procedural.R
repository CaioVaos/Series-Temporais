############################################################
# EXERCÍCIO 1 – IMPLEMENTAÇÃO PROCEDURAL
# Série temporal com tendência polinomial, média móvel e FAC
############################################################
#
# DOCUMENTAÇÃO DO SISTEMA
# ----------------------------------------------------------
# (a) ENTRADAS E PARÂMETROS GLOBAIS (definidos na seção 6)
#     n        = 90   -> tamanho da série
#     sigma    = 15   -> desvio padrão do ruído branco
#     q        = 3    -> raio da média móvel (janela de 2q+1 = 7 pontos)
#     max_lag  = 20   -> maior defasagem da autocovariância/FAC
#     semente  = 123  -> set.seed(123)
#     t        = 1:n  -> vetor temporal
#     beta     = c(2, 0.01, 0.01) -> coeficientes do polinômio
#                (beta1 + beta2*t + beta3*t^2)
#     X        = matriz n x 3 de regressão: colunas 1, t, t^2
#
# (b) SAÍDAS
#     Xt          : série simulada com tendência (vetor de tamanho n)
#     m_q         : médias móveis centradas (vetor de tamanho n - 2q)
#     media_Xt    : média amostral de Xt
#     gamma_1     : autocovariância empírica de lag 1
#     fac_Xt      : FAC empírica (vetor de tamanho max_lag + 1)
#     Console     : média, gamma_1 e FAC
#     Gráficos    : série + média móvel, FAC, e acf() do R
#
# (c) ESTRUTURAS DE DADOS
#     Vetores numéricos: t, beta, eps, Xt, m_q, autocovariancias, fac
#     Matriz numérica  : X (n linhas x 3 colunas)
#                        linha i = (1, t_i, t_i^2)
#
# (d) FUNÇÕES
#     Ruído branco      : simular_ruido_branco
#     Série c/ tendência: simular_serie_com_tendencia
#     Média móvel       : calcular_media_movel
#     Estatísticas      : calcular_media_amostral,
#                         calcular_autocovariancia_empirica,
#                         calcular_autocovariancias,
#                         calcular_fac_empirica
#     Gráficos          : plot_serie_temporal, plot_serie_media_movel,
#                         plot_fac_empirica
#
# (e) FLUXO DE EXECUÇÃO
#     1. Semente e parâmetros
#     2. Definição das funções
#     3. Vetor de tempo, beta e matriz X (loop)
#     4. Simulação de Xt (loops aninhados)
#     5. Média amostral, média móvel, gamma_1 e FAC
#     6. Resultados no console
#     7. Gráficos (série + média móvel e FAC juntos) e acf() do R
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

# RUÍDO BRANCO: n valores independentes de N(0, sigma^2)
simular_ruido_branco <- function(n, sigma){
  
  eps <- numeric(n)
  
  for(t in 1:n){
    eps[t] <-rnorm(1, mean = 0, sd = sigma)
  }
  
  return(eps)
}

# SÉRIE COM TENDÊNCIA POLINOMIAL
# X_t = beta1*1 + beta2*t + beta3*t^2 + eps_t
# Em forma de matriz: Xt = X %*% beta + eps
simular_serie_com_tendencia <- function(beta, X, sigma){
  
  n <- nrow(X)
  
  eps <-simular_ruido_branco(n, sigma)
  
  Xt <- numeric(n)
  
  numero_colunas <- ncol(X)
  
  # LOOPS ANINHADOS (multiplicação matriz x vetor, elemento a elemento):
  # - o loop de fora percorre os instantes t (linhas de X);
  # - para cada t, "valor" começa em 0 e o loop de dentro percorre
  #   as colunas j, somando X[t,j] * beta[j]. Isso é o produto
  #   escalar da linha t de X com o vetor beta, ou seja, o valor
  #   da tendência no instante t: beta1 + beta2*t + beta3*t^2.
  # - por fim soma-se o ruído eps[t] e guarda-se em Xt[t].
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

# MÉDIA MÓVEL CENTRADA DE RAIO q
# m_t = (1/(2q+1)) * soma_{j=t-q}^{t+q} X_j
calcular_media_movel <- function(serie,q){
  
  n <- length(serie)
  
  numero_medias <- n - 2*q
  
  medias <- numeric(numero_medias)
  
  if(numero_medias > 0){
    
    # o loop de fora escolhe o centro t da janela. Só vai de q+1
    # até n-q, para a janela [t-q, t+q] caber dentro da série.
    for(t in (q+1):(n-q)){
      
      soma <- 0
      
      # o loop de dentro soma os 2q+1 pontos da janela
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

calcular_media_amostral <- function(serie){
  
  n <- length(serie)
  
  soma <- 0
  
  for(t in 1:n){
    soma <-soma + serie[t]
  }
  
  media <-soma / n
  
  return(media)
}

# gamma_hat(h) = (1/n) * soma_{t=1}^{n-h} (X_t - X_bar)(X_{t+h} - X_bar)
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

# rho_hat(h) = gamma_hat(h) / gamma_hat(0)
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
# 6. EXECUÇÃO
############################################################

# VETOR DE TEMPO
t <- 1:n

# COEFICIENTES DO POLINÔMIO
beta <- c(2, 0.01, 0.01)

# MATRIZ DE REGRESSÃO X (n linhas x 3 colunas)
# Começa toda zerada e é preenchida linha por linha.
X <- matrix(
  0,
  nrow = n,
  ncol = 3
)

# LOOP DE CONSTRUÇÃO DA MATRIZ DE TENDÊNCIA:
# para cada instante i, a linha i recebe as três "potências" de t:
#   coluna 1 = 1       (termo constante)
#   coluna 2 = t_i     (termo linear)
#   coluna 3 = t_i^2   (termo quadrático)
# Assim, a linha i multiplicada por beta dá
# beta1 + beta2*t_i + beta3*t_i^2.
for(i in 1:n){
  
  X[i,1] <- 1
  
  X[i,2] <- t[i]
  
  X[i,3] <- t[i]^2
}

# SIMULAÇÃO DA SÉRIE
Xt <-simular_serie_com_tendencia(beta, X, sigma)

# MÉDIA AMOSTRAL
media_Xt <-calcular_media_amostral(Xt)

cat("Média amostral de Xt:", media_Xt, "\n")

# MÉDIA MÓVEL
m_q <-calcular_media_movel(Xt,q)

# AUTOCOVARIÂNCIA DE LAG 1
gamma_1 <-calcular_autocovariancia_empirica(Xt,h = 1)

cat("Autocovariância empírica de defasagem 1:",gamma_1,"\n")

# FAC
fac_Xt <-calcular_fac_empirica(Xt,max_lag)

cat("FAE (autocorrelação empírica) para lags 0 a",max_lag,":\n")

print(fac_Xt)

# GRÁFICOS
par(mfrow = c(2,1),mar = c(4,4,2,2))

plot_serie_media_movel(tempo = t, serie = Xt, media_movel = m_q, q = q)

plot_fac_empirica(
  fac = fac_Xt,
  titulo = "Função de Autocorrelação Empírica (FAE)",
  cor = "skyblue"
)

par(mfrow = c(1,1))

# COMPARAÇÃO COM A FUNÇÃO BUILT-IN DO R
acf(Xt, main = "ACF (R) – Série Temporal")
