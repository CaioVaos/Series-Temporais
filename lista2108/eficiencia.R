############################################################
# eficiencia.R – ANÁLISE DE EFICIÊNCIA (Exercício 4.3)
# Compara loops for explícitos x versões vetorizadas.
# Executar: Rscript eficiencia.R
############################################################

rm(list = ls())

source("config.R", encoding = "UTF-8")
source("dados.R", encoding = "UTF-8")
source("processamento.R", encoding = "UTF-8")

############################################################
# VERSÕES VETORIZADAS
############################################################

# Matriz X: cbind() junta as 3 colunas de uma vez (1, t, t^2)
matriz_vetorizada <- function(tempo){
  return(cbind(1, tempo, tempo^2))
}

# Série com tendência: X %*% beta faz, de uma vez, os loops
# aninhados (produto de cada linha de X por beta).
# rnorm(n) já sorteia o vetor inteiro de ruído.
serie_vetorizada <- function(beta, X, sigma){
  n <- nrow(X)
  eps <- rnorm(n, mean = 0, sd = sigma)
  Xt <- as.vector(X %*% beta) + eps
  return(Xt)
}

# Autocovariância: d guarda os desvios; d[1:(n-h)] e d[(h+1):n]
# são o mesmo vetor deslocado h posições. O produto elemento a
# elemento + sum() faz de uma vez o que o for fazia.
# Com h = 0 vira a variância, então o if(h == 0) não é preciso.
autocovariancia_vetorizada <- function(serie, h){
  n <- length(serie)
  d <- serie - mean(serie)
  soma <- sum(d[1:(n-h)] * d[(h+1):n])
  return(soma / n)
}

fac_vetorizada <- function(serie, max_lag){
  gamma <- numeric(max_lag + 1)
  for(h in 0:max_lag){
    gamma[h+1] <- autocovariancia_vetorizada(serie, h)
  }
  return(gamma / gamma[1])
}

############################################################
# 1. CORREÇÃO MATEMÁTICA (série original, n = 90)
############################################################

X_vet <- matriz_vetorizada(t)
cat("Maior |dif| matriz X loop x cbind:", max(abs(X - X_vet)), "\n")

# mesma semente => mesmos eps => mesma série nas duas versões
set.seed(1)
Xt_loop <- simular_serie_com_tendencia(beta, X, sigma)
set.seed(1)
Xt_vet <- serie_vetorizada(beta, X, sigma)
cat("Maior |dif| série loop x vetorizada:", max(abs(Xt_loop - Xt_vet)), "\n")

fac_loop <- calcular_fac_empirica(Xt_loop, max_lag)
fac_vet <- fac_vetorizada(Xt_loop, max_lag)
fac_R <- acf(Xt_loop, lag.max = max_lag, plot = FALSE)$acf
cat("Maior |dif| FAC loop x vetorizada:", max(abs(fac_loop - fac_vet)), "\n")
cat("Maior |dif| FAC loop x acf() do R:", max(abs(fac_loop - as.vector(fac_R))), "\n\n")

############################################################
# 2. DESEMPENHO (série longa)
############################################################

n_grande <- 100000
lag_grande <- 50

t_grande <- 1:n_grande
X_grande <- matriz_vetorizada(t_grande)

cat("n =", n_grande, "| max_lag =", lag_grande, "| q =", q, "\n\n")

cat("--- Simulação da série com tendência ---\n")
set.seed(1)
t1 <- system.time(s1 <- simular_serie_com_tendencia(beta, X_grande, sigma))
set.seed(1)
t2 <- system.time(s2 <- serie_vetorizada(beta, X_grande, sigma))
cat("Loops for  :", t1["elapsed"], "s\n")
cat("Vetorizada :", t2["elapsed"], "s\n")
cat("Ganho      :", t1["elapsed"] / max(t2["elapsed"], 0.001), "x\n")
cat("Maior |dif|:", max(abs(s1 - s2)), "\n\n")

cat("--- FAC (somas de autocovariância) ---\n")
t5 <- system.time(f1 <- calcular_fac_empirica(s1, lag_grande))
t6 <- system.time(f2 <- fac_vetorizada(s1, lag_grande))
cat("Loops for  :", t5["elapsed"], "s\n")
cat("Vetorizada :", t6["elapsed"], "s\n")
cat("Ganho      :", t5["elapsed"] / max(t6["elapsed"], 0.001), "x\n")
cat("Maior |dif|:", max(abs(f1 - f2)), "\n")
