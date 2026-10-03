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

# Média: mean() faz a soma em código compilado (C), sem loop em R
media_vetorizada <- function(serie){
  return(mean(serie))
}

# Autocovariância: em vez de percorrer t um por um, pega o
# vetor inteiro dos desvios e multiplica "deslocado":
#   d[1:(n-h)]   -> X_1 ... X_{n-h}
#   d[(h+1):n]   -> X_{1+h} ... X_n
# O produto elemento a elemento e o sum() fazem de uma vez o
# que o for fazia. Para h = 0 a fórmula vira d*d (variância),
# então não precisa do if(h == 0).
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

# Passeio aleatório: X_t = eps_1 + ... + eps_t é a soma acumulada
passeio_vetorizado <- function(n, sigma2){
  eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))
  return(cumsum(eps))
}

############################################################
# 1. CORREÇÃO MATEMÁTICA (mesma série nas duas versões)
############################################################

serie <- simular_ar1(n, phi, sigma2)

fac_loop <- calcular_fac_empirica(serie, max_lag)
fac_vet <- fac_vetorizada(serie, max_lag)
fac_R <- acf(serie, lag.max = max_lag, plot = FALSE)$acf

cat("Maior |dif| loop x vetorizada:", max(abs(fac_loop - fac_vet)), "\n")
cat("Maior |dif| loop x acf() do R:", max(abs(fac_loop - as.vector(fac_R))), "\n\n")

# passeio: mesma semente => mesmos eps => mesma série
set.seed(1)
p_loop <- simular_passeio_aleatorio(1000, 1)
set.seed(1)
p_vet <- passeio_vetorizado(1000, 1)
cat("Maior |dif| passeio loop x cumsum:", max(abs(p_loop - p_vet)), "\n\n")

############################################################
# 2. DESEMPENHO
############################################################

n_grande <- 100000
lag_grande <- 50

set.seed(1)
serie_grande <- simular_ar1(n_grande, phi, sigma2)

cat("n =", n_grande, "| max_lag =", lag_grande, "\n\n")

cat("--- FAC (autocovariâncias) ---\n")
t_loop <- system.time(f1 <- calcular_fac_empirica(serie_grande, lag_grande))
t_vet <- system.time(f2 <- fac_vetorizada(serie_grande, lag_grande))
cat("Loop for   :", t_loop["elapsed"], "s\n")
cat("Vetorizada :", t_vet["elapsed"], "s\n")
cat("Ganho      :", t_loop["elapsed"] / max(t_vet["elapsed"], 0.001), "x\n")
cat("Maior |dif|:", max(abs(f1 - f2)), "\n\n")

cat("--- Passeio aleatório (soma acumulada) ---\n")
set.seed(1)
t_loop2 <- system.time(q1 <- simular_passeio_aleatorio(n_grande, 1))
set.seed(1)
t_vet2 <- system.time(q2 <- passeio_vetorizado(n_grande, 1))
cat("Loop for   :", t_loop2["elapsed"], "s\n")
cat("cumsum     :", t_vet2["elapsed"], "s\n")
cat("Ganho      :", t_loop2["elapsed"] / max(t_vet2["elapsed"], 0.001), "x\n")
cat("Maior |dif|:", max(abs(q1 - q2)), "\n")
