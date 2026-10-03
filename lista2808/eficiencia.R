############################################################
# eficiencia.R – ANÁLISE DE EFICIÊNCIA (Exercício 4.3)
# Compara loops for aninhados x operações matriciais do R (%*%).
# Executar: Rscript eficiencia.R
############################################################

rm(list = ls())

source("config.R", encoding = "UTF-8")
source("dados.R", encoding = "UTF-8")
source("processamento.R", encoding = "UTF-8")

############################################################
# VERSÕES VETORIZADAS
############################################################

# t(X) é a transposta e %*% é o produto matricial (feito em C/BLAS)
XtX_vetorizada <- function(X){
  return(t(X) %*% X)
}

Xty_vetorizada <- function(X, y){
  return(as.vector(t(X) %*% y))
}

ols_vetorizada <- function(X, y){
  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  return(as.vector(solve(XtX, Xty)))
}

############################################################
# 1. CORREÇÃO MATEMÁTICA (série original, n = 120)
############################################################

X_tendencia <- criar_matriz_tendencia(t, grau_tendencia)
X_sazonal <- criar_matriz_fourier(t, s, K)
Xt <- simular_serie(beta_tendencia, X_tendencia, X_sazonal, beta_sazonal, sigma)
X_full <- cbind(X_tendencia, X_sazonal)

beta_loop <- ajustar_ols(X_full, Xt)
beta_vet <- ols_vetorizada(X_full, Xt)
beta_lm <- as.numeric(coef(lm(Xt ~ X_full - 1)))

cat("Maior |dif| beta loops x %*%:", max(abs(beta_loop - beta_vet)), "\n")
cat("Maior |dif| beta loops x lm():", max(abs(beta_loop - beta_lm)), "\n")
cat("Maior |dif| X%*%beta x loop:",
    max(abs(multiplicar_matriz_vetor(X_full, beta_loop) - as.vector(X_full %*% beta_loop))), "\n\n")

############################################################
# 2. DESEMPENHO (série longa)
# (só X'X, X'y e X%*%beta; o ajuste com t^2 até 1e10 fica mal
#  condicionado, então os betas foram validados acima, com n = 120)
############################################################

n_grande <- 200000
t_grande <- 1:n_grande

X_tend_g <- criar_matriz_tendencia(t_grande, 2)
X_saz_g <- criar_matriz_fourier(t_grande, 12, 2)
X_g <- cbind(X_tend_g, X_saz_g)

set.seed(1)
y_g <- rnorm(n_grande)

cat("n =", n_grande, "| colunas =", ncol(X_g), "\n\n")

cat("--- X'X ---\n")
t1 <- system.time(a1 <- calcular_XtX(X_g))
t2 <- system.time(a2 <- XtX_vetorizada(X_g))
cat("Loops for  :", t1["elapsed"], "s\n")
cat("%*%        :", t2["elapsed"], "s\n")
cat("Ganho      :", t1["elapsed"] / max(t2["elapsed"], 0.001), "x\n")
cat("Maior dif. relativa:", max(abs(a1 - a2) / (abs(a2) + 1e-12)), "\n\n")

cat("--- X'y ---\n")
t3 <- system.time(b1 <- calcular_Xty(X_g, y_g))
t4 <- system.time(b2 <- Xty_vetorizada(X_g, y_g))
cat("Loops for  :", t3["elapsed"], "s\n")
cat("%*%        :", t4["elapsed"], "s\n")
cat("Ganho      :", t3["elapsed"] / max(t4["elapsed"], 0.001), "x\n")
cat("Maior dif. relativa:", max(abs(b1 - b2) / (abs(b2) + 1e-12)), "\n\n")

cat("--- X %*% beta ---\n")
beta_teste <- c(2, 0.05, 0.001, 2, -1, 1, 0.5)
t5 <- system.time(c1 <- multiplicar_matriz_vetor(X_g, beta_teste))
t6 <- system.time(c2 <- as.vector(X_g %*% beta_teste))
cat("Loops for  :", t5["elapsed"], "s\n")
cat("%*%        :", t6["elapsed"], "s\n")
cat("Ganho      :", t5["elapsed"] / max(t6["elapsed"], 0.001), "x\n")
cat("Maior dif. relativa:", max(abs(c1 - c2) / (abs(c2) + 1e-12)), "\n")
