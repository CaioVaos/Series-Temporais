############################################################
# config.R
# RESPONSABILIDADE: parâmetros globais, semente, tempo e coeficientes.
# DEPENDÊNCIAS: nenhuma.
#
# INTERFACE (variáveis criadas):
#   n_anos         inteiro > 0       : número de anos
#   s              inteiro >= 2      : período sazonal
#   n              inteiro           : n_anos * s
#   sigma          numérico > 0      : desvio padrão do ruído
#   t              vetor 1:n         : tempo
#   grau_tendencia inteiro >= 0      : grau do polinômio
#   beta_tendencia vetor (grau+1)    : coeficientes da tendência
#   K              inteiro, K < s/2  : harmônicos de Fourier
#   alpha, beta_h  vetores (K)       : coeficientes de cos e sen
#   beta_sazonal   vetor (2K)        : alpha e beta_h intercalados
#   q              inteiro, 2q+1 <= n: raio da média móvel
#   n_prev         inteiro > 0       : horizonte de previsão
#   t_futuro       vetor (n+1):(n+n_prev)
#   (semente definida com set.seed)
############################################################

set.seed(42)

n_anos <- 10

s <- 12

n <- n_anos * s

sigma <- 5

t <- 1:n

grau_tendencia <- 2

beta_tendencia <- c(2, 0.05, 0.001)

K <- 2

alpha <- c(2, 1)

beta_h <- c(-1, 0.5)

# vetor sazonal: cos1, sen1, cos2, sen2, ...
beta_sazonal <- numeric(2 * K)

for(k in 1:K){
  
  coluna_cos <- 2*k - 1
  coluna_sin <- 2*k
  
  beta_sazonal[coluna_cos] <- alpha[k]
  
  beta_sazonal[coluna_sin] <- beta_h[k]
}

q <- 5

n_prev <- 12

t_futuro <- (n+1):(n+n_prev)
