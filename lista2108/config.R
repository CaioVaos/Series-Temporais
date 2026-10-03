############################################################
# config.R
# RESPONSABILIDADE: parâmetros, semente, vetor de tempo,
#                   coeficientes e matriz de tendência polinomial.
# DEPENDÊNCIAS: nenhuma.
#
# INTERFACE (variáveis criadas):
#   n       inteiro > 0          : tamanho da série
#   sigma   numérico > 0         : desvio padrão do ruído
#   q       inteiro >= 0         : raio da média móvel (precisa 2q+1 <= n)
#   max_lag inteiro >= 0         : maior lag da FAC (menor que n)
#   t       vetor 1:n            : tempo
#   beta    vetor numérico (3)   : coeficientes do polinômio
#   X       matriz numérica n x 3: colunas 1, t, t^2
#   (semente definida com set.seed)
############################################################

set.seed(123)

n <- 90

sigma <- 15

q <- 3

max_lag <- 20

t <- 1:n

beta <- c(2, 0.01, 0.01)

# Matriz de regressão: linha i = (1, t_i, t_i^2)
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
