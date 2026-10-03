############################################################
# config.R
# RESPONSABILIDADE: definir os parâmetros globais do modelo.
# DEPENDÊNCIAS: nenhuma.
#
# INTERFACE (variáveis criadas):
#   n       numérico inteiro > 0  : tamanho das séries
#   max_lag numérico inteiro >= 0 : maior lag da autocovariância/FAC
#                                   (deve ser menor que n)
#   sigma2  numérico > 0          : variância do ruído branco
#   phi     numérico, |phi| < 1   : coeficiente do AR(1)
#   (semente aleatória definida com set.seed)
############################################################

n <- 200

max_lag <- 20

sigma2 <- 1

phi <- 0.7

set.seed(123)
