############################################################
# AULA PRÁTICA – PASSEIO ALEATÓRIO NA ESFERA UNITÁRIA
# TESTES EMPÍRICOS DE ERGODICIDADE
############################################################

rm(list = ls())

set.seed(123)

############################################################
# 1. PARÂMETROS GERAIS
############################################################

num_steps <- 10000000

tamanho_passo <- 0.1

############################################################
# 2. FUNÇÕES BÁSICAS
############################################################

############################################################
# PRODUTO INTERNO
############################################################

calcular_produto_interno <- function(vetor1, vetor2){
  
  n <- length(vetor1)
  
  soma <- 0
  
  for(i in 1:n){
    soma <- soma + vetor1[i] * vetor2[i]
  }
  
  return(soma)
}

############################################################
# NORMA EUCLIDIANA
############################################################

calcular_norma_euclidiana <- function(vetor){
  
  n <- length(vetor)
  
  soma <- 0
  
  for(i in 1:n){
    soma <- soma + vetor[i]^2
  }
  
  norma <- sqrt(soma)
  
  return(norma)
}

############################################################
# NORMALIZAÇÃO DE UM VETOR
############################################################

normalizar_vetor <- function(vetor){
  
  n <- length(vetor)
  
  norma <- calcular_norma_euclidiana(vetor)
  
  vetor_normalizado <- numeric(n)
  
  if(norma > 0){
    
    for(i in 1:n){
      vetor_normalizado[i] <-vetor[i] / norma
    }
  }
  
  return(vetor_normalizado)
}

############################################################
# 3. PROJEÇÃO NA TANGENTE DA ESFERA
############################################################

############################################################
# PROJEÇÃO DE UM VETOR NO PLANO TANGENTE
#
# v_t = v - <v,p> p
#
# onde:
#
# v  = vetor original
# p  = posição atual
# v_t = vetor projetado na tangente
############################################################

projetar_vetor_tangente <- function(vetor, posicao_atual){
  
  produto <-calcular_produto_interno(vetor, posicao_atual)
  
  n <- length(vetor)
  
  vetor_tangente <- numeric(n)
  
  for(i in 1:n){
    vetor_tangente[i] <-vetor[i] -produto * posicao_atual[i]
  }
  
  return(vetor_tangente)
}

############################################################
# 4. SIMULAÇÃO DO PASSEIO ALEATÓRIO
############################################################

############################################################
# PASSEIO ALEATÓRIO NA ESFERA UNITÁRIA
############################################################

simular_passeio_esfera <- function(num_steps, tamanho_passo){
  
  ##########################################################
  # CONDIÇÃO INICIAL
  ##########################################################
  
  posicao_atual <- c(0, 0, 1)
  
  ##########################################################
  # MATRIZ PARA ARMAZENAR O CAMINHO
  ##########################################################
  
  caminho <- matrix(0,nrow = num_steps + 1,ncol = 3)
  
  caminho[1, ] <- posicao_atual
  
  ##########################################################
  # CONSTRUÇÃO RECURSIVA
  ##########################################################
  
  if(num_steps >= 1){
    
    for(t in 2:(num_steps + 1)){
      
      ######################################################
      # GERA VETOR ALEATÓRIO
      ######################################################
      
      vetor_aleatorio <- numeric(3)
      
      for(i in 1:3){
        vetor_aleatorio[i] <- rnorm(1)
      }
      
      ######################################################
      # PROJETA O VETOR NA TANGENTE
      ######################################################
      
      vetor_aleatorio <-projetar_vetor_tangente(vetor_aleatorio,posicao_atual)
      
      ######################################################
      # NORMALIZA O VETOR
      ######################################################
      
      vetor_aleatorio <-normalizar_vetor(vetor_aleatorio)
      
      ######################################################
      # DEFINE O TAMANHO DO PASSO
      ######################################################
      
      for(i in 1:3){
        vetor_aleatorio[i] <-vetor_aleatorio[i] * tamanho_passo
      }
      
      ######################################################
      # ATUALIZA A POSIÇÃO
      ######################################################
      
      nova_posicao <- numeric(3)
      
      for(i in 1:3){
        nova_posicao[i] <-posicao_atual[i] +vetor_aleatorio[i]
      }
      
      ######################################################
      # NORMALIZA A NOVA POSIÇÃO
      ######################################################
      
      nova_posicao <-normalizar_vetor(nova_posicao)
      
      ######################################################
      # ATUALIZA A POSIÇÃO ATUAL
      ######################################################
      
      posicao_atual <- nova_posicao
      
      ######################################################
      # ARMAZENA A POSIÇÃO
      ######################################################
      
      caminho[t, ] <- posicao_atual
    }
  }
  
  return(caminho)
}

############################################################
# 5. COORDENADAS ESFÉRICAS PARA CARTESIANAS
############################################################

############################################################
# x = cos(u) * sin(v)
# y = sin(u) * sin(v)
# z = cos(v)
############################################################

coordenadas_esfericas_cartesianas <- function(u, v){
  
  n_u <- length(u)
  
  n_v <- length(v)
  
  x <- matrix(0,nrow = n_u,ncol = n_v)
  
  y <- matrix(0,nrow = n_u,ncol = n_v)
  
  z <- matrix(0,nrow = n_u,ncol = n_v)
  
  for(i in 1:n_u){
    
    for(j in 1:n_v){
      
      x[i,j] <-cos(u[i]) * sin(v[j])
      
      y[i,j] <-sin(u[i]) * sin(v[j])
      
      z[i,j] <-cos(v[j])
    }
  }
  
  return(list(x = x,y = y,z = z))
}

############################################################
# 6. SIMULAÇÃO
############################################################

caminho <-simular_passeio_esfera(num_steps,tamanho_passo)

############################################################
# 7. MALHA DA ESFERA
############################################################

u <- seq(0,2*pi,length.out = 100)

v <- seq(0,pi,length.out = 100)

coords <-coordenadas_esfericas_cartesianas(u,v)

############################################################
# 8. VISUALIZAÇÃO 3D
############################################################

library(rgl)

open3d()

surface3d(
  coords$x,
  coords$y,
  coords$z,
  color = "lightblue",
  alpha = 0.3,
  lit = FALSE
)

lines3d(
  caminho[,1],
  caminho[,2],
  caminho[,3],
  color = "red",
  lwd = 2
)

axes3d()

title3d(
  xlab = "X",
  ylab = "Y",
  zlab = "Z"
)

############################################################
# 9. MÉDIA AMOSTRAL
############################################################

############################################################
# MÉDIA AMOSTRAL DE UM VETOR
############################################################

calcular_media_amostral <- function(vetor){
  
  n <- length(vetor)
  
  soma <- 0
  
  for(i in 1:n){
    soma <- soma + vetor[i]
  }
  
  media <- soma / n
  
  return(media)
}
############################################################
# MÉDIA AMOSTRAL DO PASSEIO
############################################################

calcular_media_passeio <- function(caminho){
  
  media_x <-calcular_media_amostral(caminho[,1])
  
  media_y <-calcular_media_amostral(caminho[,2])
  
  media_z <-calcular_media_amostral(caminho[,3])
  
  return(c(x = media_x,y = media_y,z = media_z)
  )
}

############################################################
# 10. FUNÇÃO DE AUTOCORRELAÇÃO EMPÍRICA
############################################################

############################################################
# AUTOCORRELAÇÃO EMPÍRICA
############################################################

calcular_autocorrelacao_empirica <- function(vetor,lag){
  
  n <- length(vetor)
  
  media <- calcular_media_amostral(vetor)
  
  numerador <- 0
  
  denominador <- 0
  
  ##########################################################
  # NUMERADOR
  ##########################################################
  
  if(lag < n){
    for(t in 1:(n - lag)){
      numerador <-numerador +(vetor[t] - media) *(vetor[t + lag] - media)
    }
  }
  
  ##########################################################
  # DENOMINADOR
  ##########################################################
  
  for(t in 1:n){
    denominador <-denominador +(vetor[t] - media)^2
  }
  
  ##########################################################
  # AUTOCORRELAÇÃO
  ##########################################################
  
  if(denominador > 0){
    autocorrelacao <-numerador / denominador
  } else {
    autocorrelacao <- NA
  }
  
  return(autocorrelacao)
}

############################################################
# AUTOCORRELAÇÃO PARA VÁRIAS DEFASAGENS
############################################################

calcular_acf_empirica <- function(vetor, max_lag){
  
  autocorrelacoes <- numeric(max_lag + 1)
  
  for(lag in 0:max_lag){
    autocorrelacoes[lag + 1] <-calcular_autocorrelacao_empirica(vetor,lag)
  }
  
  return(autocorrelacoes)
}

############################################################
# 11. APLICAÇÃO AO PASSEIO
############################################################

############################################################
# MÉDIA AMOSTRAL DAS COORDENADAS
############################################################

media_passeio <-calcular_media_passeio(caminho)

print(media_passeio)

############################################################
# AUTOCORRELAÇÃO EMPÍRICA
############################################################

max_lag <- 100

acf_x <-calcular_acf_empirica(caminho[,1],max_lag)

acf_y <-calcular_acf_empirica(caminho[,2],max_lag)

acf_z <-calcular_acf_empirica(caminho[,3],max_lag)

############################################################
# VISUALIZAÇÃO DA AUTOCORRELAÇÃO
############################################################

lags <- 0:max_lag

plot(
  lags,
  acf_x,
  type = "l",
  col = "red",
  lwd = 2,
  ylim = c(-1, 1),
  xlab = "Defasagem (lag)",
  ylab = "Autocorrelação",
  main = "Função de Autocorrelação Empírica"
)

lines(
  lags,
  acf_y,
  col = "blue",
  lwd = 2
)

lines(
  lags,
  acf_z,
  col = "darkgreen",
  lwd = 2
)

abline(
  h = 0,
  col = "black"
)

legend(
  "topright",
  legend = c("X", "Y", "Z"),
  col = c("red", "blue", "darkgreen"),
  lwd = 2
)

############################################################
# MÉDIAS DOS QUADRADOS
############################################################

media_quadrados <- c(
  x2 = calcular_media_amostral(caminho[,1]^2),
  y2 = calcular_media_amostral(caminho[,2]^2),
  z2 = calcular_media_amostral(caminho[,3]^2)
)

print(media_quadrados)

############################################################
# 12. TESTES DE ERGODICIDADE
############################################################

############################################################
# 12.1 MÉDIAS TEMPORAIS ACUMULADAS
############################################################

calcular_medias_temporais <- function(vetor){
  
  n <- length(vetor)
  
  medias <- numeric(n)
  
  soma <- 0
  
  for(t in 1:n){
    soma <- soma + vetor[t]
    medias[t] <- soma / t
  }
  
  return(medias)
}

############################################################
# 12.2 MÉDIAS TEMPORAIS DAS COORDENADAS
############################################################

media_temporal_x <-calcular_medias_temporais(caminho[,1])

media_temporal_y <-calcular_medias_temporais(caminho[,2])

media_temporal_z <-calcular_medias_temporais(caminho[,3])

############################################################
# VALORES TEÓRICOS PARA A MEDIDA UNIFORME
# E[X] = E[Y] = E[Z] = 0
############################################################

valor_teorico_x <- 0

valor_teorico_y <- 0

valor_teorico_z <- 0

############################################################
# 12.3 GRÁFICO DAS MÉDIAS TEMPORAIS
############################################################

plot(
  media_temporal_x,
  type = "l",
  col = "red",
  lwd = 2,
  ylim = c(-1, 1),
  xlab = "Número de passos",
  ylab = "Média temporal",
  main = "Convergência das Médias Temporais"
)

lines(
  media_temporal_y,
  col = "blue",
  lwd = 2
)

lines(
  media_temporal_z,
  col = "darkgreen",
  lwd = 2
)

abline(
  h = 0,
  col = "black",
  lwd = 2
)

legend(
  "topright",
  legend = c("X", "Y", "Z"),
  col = c("red", "blue", "darkgreen"),
  lwd = 2
)

############################################################
# 12.4 MÉDIAS TEMPORAIS DOS QUADRADOS
############################################################

############################################################
# Para a distribuição uniforme na esfera:
#
# E[X²] = E[Y²] = E[Z²] = 1/3
############################################################

media_temporal_x2 <-calcular_medias_temporais(caminho[,1]^2)

media_temporal_y2 <-calcular_medias_temporais(caminho[,2]^2)

media_temporal_z2 <-calcular_medias_temporais(caminho[,3]^2)

############################################################
# VALOR TEÓRICO
############################################################

valor_teorico_quadrado <- 1 / 3

############################################################
# 12.5 GRÁFICO DAS MÉDIAS TEMPORAIS DOS QUADRADOS
############################################################

plot(
  media_temporal_x2,
  type = "l",
  col = "red",
  lwd = 2,
  ylim = c(0, 0.7),
  xlab = "Número de passos",
  ylab = "Média temporal",
  main = "Convergência das Médias Temporais dos Quadrados"
)

lines(
  media_temporal_y2,
  col = "blue",
  lwd = 2
)

lines(
  media_temporal_z2,
  col = "darkgreen",
  lwd = 2
)

abline(
  h = valor_teorico_quadrado,
  col = "black",
  lwd = 2
)

legend(
  "topright",
  legend = c(
    "X²",
    "Y²",
    "Z²",
    "Valor teórico = 1/3"
  ),
  col = c(
    "red",
    "blue",
    "darkgreen",
    "black"
  ),
  lwd = 2
)

###########################################################
# 12.8 MOMENTO DE QUARTA ORDEM
############################################################

############################################################
# Para a distribuição uniforme na esfera:
#
# E[X^4] = E[Y^4] = E[Z^4] = 1/5
#
############################################################

x4 <- numeric(length(caminho[,1]))

y4 <- numeric(length(caminho[,2]))

z4 <- numeric(length(caminho[,3]))

for(t in 1:length(caminho[,1])){
  
  x4[t] <- caminho[t,1]^4
  
  y4[t] <- caminho[t,2]^4
  
  z4[t] <- caminho[t,3]^4
}

############################################################
# MÉDIAS DOS QUARTOS PODERES
############################################################

media_x4 <-calcular_media_amostral(x4)

media_y4 <-calcular_media_amostral(y4)

media_z4 <-calcular_media_amostral(z4)

############################################################
# VALOR TEÓRICO
############################################################

valor_teorico_quarta_ordem <- 1 / 5

############################################################
# RESULTADOS
############################################################

media_quarta_ordem <- c(
  x4 = media_x4,
  y4 = media_y4,
  z4 = media_z4
)

print(media_quarta_ordem)

print(c(valor_teorico = valor_teorico_quarta_ordem))

############################################################
# 12.9 MÉDIA TEMPORAL DE X^4
############################################################

media_temporal_x4 <-calcular_medias_temporais(x4)

media_temporal_y4 <-calcular_medias_temporais(y4)

media_temporal_z4 <-calcular_medias_temporais(z4)

############################################################
# GRÁFICO
############################################################

plot(
  media_temporal_x4,
  type = "l",
  col = "red",
  lwd = 2,
  ylim = c(0, 0.35),
  xlab = "Número de passos",
  ylab = "Média temporal",
  main = "Convergência da Média Temporal de X^4, Y^4 e Z^4"
)

lines(
  media_temporal_y4,
  col = "blue",
  lwd = 2
)

lines(
  media_temporal_z4,
  col = "darkgreen",
  lwd = 2
)

abline(
  h = valor_teorico_quarta_ordem,
  col = "black",
  lwd = 2
)

legend(
  "topright",
  legend = c(
    "X^4",
    "Y^4",
    "Z^4",
    "Valor teórico = 1/5"
  ),
  col = c(
    "red",
    "blue",
    "darkgreen",
    "black"
  ),
  lwd = 2
)

############################################################
# 15. TESTE DE ERGODICIDADE PARA DIFERENTES TAMANHOS
#     DE AMOSTRA
############################################################

############################################################
# Vamos observar o comportamento da média temporal para
# diferentes tamanhos de passeio.
#
# N = 100
# N = 1.000
# N = 10.000
# N = 100.000
# N = 1.000.000
# N = 10.000.000
############################################################

tamanhos_amostra <- c(100,1000,10000,100000,1000000,10000000)

numero_tamanhos <-length(tamanhos_amostra)

############################################################
# CÁLCULO
############################################################

for(i in 1:numero_tamanhos){
  
  tamanho_atual <-tamanhos_amostra[i]
  
  media_x2_atual <-calcular_media_amostral(caminho[1:tamanho_atual,1]^2)
  
  media_y2_atual <-calcular_media_amostral(caminho[1:tamanho_atual,2]^2)
  
  media_z2_atual <-calcular_media_amostral(caminho[1:tamanho_atual,3]^2)
}

############################################################
# 16. RESUMO DOS TESTES
############################################################

############################################################
# MÉDIAS DAS COORDENADAS
############################################################

cat("\n============================================\n")

cat("MÉDIAS DAS COORDENADAS\n")

cat("============================================\n")

print(media_passeio)

############################################################
# MÉDIAS DOS QUADRADOS
############################################################

cat("\n============================================\n")

cat("MÉDIAS DOS QUADRADOS\n")

cat("============================================\n")

print(media_quadrados)

cat("Valor teórico = ",valor_teorico_quadrado,"\n")

############################################################
# MÉDIAS DOS QUARTOS PODERES
############################################################

cat("\n============================================\n")

cat("MÉDIAS DOS QUARTOS PODERES\n")

cat("============================================\n")

print(media_quarta_ordem)

cat("Valor teórico = ",valor_teorico_quarta_ordem,"\n")

############################################################
# CONCLUSÃO EMPÍRICA
############################################################

cat("\n============================================\n")

cat("CONCLUSÃO EMPÍRICA\n")

cat("============================================\n")

cat("As médias temporais das coordenadas devem\n")

cat("convergir para 0.\n")

cat("As médias temporais dos quadrados devem\n")

cat("convergir para 1/3.\n")

cat("As médias temporais dos quartos poderes devem\n")

cat("convergir para 1/5.\n")

cat("============================================\n")
