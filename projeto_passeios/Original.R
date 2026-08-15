############################################################
# AULA PRÁTICA – PROCESSOS ESTOCÁSTICOS 
############################################################

rm(list = ls())
set.seed(123)

############################################################
# 1. PARÂMETROS GERAIS
############################################################

sigma2 <- 1  # variância do ruído branco

############################################################
# 2. SIMULAÇÃO
############################################################

############################################################
# RUÍDO BRANCO
############################################################

simular_ruido_branco <- function(n){
  # eps_t ~ N(0,1)
  return(rnorm(n, mean = 0, sd = sqrt(sigma2)))
}

############################################################
# MÚLTIPLAS TRAJETÓRIAS DE RUÍDO BRANCO
############################################################

simular_multiplas_trajetorias_ruido <- function(n, m){
  
  trajetorias <- matrix(0, nrow = n, ncol = m)
  
  for(i in 1:m){
    trajetorias[, i] <- simular_ruido_branco(n)
  }
  
  return(trajetorias)
}

############################################################
# PASSEIO ALEATÓRIO
# X_t = X_{t-1} + eps_t
############################################################

simular_passeio_aleatorio <- function(n){
  
  eps <- simular_ruido_branco(n)
  
  X <- numeric(n)
  
  # condição inicial
  X[1] <- eps[1]
  
  # construção recursiva
  if(n >= 2){
    for(t in 2:n){
      X[t] <- X[t-1] + eps[t]
    }
  }
  
  return(X)
}

############################################################
# MÚLTIPLOS PASSEIOS ALEATORIOS
############################################################

simular_multiplos_passeio_aleatorio <- function(n, m){
  
  trajetorias <- matrix(0, nrow = n, ncol = m)
  
  for(i in 1:m){
    trajetorias[, i] <- simular_passeio_aleatorio(n)
  }
  
  return(trajetorias)
}

############################################################
# 3. ESTATÍSTICAS
############################################################

############################################################
# MÉDIA
############################################################

calcular_media_trajetoria <- function(trajetorias){
  
  n <- nrow(trajetorias)
  m <- ncol(trajetorias)
  
  media <- numeric(n)
  
  for(t in 1:n){
    
    soma <- 0
    
    for(i in 1:m){
      soma <- soma + trajetorias[t,i]
    }
    
    media[t] <- soma / m
  }
  
  return(media)
}

############################################################
# VARIÂNCIA
############################################################

calcular_variancia_trajetoria <- function(trajetorias){
  
  n <- nrow(trajetorias)
  m <- ncol(trajetorias)
  
  if(m < 2){
    stop("São necessárias pelo menos duas trajetórias.")
  }
  
  variancia <- numeric(n)
  
  for(t in 1:n){
    soma <- 0
    
    for(i in 1:m){
      soma <- soma + trajetorias[t,i]
    }
    
    media <- soma / m
    
    soma <- 0
    
    for(i in 1:m){
      d <- trajetorias[t,i] - media
      soma <- soma + d*d
    }
    
    variancia[t] <- soma / (m-1)
  }
  
  return(variancia)
}

############################################################
# 4. PLOTS
############################################################

############################################################
# SÉRIE TEMPORAL
############################################################

plot_serie_temporal <- function(serie, titulo, ylab = "X_t"){
  
  n <- length(serie)
  
  plot(1:n, serie,
       type = "l",
       lwd = 2,
       col = "gray",
       main = titulo,
       xlab = "Tempo (t)",
       ylab = ylab)
}

############################################################
# ACF
############################################################

plot_acf <- function(serie, titulo){
  acf(serie, main = titulo)
}

############################################################
# MÚLTIPLAS TRAJETÓRIAS
############################################################

plot_multiplas_trajetorias <- function(trajetorias, titulo, ylab = "X_t"){
  
  n <- nrow(trajetorias)
  m <- ncol(trajetorias)
  
  plot(1:n, trajetorias[,1],
       type = "n",
       xlim = c(1, n),
       ylim = range(trajetorias),
       xlab = "Tempo (t)",
       ylab = ylab,
       main = titulo)
  
  for(i in 1:m){
    lines(1:n, trajetorias[, i], col = "gray", lwd = 2)
  }
  
  abline(h = 0, col = "black", lty = 2)
}

############################################################
# TRAJETÓRIAS + MÉDIA
############################################################

plot_trajetorias_media <- function(trajetorias, media, titulo){
  
  n <- nrow(trajetorias)
  m <- ncol(trajetorias)
  
  plot(1:n, trajetorias[,1],
       type = "n",
       xlim = c(1, n),
       ylim = range(trajetorias),
       xlab = "Tempo (t)",
       ylab = "X_t",
       main = titulo)
  
  for(i in 1:m){
    lines(1:n, trajetorias[, i], col = "gray", lwd = 2)
  }
  
  lines(1:n, media, col = "black", lwd = 3)
  
  abline(h = 0, col = "black", lty = 2)
}

############################################################
# VARIÂNCIA
############################################################

plot_variancia <- function(variancia, referencia = NULL, titulo){
  
  n <- length(variancia)
  
  plot(1:n, variancia,
       type = "l",
       lwd = 2,
       col = "purple",
       xlab = "Tempo (t)",
       ylab = "Variância",
       main = titulo)
  
  if(!is.null(referencia)){
    lines(1:n, referencia, col = "red", lty = 2)
  }
}

############################################################
# 5. EXEMPLOS DE USO
############################################################

n <- 200
m <- 5

############################################################
# RUÍDO BRANCO
############################################################

eps <- simular_ruido_branco(n)

par(mfrow = c(2,1))

plot_serie_temporal(eps, "Ruído Branco", "eps_t")
plot_acf(eps, "ACF do Ruído Branco")

par(mfrow = c(1,1))

############################################################
# MÚLTIPLAS TRAJETÓRIAS (RUÍDO BRANCO)
############################################################

traj <- simular_multiplas_trajetorias_ruido(n, m)

plot_multiplas_trajetorias(traj,
                           "Múltiplas Trajetórias - Ruído Branco",
                           "eps_t")

media <- calcular_media_trajetoria(traj)
variancia <- calcular_variancia_trajetoria(traj)

par(mfrow = c(2,1))

plot_trajetorias_media(traj, media,
                       "Trajetórias + Média (Ruído Branco)")

plot_variancia(
  variancia = variancia,
  referencia = rep(sigma2, n),
  titulo = "Variância (Ruído Branco)"
)

par(mfrow = c(1,1))

############################################################
# PASSEIO ALEATORIO
############################################################

pa <- simular_passeio_aleatorio(n)

par(mfrow = c(2,1))

plot_serie_temporal(pa, "Random Walk")
plot_acf(diff(pa), "ACF dos Incrementos (ΔX_t)")

par(mfrow = c(1,1))

############################################################
# MÚLTIPLOS PASSEIOS ALEATORIOS
############################################################

traj_pa <- simular_multiplos_passeio_aleatorio(n, m)

media_pa <- calcular_media_trajetoria(traj_pa)
var_pa <- calcular_variancia_trajetoria(traj_pa)

par(mfrow = c(2,1))

plot_trajetorias_media(traj_pa,
                       media_pa,
                       "Passeio Aleatório + Média")

plot_variancia(var_pa,
               referencia = sigma2 * (1:n),
               titulo = "Variância do Passeio Aleatório")

par(mfrow = c(1,1))

############################################################
# CONJUNTO CILÍNDRICO PARA O PASSEIO ALEATÓRIO
############################################################

n <- 200
m <- 1000

############################################################
# TEMPOS FIXADOS
############################################################

tempos_fix <- c(20, 60, 100, 140)

############################################################
# JANELAS NO EIXO Y
############################################################

limite_inferior <- c(-5,  2, -9,  4)

limite_superior <- c( 3, 10, -1, 12)

############################################################
# MATRIZ DAS TRAJETÓRIAS
############################################################

trajetorias <- simular_multiplos_passeio_aleatorio(n, m)

############################################################
# VERIFICAÇÃO DO CONJUNTO CILÍNDRICO
############################################################

pertence_cilindro <- logical(m)

for(i in 1:m){
  
  pertence <- TRUE
  
  for(j in 1:length(tempos_fix)){
    
    t <- tempos_fix[j]
    
    valor <- trajetorias[t,i]
    
    limite_inf <- limite_inferior[j]
    limite_sup <- limite_superior[j]
    
    if(valor < limite_inf || valor > limite_sup){
      pertence <- FALSE
    }
  }
  
  pertence_cilindro[i] <- pertence
}

############################################################
# CONTAGEM
############################################################

numero_trajetorias_cilindro <- 0

for(i in 1:m){
  
  if(pertence_cilindro[i]){
    numero_trajetorias_cilindro <-
      numero_trajetorias_cilindro + 1
  }
}

############################################################
# PROPORÇÃO
############################################################

proporcao <- numero_trajetorias_cilindro / m

cat("Número total de trajetórias:", m, "\n")

cat("Número de trajetórias no cilindro:",
    numero_trajetorias_cilindro, "\n")

cat("Proporção de trajetórias no cilindro:",
    proporcao, "\n")

############################################################
# GRÁFICO
############################################################

dev.new(width = 14, height = 8)

plot(1:n, trajetorias[,1],
     type = "n",
     xlim = c(1,n),
     ylim = range(trajetorias),
     xlab = "Tempo (t)",
     ylab = "X_t",
     main = "Passeio Aleatório e Conjunto Cilíndrico")

############################################################
# TRAJETÓRIAS FORA DO CILINDRO
############################################################

for(i in 1:m){
  
  if(!pertence_cilindro[i]){
    
    lines(1:n,
          trajetorias[,i],
          col = rgb(0.5,0.5,0.5,0.15),
          lwd = 1)
  }
}

############################################################
# TRAJETÓRIAS DENTRO DO CILINDRO
############################################################

for(i in 1:m){
  
  if(pertence_cilindro[i]){
    
    lines(1:n,
          trajetorias[,i],
          col = rgb(1,0,0,0.7),
          lwd = 2)
  }
}

############################################################
# JANELAS DO CILINDRO
############################################################

for(j in 1:length(tempos_fix)){
  
  t <- tempos_fix[j]
  
  limite_inf <- limite_inferior[j]
  limite_sup <- limite_superior[j]
  
  lines(c(t,t),
        c(limite_inf, limite_sup),
        col = "blue",
        lwd = 6)
  
  points(t,
         limite_inf,
         pch = 19,
         col = "blue")
  
  points(t,
         limite_sup,
         pch = 19,
         col = "blue")
}

############################################################
# LEGENDA
############################################################

legend("topleft",
       legend = c("Fora do cilindro",
                  "Dentro do cilindro",
                  "Janelas"),
       col = c("gray","red","blue"),
       lwd = c(2,2,6),
       bty = "n")
