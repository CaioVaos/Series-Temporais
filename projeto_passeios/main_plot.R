############################################################
# MAIN_PLOT.R
# Script principal: carrega as simulações e gera os gráficos com ggplot2.
############################################################

# 1. Setup ----

rm(list = ls())
set.seed(123)

## 1.1 Pacotes ----

library(ggplot2)

## 1.2 Carregando scripts ----

source("projeto_passeios/scripts/sim_ruido_branco.R")
source("projeto_passeios/scripts/sim_passeio_aleatorio.R")
source("projeto_passeios/scripts/sim_cilindro.R")

# 2. Parâmetros Gerais ----

sigma2 <- 1  # variância do ruído branco

# 3. Estatísticas ----

## 3.1 Média ----

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

## 3.2 Variância ----

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


# 4. Plots ----

## 4.1 Série temporal ----
plot_serie_temporal_gg <- function(serie, titulo, ylab = "X_t"){
  
  df <- data.frame(tempo = 1:length(serie), valor = serie)
  
  ggplot(df, aes(x = tempo, y = valor)) +
    geom_line(color = "gray30", linewidth = 0.8) +
    labs(title = titulo, x = "Tempo (t)", y = ylab) +
    theme_minimal()
}

## 4.2 ACF ----
plot_acf_gg <- function(serie, titulo){
  
  acf_obj <- acf(serie, plot = FALSE)
  
  df <- data.frame(lag = as.numeric(acf_obj$lag),
                    acf = as.numeric(acf_obj$acf))
  
  ic <- qnorm(0.975) / sqrt(length(serie))
  
  ggplot(df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0) +
    geom_segment(aes(xend = lag, yend = 0), color = "gray30") +
    geom_hline(yintercept = c(-ic, ic), color = "blue", linetype = "dashed") +
    labs(title = titulo, x = "Lag", y = "ACF") +
    theme_minimal()
}

## 4.3 Múltiplas trajetórias ----
plot_multiplas_trajetorias_gg <- function(trajetorias, titulo, ylab = "X_t"){
  
  n <- nrow(trajetorias)
  m <- ncol(trajetorias)
  
  lista_df <- vector("list", m)
  
  for(i in 1:m){
    lista_df[[i]] <- data.frame(tempo = 1:n, valor = trajetorias[, i], traj = i)
  }
  
  df <- do.call(rbind, lista_df)
  
  ggplot(df, aes(x = tempo, y = valor, group = traj)) +
    geom_line(color = "gray50", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = titulo, x = "Tempo (t)", y = ylab) +
    theme_minimal()
}

## 4.4 Trajetórias + média ----
plot_trajetorias_media_gg <- function(trajetorias, media, titulo){
  
  n <- nrow(trajetorias)
  m <- ncol(trajetorias)
  
  lista_df <- vector("list", m)
  
  for(i in 1:m){
    lista_df[[i]] <- data.frame(tempo = 1:n, valor = trajetorias[, i], traj = i)
  }
  
  df <- do.call(rbind, lista_df)
  df_media <- data.frame(tempo = 1:n, valor = media)
  
  ggplot() +
    geom_line(data = df, aes(x = tempo, y = valor, group = traj),
              color = "gray50", alpha = 0.7) +
    geom_line(data = df_media, aes(x = tempo, y = valor),
              color = "black", linewidth = 1.1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = titulo, x = "Tempo (t)", y = "X_t") +
    theme_minimal()
}

## 4.5 Variância ----
plot_variancia_gg <- function(variancia, referencia = NULL, titulo){
  
  n <- length(variancia)
  df <- data.frame(tempo = 1:n, valor = variancia)
  
  p <- ggplot(df, aes(x = tempo, y = valor)) +
    geom_line(color = "purple", linewidth = 0.9) +
    labs(title = titulo, x = "Tempo (t)", y = "Variância") +
    theme_minimal()
  
  if(!is.null(referencia)){
    df_ref <- data.frame(tempo = 1:n, valor = referencia)
    p <- p + geom_line(data = df_ref, aes(x = tempo, y = valor),
                        color = "red", linetype = "dashed")
  }
  
  return(p)
}

# 5. Exemplos ----

n <- 200
m <- 5

## 5.1 Ruído Branco ----

eps <- simular_ruido_branco(n)

plot_serie_temporal_gg(eps, "Ruído Branco", "eps_t")
plot_acf_gg(eps, "ACF do Ruído Branco")

### Múltiplas Trajetórias ----

traj <- simular_multiplas_trajetorias_ruido(n, m)

plot_multiplas_trajetorias_gg(traj, "Múltiplas Trajetórias - Ruído Branco", "eps_t")

media <- calcular_media_trajetoria(traj)
variancia <- calcular_variancia_trajetoria(traj)

plot_trajetorias_media_gg(traj, media, "Trajetórias + Média (Ruído Branco)")
plot_variancia_gg(variancia, referencia = rep(sigma2, n), titulo = "Variância (Ruído Branco)")

## 5.2 Passeio Aleatório

pa <- simular_passeio_aleatorio(n)

plot_serie_temporal_gg(pa, "Passeio Aleatório")
plot_acf_gg(diff(pa), "ACF dos Incrementos (Delta X_t)")

### Múltiplas Trajetórias ----

traj_pa <- simular_multiplos_passeio_aleatorio(n, m)

media_pa <- calcular_media_trajetoria(traj_pa)
var_pa <- calcular_variancia_trajetoria(traj_pa)

plot_trajetorias_media_gg(traj_pa, media_pa, "Passeio Aleatório + Média")
plot_variancia_gg(var_pa, referencia = sigma2 * (1:n), titulo = "Variância do Passeio Aleatório")

# 6. Cilíndros ----

n <- 200
m <- 1000

trajetorias_cil <- simular_multiplas_trajetorias_ruido(n, m)
trajetorias_cil <- simular_multiplos_passeio_aleatorio(n, m)

# 6.1 Cilindros
tempos          <- c(20, 100)
limite_inferior <- c(-5, -20)
limite_superior <- c( 3, -10)

pertence <- verifica_cilindro_matriz(trajetorias_cil, tempos,
                                        limite_inferior, limite_superior)

pertence_grupo <- pertence

proporcao <- sum(pertence_grupo) / m

cat("Número total de trajetórias:", m, "\n")
cat("Número de trajetórias no grupo cilíndrico:", sum(pertence_grupo), "\n")
cat("Proporção de trajetórias no grupo cilíndrico:", proporcao, "\n")

## 6.2 Plot ----
lista_df_cil <- vector("list", m)

for(i in 1:m){
  lista_df_cil[[i]] <- data.frame(tempo  = 1:n,
                                   valor  = trajetorias_cil[, i],
                                   traj   = i,
                                   dentro = pertence_grupo[i])
}

df_cil <- do.call(rbind, lista_df_cil)

df_janelas <- data.frame(t   = tempos,
                          inf = limite_inferior,
                          sup = limite_superior)

cores <- c(
  "Fora do grupo cilíndrico"   = "gray50",
  "Dentro do grupo cilíndrico" = "red",
  "Janelas"                    = "blue"
)

grafico_cilindros <-
  ggplot() +
  geom_line(data = subset(df_cil, !dentro),
            aes(x = tempo, y = valor, group = traj, color = "Fora do grupo cilíndrico"),
            alpha = 0.15) +
  geom_line(data = subset(df_cil, dentro),
            aes(x = tempo, y = valor, group = traj, color = "Dentro do grupo cilíndrico"),
            alpha = 0.9, linewidth = 1) +
  geom_segment(data = df_janelas,
               aes(x = t, xend = t, y = inf, yend = sup, color = "Janelas"),
               linewidth = 2) +
  scale_color_manual(values = cores, name = "Legenda") +
  labs(title = "Processo Estocástico e Grupo Cilíndrico",
       x = "Tempo (t)", y = "X_t", caption = paste("Prop:", proporcao)) +
  theme_minimal()

grafico_cilindros
