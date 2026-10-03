############################################################
# visualizacao.R
# RESPONSABILIDADE: gráficos com ggplot2 (sem cálculos de estimação).
# DEPENDÊNCIAS: pacotes ggplot2 e gridExtra
#   install.packages(c("ggplot2", "gridExtra"))
#
# INTERFACE (todas retornam um objeto ggplot; t = vetor de tempo
# de tamanho n; os demais vetores também têm tamanho n,
# exceto media_movel, que tem n - 2q):
#
# plot_serie_ajustada(t, serie, ajustada)
# plot_tendencia(t, tendencia_estimada, tendencia_real)
# plot_sazonalidade(t, sazonalidade_estimada, sazonalidade_real)
# plot_residuos(t, residuos)
# plot_media_movel_tendencia(t, serie, media_movel, tendencia, q)
#
# Pré-condição: os vetores já vêm calculados (processamento.R).
############################################################

library(ggplot2)
library(gridExtra)

plot_serie_ajustada <- function(t, serie, ajustada){
  
  df <- data.frame(t = t, observada = serie, ajustada = ajustada)
  
  grafico <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = observada, color = "Série observada"), linewidth = 0.8) +
    geom_line(aes(y = ajustada, color = "Série ajustada"),
              linewidth = 0.8, linetype = "dashed") +
    scale_color_manual(values = c("Série observada" = "blue",
                                  "Série ajustada" = "red")) +
    labs(title = "Série Observada vs Ajustada",
         x = "Tempo", y = "X_t", color = "") +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(grafico)
}

plot_tendencia <- function(t, tendencia_estimada, tendencia_real){
  
  df <- data.frame(t = t, estimada = tendencia_estimada, real = tendencia_real)
  
  grafico <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = estimada, color = "Tendência estimada"), linewidth = 0.9) +
    geom_line(aes(y = real, color = "Tendência real"), linewidth = 0.9) +
    scale_color_manual(values = c("Tendência estimada" = "red",
                                  "Tendência real" = "blue")) +
    labs(title = "Tendência", x = "Tempo", y = "Tendência", color = "") +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(grafico)
}

plot_sazonalidade <- function(t, sazonalidade_estimada, sazonalidade_real){
  
  df <- data.frame(t = t, estimada = sazonalidade_estimada,
                   real = sazonalidade_real)
  
  grafico <- ggplot(df, aes(x = t)) +
    geom_line(aes(y = estimada, color = "Sazonalidade estimada"), linewidth = 0.9) +
    geom_line(aes(y = real, color = "Sazonalidade real"), linewidth = 0.9) +
    scale_color_manual(values = c("Sazonalidade estimada" = "darkgreen",
                                  "Sazonalidade real" = "blue")) +
    labs(title = "Sazonalidade", x = "Tempo", y = "Sazonalidade", color = "") +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(grafico)
}

plot_residuos <- function(t, residuos){
  
  df <- data.frame(t = t, residuo = residuos)
  
  grafico <- ggplot(df, aes(x = t, y = residuo)) +
    geom_line(color = "black", linewidth = 0.7) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Resíduos", x = "Tempo", y = "Resíduo") +
    theme_minimal()
  
  return(grafico)
}

plot_media_movel_tendencia <- function(t, serie, media_movel, tendencia, q){
  
  n <- length(serie)
  
  nome_media <- paste0("Média móvel q=", q)
  
  cores <- c("blue", "red", "darkgreen")
  names(cores) <- c("Série Xt", nome_media, "Tendência estimada por OLS")
  
  df_serie <- data.frame(t = t, x = serie)
  df_media <- data.frame(t = (q+1):(n-q), x = media_movel)
  df_tend <- data.frame(t = t, x = tendencia)
  
  grafico <- ggplot() +
    geom_line(data = df_serie, aes(x = t, y = x, color = "Série Xt"),
              linewidth = 0.7) +
    geom_line(data = df_media, aes(x = t, y = x, color = nome_media),
              linewidth = 1.1) +
    geom_line(data = df_tend,
              aes(x = t, y = x, color = "Tendência estimada por OLS"),
              linewidth = 1.1) +
    scale_color_manual(values = cores) +
    labs(title = "Série Temporal, Média Móvel e Tendência",
         x = "Tempo", y = "X_t", color = "") +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(grafico)
}
