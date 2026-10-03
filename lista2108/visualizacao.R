############################################################
# visualizacao.R
# RESPONSABILIDADE: gráficos com ggplot2 (sem cálculos estatísticos).
# DEPENDÊNCIAS: pacotes ggplot2 e gridExtra
#   install.packages(c("ggplot2", "gridExtra"))
#
# INTERFACE (retornam objetos ggplot):
#
# plot_serie_media_movel(tempo, serie, media_movel, q)
#   Entrada : tempo (vetor 1:n), serie (vetor de tamanho n),
#             media_movel (vetor de tamanho n - 2q), q (inteiro)
#
# plot_fac_empirica(fac, titulo, cor)
#   Entrada : fac (vetor numérico, posição h+1 = lag h),
#             titulo (texto), cor (nome de cor)
#
# Pré-condição: os vetores já vêm calculados (processamento.R).
############################################################

library(ggplot2)
library(gridExtra)

plot_serie_media_movel <- function(tempo, serie, media_movel, q){
  
  n <- length(serie)
  
  df_serie <- data.frame(t = tempo, x = serie)
  
  df_media <- data.frame(t = (q+1):(n-q), x = media_movel)
  
  nome_media <- paste0("Média móvel q=", q)
  
  # cada cor está ligada ao nome da linha (evita trocar as cores)
  cores <- c("blue", "red")
  names(cores) <- c("Série Xt", nome_media)
  
  grafico <- ggplot() +
    geom_line(data = df_serie, aes(x = t, y = x, color = "Série Xt"),
              linewidth = 0.8) +
    geom_line(data = df_media,
              aes(x = t, y = x, color = nome_media),
              linewidth = 1.2) +
    scale_color_manual(values = cores) +
    labs(title = "Série Temporal com Média Móvel",
         x = "Tempo (t)", y = "X_t", color = "") +
    theme_minimal() +
    theme(legend.position = "top")
  
  return(grafico)
}

plot_fac_empirica <- function(fac, titulo, cor = "skyblue"){
  
  max_lag <- length(fac) - 1
  
  df <- data.frame(lag = 0:max_lag, rho = fac)
  
  grafico <- ggplot(df, aes(x = lag, y = rho)) +
    geom_col(fill = cor) +
    geom_hline(yintercept = 0, color = "gray40", linetype = "dashed") +
    labs(title = titulo, x = "Lag (h)", y = "rho_hat") +
    theme_minimal()
  
  return(grafico)
}
