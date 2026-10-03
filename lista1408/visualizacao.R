############################################################
# visualizacao.R
# RESPONSABILIDADE: gráficos com ggplot2 (sem cálculos estatísticos).
# DEPENDÊNCIAS: pacote ggplot2 (instalar com install.packages("ggplot2")).
#
# INTERFACE (todas retornam um objeto ggplot):
#
# plot_serie_temporal(serie, titulo, ylab, cor)
#   Entrada : serie (vetor numérico), titulo (texto),
#             ylab (texto), cor (texto, nome de cor)
#
# plot_fac_empirica(fac, titulo, cor)
#   Entrada : fac (vetor numérico, posição h+1 = lag h)
#
# plot_fac_comparacao(fac_empirica, fac_teorica, titulo)
#   Entrada : dois vetores numéricos de mesmo tamanho
#
# Pré-condição: os vetores já vêm calculados (pelo processamento.R).
############################################################

library(ggplot2)

plot_serie_temporal <- function(serie, titulo, ylab = "X_t", cor = "gray30"){
  
  n <- length(serie)
  
  df <- data.frame(t = 1:n, x = serie)
  
  grafico <- ggplot(df, aes(x = t, y = x)) +
    geom_line(color = cor, linewidth = 0.8) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = titulo, x = "Tempo (t)", y = ylab) +
    theme_minimal()
  
  return(grafico)
}

plot_fac_empirica <- function(fac, titulo, cor = "gray70"){
  
  max_lag <- length(fac) - 1
  
  df <- data.frame(lag = 0:max_lag, rho = fac)
  
  grafico <- ggplot(df, aes(x = lag, y = rho)) +
    geom_col(fill = cor) +
    labs(title = titulo, x = "Lag (h)", y = "rho_hat") +
    theme_minimal()
  
  return(grafico)
}

plot_fac_comparacao <- function(fac_empirica, fac_teorica, titulo){
  
  max_lag <- length(fac_empirica) - 1
  
  df <- data.frame(lag = 0:max_lag,
                   empirica = fac_empirica,
                   teorica = fac_teorica)
  
  grafico <- ggplot(df, aes(x = lag)) +
    geom_col(aes(y = empirica, fill = "FAC Empírica")) +
    geom_line(aes(y = teorica, color = "FAC Teórica"), linewidth = 1) +
    geom_point(aes(y = teorica, color = "FAC Teórica"), size = 2) +
    scale_fill_manual(values = c("FAC Empírica" = "lightgreen")) +
    scale_color_manual(values = c("FAC Teórica" = "darkgreen")) +
    labs(title = titulo, x = "Lag (h)", y = "rho(h)",
         fill = "", color = "") +
    theme_minimal()
  
  return(grafico)
}
