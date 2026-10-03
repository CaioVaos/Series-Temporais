############################################################
# main.R – COORDENADOR DO PIPELINE
#
# Configuração -> Simulação -> Cálculo Estatístico
#              -> Análise e Comparação -> Exibição de Resultados
#
# Como executar (com a pasta do projeto como diretório de trabalho):
#   Rscript main.R
############################################################

rm(list = ls())

source("lista1408/config.R", encoding = "UTF-8")
source("lista1408/dados.R", encoding = "UTF-8")
source("lista1408/processamento.R", encoding = "UTF-8")
source("lista1408/visualizacao.R", encoding = "UTF-8")

dir.create("lista1408/graficos", showWarnings = FALSE)

############################################################
# ETAPA 1 – SIMULAÇÃO DE SÉRIES
############################################################

eps <- simular_ruido_branco(n, sigma2)

X <- simular_passeio_aleatorio(n, sigma2)

X_ar1 <- simular_ar1(n, phi, sigma2)

############################################################
# ETAPA 2 – CÁLCULO ESTATÍSTICO
############################################################

media_eps <- calcular_media_empirica(eps)
media_X <- calcular_media_empirica(X)
media_ar1 <- calcular_media_empirica(X_ar1)

fac_eps <- calcular_fac_empirica(eps, max_lag)
fac_X <- calcular_fac_empirica(X, max_lag)
fac_ar1 <- calcular_fac_empirica(X_ar1, max_lag)

fac_ar1_teorica <- calcular_fac_teorica_ar1(phi, max_lag)

############################################################
# ETAPA 3 – ANÁLISE E COMPARAÇÃO
# diferença entre FAC empírica e teórica do AR(1), lag a lag
# e comparação com a função acf() do R
############################################################

diferenca_ar1 <- numeric(max_lag + 1)

for(h in 0:max_lag){
  diferenca_ar1[h+1] <- abs(fac_ar1[h+1] - fac_ar1_teorica[h+1])
}

fac_acf_R <- acf(X_ar1, lag.max = max_lag, plot = FALSE)$acf

maior_diferenca_acf <- 0

for(h in 0:max_lag){
  d <- abs(fac_ar1[h+1] - fac_acf_R[h+1])
  if(d > maior_diferenca_acf){
    maior_diferenca_acf <- d
  }
}

############################################################
# ETAPA 4 – EXIBIÇÃO DE RESULTADOS (terminal)
############################################################

cat("========================================\n")
cat("PARÂMETROS\n")
cat("========================================\n")
cat("n =", n, "| max_lag =", max_lag,
    "| sigma2 =", sigma2, "| phi =", phi, "\n\n")

cat("========================================\n")
cat("MÉDIAS EMPÍRICAS\n")
cat("========================================\n")
cat("Ruído branco:", media_eps, "\n")
cat("Passeio aleatório:", media_X, "\n")
cat("AR(1):", media_ar1, "\n\n")

cat("========================================\n")
cat("FAC DO AR(1): EMPÍRICA x TEÓRICA\n")
cat("========================================\n")
for(h in 0:max_lag){
  cat("lag", h,
      "| empírica:", round(fac_ar1[h+1], 4),
      "| teórica:", round(fac_ar1_teorica[h+1], 4),
      "| |dif|:", round(diferenca_ar1[h+1], 4), "\n")
}

cat("\nMaior diferença entre FAC manual e acf() do R:",
    maior_diferenca_acf, "\n")

############################################################
# ETAPA 5 – EXIBIÇÃO DE RESULTADOS (gráficos)
############################################################

g1 <- plot_serie_temporal(eps, "Ruído Branco", "eps_t", "gray30")
g2 <- plot_fac_empirica(fac_eps, "FAC Empírica – Ruído Branco", "gray70")

g3 <- plot_serie_temporal(X, "Passeio Aleatório", "X_t", "blue")
g4 <- plot_fac_empirica(fac_X, "FAC Empírica – Passeio Aleatório", "lightblue")

g5 <- plot_serie_temporal(X_ar1, paste("Processo AR(1) (phi =", phi, ")"),
                          "X_t", "forestgreen")
g6 <- plot_fac_comparacao(fac_ar1, fac_ar1_teorica,
                          "FAC Empírica vs Teórica – AR(1)")

ggsave("lista1408/graficos/01_serie_ruido_branco.png", g1, width = 8, height = 4, dpi = 150)
ggsave("lista1408/graficos/02_fac_ruido_branco.png", g2, width = 8, height = 4, dpi = 150)
ggsave("lista1408/graficos/03_serie_passeio.png", g3, width = 8, height = 4, dpi = 150)
ggsave("lista1408/graficos/04_fac_passeio.png", g4, width = 8, height = 4, dpi = 150)
ggsave("lista1408/graficos/05_serie_ar1.png", g5, width = 8, height = 4, dpi = 150)
ggsave("lista1408/graficos/06_fac_ar1_comparacao.png", g6, width = 8, height = 4, dpi = 150)

