############################################################
# main.R – COORDENADOR DO PIPELINE
#
# Configuração -> Simulação da Série -> Transformação (Média Móvel)
#              -> Análise Estatística (FAC) -> Visualização e Saída
#
# Como executar (com a pasta do projeto como diretório de trabalho):
#   Rscript main.R
############################################################

rm(list = ls())

source("lista2108/config.R", encoding = "UTF-8")
source("lista2108/dados.R", encoding = "UTF-8")
source("lista2108/processamento.R", encoding = "UTF-8")
source("lista2108/visualizacao.R", encoding = "UTF-8")

dir.create("lista2108/graficos", showWarnings = FALSE)

############################################################
# ETAPA 1 – SIMULAÇÃO DA SÉRIE
############################################################

Xt <- simular_serie_com_tendencia(beta, X, sigma)

############################################################
# ETAPA 2 – TRANSFORMAÇÃO (MÉDIA MÓVEL)
############################################################

m_q <- calcular_media_movel(Xt, q)

############################################################
# ETAPA 3 – ANÁLISE ESTATÍSTICA
############################################################

media_Xt <- calcular_media_amostral(Xt)

gamma_1 <- calcular_autocovariancia_empirica(Xt, h = 1)

fac_Xt <- calcular_fac_empirica(Xt, max_lag)

# validação: compara a FAC manual com a acf() do R
fac_acf_R <- acf(Xt, lag.max = max_lag, plot = FALSE)$acf

maior_diferenca <- 0

for(h in 0:max_lag){
  d <- abs(fac_Xt[h+1] - fac_acf_R[h+1])
  if(d > maior_diferenca){
    maior_diferenca <- d
  }
}

############################################################
# ETAPA 4 – VISUALIZAÇÃO E SAÍDA (terminal)
############################################################

cat("========================================\n")
cat("PARÂMETROS\n")
cat("========================================\n")
cat("n =", n, "| sigma =", sigma, "| q =", q,
    "| max_lag =", max_lag, "\n")
cat("beta =", beta, "\n\n")

cat("========================================\n")
cat("RESULTADOS\n")
cat("========================================\n")
cat("Média amostral de Xt:", media_Xt, "\n")
cat("Autocovariância empírica de lag 1:", gamma_1, "\n")
cat("Número de médias móveis:", length(m_q), "(n - 2q)\n\n")

cat("FAC empírica (lags 0 a", max_lag, "):\n")
print(round(fac_Xt, 4))

cat("\nMaior diferença entre FAC manual e acf() do R:",
    maior_diferenca, "\n")

############################################################
# ETAPA 4 – VISUALIZAÇÃO E SAÍDA (gráficos)
############################################################

g1 <- plot_serie_media_movel(t, Xt, m_q, q)

g2 <- plot_fac_empirica(fac_Xt, "Função de Autocorrelação Empírica (FAE)",
                        "skyblue")

# gráfico integrado: série + média móvel em cima, FAC embaixo
g_integrado <- arrangeGrob(g1, g2, ncol = 1)

ggsave("lista2108/graficos/01_serie_media_movel.png", g1, width = 8, height = 4, dpi = 150)
ggsave("lista2108/graficos/02_fac_empirica.png", g2, width = 8, height = 4, dpi = 150)
ggsave("lista2108/graficos/03_grafico_integrado.png", g_integrado, width = 8, height = 8, dpi = 150)
