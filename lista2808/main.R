############################################################
# main.R – COORDENADOR DO PIPELINE
#
# Configuração -> Simulação/Matrizes -> Estimação OLS
#              -> Diagnóstico -> Previsão -> Visualização
#
# Como executar (com a pasta do projeto como diretório de trabalho):
#   Rscript main.R
############################################################

rm(list = ls())

source("lista2808/config.R", encoding = "UTF-8")
source("lista2808/dados.R", encoding = "UTF-8")
source("lista2808/processamento.R", encoding = "UTF-8")
source("lista2808/visualizacao.R", encoding = "UTF-8")

dir.create("lista2808/graficos", showWarnings = FALSE)

############################################################
# ETAPA 1 – SIMULAÇÃO / MATRIZES
############################################################

X_tendencia <- criar_matriz_tendencia(t, grau_tendencia)

X_sazonal <- criar_matriz_fourier(t, s, K)

Xt <- simular_serie(beta_tendencia, X_tendencia, X_sazonal,
                    beta_sazonal, sigma)

# componentes verdadeiros (para comparar com os estimados)
tendencia_real <- multiplicar_matriz_vetor(X_tendencia, beta_tendencia)

sazonalidade_real <- multiplicar_matriz_vetor(X_sazonal, beta_sazonal)

############################################################
# ETAPA 2 – ESTIMAÇÃO OLS
############################################################

modelo <- ajustar_modelo(Xt, X_tendencia, X_sazonal)

# média móvel (para comparar com a tendência estimada)
tendencia_media_movel <- calcular_media_movel(Xt, q)

# modelo alternativo com dummies (comparação Fourier x dummies)
X_dummies <- criar_matriz_dummies(t, s)

modelo_dummies <- ajustar_modelo(Xt, X_tendencia, X_dummies)

############################################################
# ETAPA 3 – DIAGNÓSTICO
############################################################

diag_fourier <- diagnostico(Xt, modelo$residuos)

diag_dummies <- diagnostico(Xt, modelo_dummies$residuos)

# validação: compara os coeficientes manuais com a função lm() do R
coef_lm <- as.numeric(coef(lm(Xt ~ modelo$X_full - 1)))

maior_diferenca_lm <- 0

for(i in 1:length(coef_lm)){
  d <- abs(modelo$beta_hat[i] - coef_lm[i])
  if(d > maior_diferenca_lm){
    maior_diferenca_lm <- d
  }
}

############################################################
# ETAPA 4 – PREVISÃO
############################################################

X_tendencia_fut <- criar_matriz_tendencia(t_futuro, grau_tendencia)

X_sazonal_fut <- criar_matriz_fourier(t_futuro, s, K)

previsoes <- prever(modelo, X_tendencia_fut, X_sazonal_fut)

# validação: valor verdadeiro sem ruído (tendência + sazonalidade reais)
tendencia_real_fut <- multiplicar_matriz_vetor(X_tendencia_fut, beta_tendencia)

sazonalidade_real_fut <- multiplicar_matriz_vetor(X_sazonal_fut, beta_sazonal)

soma_erro2 <- 0

for(i in 1:n_prev){
  valor_real <- tendencia_real_fut[i] + sazonalidade_real_fut[i]
  erro <- previsoes[i] - valor_real
  soma_erro2 <- soma_erro2 + erro^2
}

rmse_previsao <- sqrt(soma_erro2 / n_prev)

############################################################
# ETAPA 5 – VISUALIZAÇÃO E SAÍDA (terminal)
############################################################

cat("========================================\n")
cat("PARÂMETROS\n")
cat("========================================\n")
cat("n =", n, "| s =", s, "| sigma =", sigma, "| K =", K,
    "| grau =", grau_tendencia, "| q =", q, "\n\n")

cat("========================================\n")
cat("COEFICIENTES (verdadeiro x estimado)\n")
cat("========================================\n")
beta_verdadeiro <- c(beta_tendencia, beta_sazonal)
for(i in 1:length(beta_verdadeiro)){
  cat("beta", i, "| verdadeiro:", beta_verdadeiro[i],
      "| estimado:", round(modelo$beta_hat[i], 4), "\n")
}

cat("\n========================================\n")
cat("DIAGNÓSTICO DOS RESÍDUOS\n")
cat("========================================\n")
cat("               Fourier      Dummies\n")
cat("Média       ", round(diag_fourier$media, 4), "  ", round(diag_dummies$media, 4), "\n")
cat("Variância   ", round(diag_fourier$variancia, 4), "  ", round(diag_dummies$variancia, 4), "\n")
cat("RMSE        ", round(diag_fourier$rmse, 4), "  ", round(diag_dummies$rmse, 4), "\n")
cat("R²          ", round(diag_fourier$r2, 4), "  ", round(diag_dummies$r2, 4), "\n")

cat("\nMaior diferença entre beta manual e lm():", maior_diferenca_lm, "\n")

cat("\n========================================\n")
cat("PREVISÃO (próximos", n_prev, "períodos)\n")
cat("========================================\n")
for(i in 1:n_prev){
  cat("t =", t_futuro[i], "| previsto:", round(previsoes[i], 4),
      "| valor sem ruído:",
      round(tendencia_real_fut[i] + sazonalidade_real_fut[i], 4), "\n")
}
cat("RMSE da previsão (contra o valor sem ruído):", rmse_previsao, "\n")

############################################################
# ETAPA 5 – VISUALIZAÇÃO E SAÍDA (gráficos)
############################################################

g1 <- plot_serie_ajustada(t, Xt, modelo$y_hat)

g2 <- plot_tendencia(t, modelo$tendencia, tendencia_real)

g3 <- plot_sazonalidade(t, modelo$sazonalidade, sazonalidade_real)

g4 <- plot_residuos(t, modelo$residuos)

g5 <- plot_media_movel_tendencia(t, Xt, tendencia_media_movel,
                                 modelo$tendencia, q)

# gráfico integrado: decomposição aditiva em 4 painéis
g_decomposicao <- arrangeGrob(g1, g2, g3, g4, ncol = 1)

ggsave("lista2808/graficos/01_decomposicao.png", g_decomposicao, width = 8, height = 12, dpi = 200)
ggsave("lista2808/graficos/02_media_movel_tendencia.png", g5, width = 8, height = 4, dpi = 200)
