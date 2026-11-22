# RESULTADOS DOS MODELOS

tabela_metricas_busca <- metricas_busca[, c("Grupo", "Inicio", "Fim", "Dias",
                                           "RMSE", "MAE", "R2", "Alarmes", "Cobertura95")]
tabela_metricas_busca$Cobertura95 <- round(100 * tabela_metricas_busca$Cobertura95, 1)
tabela_metricas_busca$Unidade <- "Pontos de interesse de busca"
tabela_metricas_busca[, c("RMSE", "MAE", "R2")] <-
  round(tabela_metricas_busca[, c("RMSE", "MAE", "R2")], 4)
names(tabela_metricas_busca) <- c("Conjunto", "Início", "Fim", "Dias",
                                 "RMSE", "MAE", "R²", "Alarmes", "Cobertura de 95% (%)", "Unidade dos erros")

tabela_metricas_crises <- resultados_crises[, c("Crise", "Grupo", "Dia", "Decisao",
                                               "RMSE", "MAE", "R2", "Elegivel")]
tabela_metricas_crises[, c("RMSE", "MAE", "R2")] <-
  round(tabela_metricas_crises[, c("RMSE", "MAE", "R2")], 4)
tabela_metricas_crises$Elegivel <- ifelse(is.na(resultados_crises$VoltaObservada),
                                        "Sem retorno observado no horizonte",
                                        ifelse(tabela_metricas_crises$Elegivel,
                                               "Antes do retorno observado",
                                               "A partir do início do retorno observado"))
tabela_metricas_crises$Horizonte <- resultados_crises$Horizonte
tabela_metricas_crises$Modelo <- paste(ordens_crises$Modelo[match(resultados_crises$Crise, ordens_crises$Crise)],
                                       "+ AR(1) sem constante")
tabela_metricas_crises$Unidade <- "Pontos de interesse de busca"
names(tabela_metricas_crises) <- c("Crise", "Conjunto", "Dias após a detecção", "Decisão",
                                  "RMSE", "MAE", "R²", "Avaliação", "Horizonte (dias)", "Modelo", "Unidade dos erros")

tabela_metricas_passageiros <- metricas_passageiros[, c("Grupo", "Previsao", "Meses",
                                                       "RMSE", "MAE", "R2", "Cobertura95")]
tabela_metricas_passageiros$Cobertura95 <- round(100 * tabela_metricas_passageiros$Cobertura95, 1)
tabela_metricas_passageiros[, c("RMSE", "MAE", "R2")] <-
  round(tabela_metricas_passageiros[, c("RMSE", "MAE", "R2")], 4)
tabela_metricas_passageiros$Unidade <- "Milhões de passageiros"
names(tabela_metricas_passageiros) <- c("Conjunto", "Previsão", "Meses",
                                       "RMSE", "MAE", "R²", "Cobertura de 95% (%)", "Unidade dos erros")

tabelas <- list(Interesse = tabela_metricas_busca, Crises = tabela_metricas_crises, Passageiros = tabela_metricas_passageiros)
write_xlsx(tabelas, "Resultados/[40] Métricas dos modelos.xlsx")

diagnosticos_modelos <- data.frame(
  Modelo = c(modelo_busca, "AR(1) sem constante", modelo_airline),
  Serie = c("ln(Interesse de busca diário)", "Excesso de interesse", "ln(Nº mensal de passageiros em milhões)"),
  Inicio = c(min(residuos_busca$Data), min(residuos_excesso$Data), min(residuos_airline$Data)),
  Fim = c(max(residuos_busca$Data), max(residuos_excesso$Data), max(residuos_airline$Data)),
  Observacoes = c(nrow(residuos_busca), nrow(residuos_excesso), nrow(residuos_airline)),
  Media = c(calcular_media_amostral(residuos_busca$Residuo),
            calcular_media_amostral(residuos_excesso$Residuo),
            calcular_media_amostral(residuos_airline$Residuo)),
  Variancia = c(calcular_variancia_amostral(residuos_busca$Residuo),
                calcular_variancia_amostral(residuos_excesso$Residuo),
                calcular_variancia_amostral(residuos_airline$Residuo)),
  Defasagens = c(14, 14, 24),
  GrausLiberdade = c(ljung_busca$parameter, ljung_excesso$parameter, ljung_airline$parameter),
  LjungBox = c(ljung_busca$statistic, ljung_excesso$statistic, ljung_airline$statistic),
  ValorP = c(ljung_busca$p.value, ljung_excesso$p.value, ljung_airline$p.value),
  row.names = NULL
)
diagnosticos_modelos

tabela_diagnosticos <- diagnosticos_modelos
tabela_diagnosticos[, c("Media", "Variancia", "LjungBox")] <-
  round(tabela_diagnosticos[, c("Media", "Variancia", "LjungBox")], 4)
tabela_diagnosticos$ValorP <- signif(tabela_diagnosticos$ValorP, 4)
names(tabela_diagnosticos) <- c("Modelo", "Série", "Início", "Fim", "Resíduos", "Média dos resíduos",
                               "Variância dos resíduos", "Defasagens",
                               "Graus de liberdade", "Ljung-Box", "Valor p")

write_xlsx(tabela_diagnosticos, "Resultados/[44] Diagnóstico dos resíduos.xlsx")
