# AIRLINE SARIMA(0,1,1)(0,1,1)₁₂

ordem_airline <- c(0, 1, 1)
sazonal_airline <- list(order = c(0, 1, 1), period = 12)

# AJUSTE NO TREINO, VALIDAÇÃO E TESTE

ajuste_treino <- arima(ln_passageiros[1:n_treino], order = ordem_airline,
                      seasonal = sazonal_airline)
ajuste_airline <- arima(ln_passageiros[1:n_treino_validacao], order = ordem_airline,
                       seasonal = sazonal_airline)

ajuste_treino
ajuste_airline

# Os primeiros 13 meses ficam fora das métricas do treino
meses_ajuste <- 14:n_treino
observado <- treino$PassageirosMilhoes[meses_ajuste]
media_ln <- numeric(length(meses_ajuste))
variancia_ln <- numeric(length(meses_ajuste))

# Primeira previsão após as diferenciações
theta <- unname(coef(ajuste_treino)["ma1"])
theta_sazonal <- unname(coef(ajuste_treino)["sma1"])
media_ln[1] <- ln_treino[13] + ln_treino[2] - ln_treino[1]
variancia_ln[1] <- ajuste_treino$sigma2 * (1 + theta^2) * (1 + theta_sazonal^2)

for(j in 2:length(meses_ajuste)){
  t <- meses_ajuste[j]
  atualizado <- arima(ln_treino[1:(t-1)], order = ordem_airline,
                      seasonal = sazonal_airline, fixed = coef(ajuste_treino), method = "ML")
  atualizado$sigma2 <- ajuste_treino$sigma2
  um_passo <- predict(atualizado)
  media_ln[j] <- as.numeric(um_passo$pred)
  variancia_ln[j] <- as.numeric(um_passo$se)^2
}

previsto <- exp(media_ln + variancia_ln/2)
medidas <- diagnostico(observado, observado - previsto)

metricas_passageiros <- list(data.frame(
  Grupo = "Treino", Previsao = "Ajuste", Meses = length(meses_ajuste),
  RMSE = medidas["RMSE"], MAE = medidas["MAE"], R2 = medidas["R2"],
  Cobertura95 = NA, row.names = NULL
))
previsoes_passageiros <- list()

for(i in 1:2){
  if(i == 1){
    ajuste <- ajuste_treino
    meses <- (n_treino+1):n_treino_validacao
    grupo <- "Validação"
  } else {
    ajuste <- ajuste_airline
    meses <- (n_treino_validacao+1):n
    grupo <- "Teste"
  }

  observado <- passageiros_antes$PassageirosMilhoes[meses]
  datas <- passageiros_antes$Data[meses]
  horizonte <- length(meses)
  media_ln <- numeric(horizonte)
  desvio <- numeric(horizonte)

  # Um passo por vez com coeficientes e variância estimados no treino
  for(j in 1:horizonte){
    t <- meses[j]
    atualizado <- arima(ln_passageiros[1:(t-1)], order = ordem_airline,
                        seasonal = sazonal_airline, fixed = coef(ajuste), method = "ML")
    atualizado$sigma2 <- ajuste$sigma2
    previsao <- predict(atualizado)
    media_ln[j] <- as.numeric(previsao$pred)
    desvio[j] <- as.numeric(previsao$se)
  }

  um_passo <- data.frame(
    Data = datas, Grupo = grupo, Previsao = "Um passo", Observado = observado,
    Previsto = exp(media_ln + desvio^2/2),
    Inferior = exp(media_ln - qnorm(0.975)*desvio),
    Superior = exp(media_ln + qnorm(0.975)*desvio)
  )

  previsao <- predict(ajuste, n.ahead = horizonte)
  media_ln <- as.numeric(previsao$pred)
  desvio <- as.numeric(previsao$se)

  previsao_completa <- data.frame(
    Data = datas, Grupo = grupo, Previsao = "Horizonte completo", Observado = observado,
    Previsto = exp(media_ln + desvio^2/2),
    Inferior = exp(media_ln - qnorm(0.975)*desvio),
    Superior = exp(media_ln + qnorm(0.975)*desvio)
  )

  previsoes_passageiros[[i]] <- rbind(um_passo, previsao_completa)
  for(base in list(um_passo, previsao_completa)){
    medidas <- diagnostico(base$Observado, base$Observado - base$Previsto)
    metricas_passageiros[[length(metricas_passageiros)+1]] <- data.frame(
      Grupo = grupo, Previsao = base$Previsao[1], Meses = nrow(base),
      RMSE = medidas["RMSE"], MAE = medidas["MAE"], R2 = medidas["R2"],
      Cobertura95 = mean(base$Observado >= base$Inferior & base$Observado <= base$Superior),
      row.names = NULL
    )
  }
}

previsoes_passageiros <- do.call(rbind, previsoes_passageiros)
metricas_passageiros <- do.call(rbind, metricas_passageiros)

metricas_passageiros

# FAIXA HISTÓRICA E PREVISÃO NO TESTE

modelo_airline <- "Airline SARIMA(0,1,1)(0,1,1)₁₂"

faixa_passageiros <- data.frame(Data = dados_passageiros$Data,
                                Observado = dados_passageiros$PassageirosMilhoes,
                                Previsto = NA, Inferior = NA, Superior = NA)

# Os meses da pandemia continuam no gráfico e ficam fora da entrada do ajuste
pandemia_passageiros <- faixa_passageiros$Data >= as.Date("2020-03-01") &
                       faixa_passageiros$Data <= as.Date("2021-05-01")
entrada_airline <- log(faixa_passageiros$Observado)
entrada_airline[pandemia_passageiros] <- NA

for(t in which(faixa_passageiros$Data >= as.Date("2005-01-01") & !pandemia_passageiros)){
  ajuste <- arima(entrada_airline[1:(t-1)], order = ordem_airline,
                  seasonal = sazonal_airline)
  previsao <- predict(ajuste)
  media_ln <- as.numeric(previsao$pred)
  desvio <- as.numeric(previsao$se)

  faixa_passageiros$Previsto[t] <- exp(media_ln + desvio^2/2)
  faixa_passageiros$Inferior[t] <- exp(media_ln - qnorm(0.975)*desvio)
  faixa_passageiros$Superior[t] <- exp(media_ln + qnorm(0.975)*desvio)
}

faixa_passageiros$Fora <- faixa_passageiros$Observado < faixa_passageiros$Inferior |
                         faixa_passageiros$Observado > faixa_passageiros$Superior
faixa_passageiros$Trecho <- cumsum(c(TRUE, diff(is.finite(faixa_passageiros$Previsto)) != 0))

faixa_airline <- faixa_passageiros[!is.na(faixa_passageiros$Previsto), ]

grafico_airline_historico <- ggplot() +
  annotate("rect", xmin = as.Date("2002-01-01"), xmax = as.Date("2004-12-31"),
           ymin = -Inf, ymax = Inf, fill = cor_trajetorias, alpha = 0.18) +
  annotate("text", x = as.Date("2003-07-01"), y = 3.8,
           label = "Treino inicial\n(2002 a 2004)", size = 3.8) +
  geom_ribbon(data = faixa_airline,
              aes(x = Data, ymin = Inferior, ymax = Superior,
                  group = Trecho, fill = "Intervalo de previsão (95%)"),
              alpha = 0.25) +
  geom_line(data = faixa_passageiros,
            aes(x = Data, y = Observado, color = "Observado"),
            linewidth = 0.65) +
  geom_line(data = faixa_airline,
            aes(x = Data, y = Previsto, group = Trecho, color = "Previsão"),
            linewidth = 0.86) +
  scale_color_manual(values = c("Observado" = cor_principal, "Previsão" = cor_referencia),
                     breaks = c("Observado", "Previsão")) +
  scale_fill_manual(values = c("Intervalo de previsão (95%)" = cor_acf)) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(faixa_passageiros$Data), max(faixa_passageiros$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(
    title = paste("Nº de passageiros —", modelo_airline),
    subtitle = "Previsões mensais com reestimação (01/01/2005 a 31/12/2025)",
    x = "Ano", y = "Nº de passageiros (milhões)"
  ) +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[38] Nº de passageiros - Previsões mensais do Airline.png"),
       plot = grafico_airline_historico, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

previsao_teste_airline <- previsoes_passageiros[previsoes_passageiros$Grupo == "Teste" &
                                               previsoes_passageiros$Previsao == "Horizonte completo", ]

grafico_airline_teste <- ggplot(previsao_teste_airline, aes(x = Data)) +
  geom_ribbon(aes(ymin = Inferior, ymax = Superior, fill = "Intervalo de previsão (95%)"),
              alpha = 0.25) +
  geom_line(aes(y = Observado, color = "Observado"), linewidth = 0.65) +
  geom_line(aes(y = Previsto, color = "Previsão"), linewidth = 0.86) +
  scale_color_manual(values = c("Observado" = cor_principal, "Previsão" = cor_referencia),
                     breaks = c("Observado", "Previsão")) +
  scale_fill_manual(values = c("Intervalo de previsão (95%)" = cor_acf)) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  scale_x_date(date_labels = "%m/%Y",
               breaks = seq(min(previsao_teste_airline$Data), max(previsao_teste_airline$Data),
                            by = "6 months"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(
    title = paste("Nº de passageiros —", modelo_airline),
    subtitle = "Teste, horizonte completo (01/06/2016 a 29/02/2020)",
    x = "Mês", y = "Nº de passageiros (milhões)"
  ) +
  tema_graficos

ggsave(file.path("Resultados", "[39] Nº de passageiros - Teste do Airline.png"),
       plot = grafico_airline_teste, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# RESÍDUOS DO AIRLINE

# Os 13 primeiros meses inicializam as diferenças do Airline
residuos_airline <- data.frame(
  Data = treino$Data[-(1:13)],
  Residuo = as.numeric(residuals(ajuste_treino))[-(1:13)]
)
residuos_airline$Padronizado <- residuos_airline$Residuo / sqrt(ajuste_treino$sigma2)

fac_residuos_airline <- data.frame(
  Defasagem = 0:36,
  Autocorrelacao = calcular_fac_empirica(residuos_airline$Residuo, 36),
  Limite = 1.96 / sqrt(nrow(residuos_airline))
)

ljung_airline <- Box.test(residuos_airline$Residuo, lag = 24, type = "Ljung-Box", fitdf = 2)
ljung_airline

grafico_residuos_airline <- ggplot(residuos_airline, aes(x = Data, y = Residuo)) +
  geom_hline(yintercept = 0, color = cor_trajetorias, linewidth = 0.4) +
  geom_line(color = cor_principal, linewidth = 0.65) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(residuos_airline$Data), max(residuos_airline$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = paste("Resíduos do", modelo_airline),
       subtitle = "Treino (01/02/2003 a 31/10/2012)",
       x = "Ano", y = "Resíduo de ln(Nº mensal de passageiros em milhões)") +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[41] Resíduos do Airline.png"),
       plot = grafico_residuos_airline, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_fac_residuos_airline <- ggplot(fac_residuos_airline[fac_residuos_airline$Defasagem > 0, ], aes(x = Defasagem)) +
  geom_hline(yintercept = 0, color = cor_principal, linewidth = 0.42) +
  geom_hline(yintercept = c(-fac_residuos_airline$Limite[1], fac_residuos_airline$Limite[1]),
             color = cor_acf, linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = Defasagem, y = 0, yend = Autocorrelacao),
               color = cor_principal, linewidth = 0.52) +
  scale_x_continuous(breaks = seq(0, max(fac_residuos_airline$Defasagem), by = 6)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = paste("Função de Autocorrelação dos resíduos do", modelo_airline),
       subtitle = "Treino (01/02/2003 a 31/10/2012)",
       x = "Defasagem (meses)", y = "Autocorrelação") +
  tema_graficos

ggsave(file.path("Resultados", "[42] FAC dos resíduos do Airline.png"),
       plot = grafico_fac_residuos_airline, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_qq_residuos_airline <- ggplot(residuos_airline, aes(sample = Padronizado)) +
  stat_qq_line(color = cor_referencia, linewidth = 0.52) +
  stat_qq(color = cor_principal, size = 1.3) +
  scale_x_continuous(labels = label_decimal) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = paste("QQ-plot dos resíduos do", modelo_airline),
       subtitle = "Treino (01/02/2003 a 31/10/2012)",
       x = "Quantis teóricos", y = "Quantis dos resíduos padronizados") +
  tema_graficos

ggsave(file.path("Resultados", "[43] QQ-plot dos resíduos do Airline.png"),
       plot = grafico_qq_residuos_airline, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")
