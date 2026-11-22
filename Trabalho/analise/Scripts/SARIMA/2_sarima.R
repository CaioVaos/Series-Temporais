# SARIMA E SISTEMA DE ALERTAS

ordens_alertas <- ordens[ordens$Aplicacao == "Alertas", ]
faixas <- vector("list", nrow(ordens_alertas))
quantil95 <- qnorm(0.975)
quantil99 <- qnorm(0.995)

for(i in 1:nrow(ordens_alertas)){
  ordem <- ordens_alertas[i, ]
  origem <- ordem$Data
  antes <- dados_diarios[dados_diarios$Data >= origem - parametros$janela_normal &
                          dados_diarios$Data < origem, ]
  depois <- dados_diarios[dados_diarios$Data >= origem &
                           dados_diarios$Data < origem + ordem$Periodo, ]

  inicializacao <- ifelse(origem == as.Date("2025-06-19"), "Rossignol2011", "Gardner1980")

  ajuste <- arima(log(antes$InteresseWeb), order = c(ordem$p, ordem$d, ordem$q),
                  seasonal = list(order = c(ordem$P, ordem$D, ordem$Q), period = ordem$Periodo),
                  SSinit = inicializacao, optim.control = list(maxit = 1000))
  previsao <- predict(ajuste, n.ahead = nrow(depois))
  media_ln <- as.numeric(previsao$pred)
  desvio <- as.numeric(previsao$se)

  faixas[[i]] <- data.frame(
    Data = depois$Data, Origem = origem, Observado = depois$InteresseWeb,
    Previsto = exp(media_ln + desvio^2/2),
    Inferior = exp(media_ln - quantil95*desvio),
    Superior = exp(media_ln + quantil95*desvio),
    Superior99 = exp(media_ln + quantil99*desvio),
    Modelo = sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)₇",
                     ordem$p, ordem$d, ordem$q, ordem$P, ordem$D, ordem$Q)
  )
}

faixa_busca <- do.call(rbind, faixas)

# Pausa dos alertas durante a pandemia
pandemia_busca <- faixa_busca$Data >= as.Date("2020-03-01") &
                  faixa_busca$Data <= as.Date("2021-06-30")
faixa_busca <- faixa_busca[!pandemia_busca, ]
faixa_busca$Alarme <- faixa_busca$Observado > faixa_busca$Superior99
faixa_busca$Trecho <- cumsum(c(TRUE, diff(faixa_busca$Data) != 1))

n <- nrow(faixa_busca)
fim_treino <- floor(0.6*n)
fim_validacao <- floor(0.8*n)
faixa_busca$Grupo <- c(rep("Treino", fim_treino),
                      rep("Validação", fim_validacao - fim_treino),
                      rep("Teste", n - fim_validacao))

metricas_busca <- list()
for(grupo in c("Treino", "Validação", "Teste")){
  base <- faixa_busca[faixa_busca$Grupo == grupo, ]
  medidas <- diagnostico(base$Observado, base$Observado - base$Previsto)
  dentro_faixa <- base$Observado >= base$Inferior & base$Observado <= base$Superior

  metricas_busca[[grupo]] <- data.frame(
    Grupo = grupo, Inicio = min(base$Data), Fim = max(base$Data), Dias = nrow(base),
    RMSE = medidas["RMSE"], MAE = medidas["MAE"], R2 = medidas["R2"],
    Cobertura95 = mean(dentro_faixa), Alarmes = sum(base$Alarme),
    row.names = NULL
  )
}

metricas_busca <- do.call(rbind, metricas_busca)
rownames(metricas_busca) <- NULL

metricas_busca

alertas <- faixa_busca[faixa_busca$Alarme, c("Data", "Origem", "Observado", "Superior99", "Modelo")]
alertas

tabela_alertas <- alertas
tabela_alertas[, c("Observado", "Superior99")] <- round(tabela_alertas[, c("Observado", "Superior99")], 3)
names(tabela_alertas) <- c("Data", "Origem da previsão", "Interesse de busca", "Limite de alarme", "Modelo")
write_xlsx(tabela_alertas, "Resultados/[17] Alertas de buscas excessivas.xlsx")

# FAIXA HABITUAL NAS CRISES

ordens_crises <- data.frame(Crise = crises$Crise, Modelo = "")
normal_crises <- vector("list", nrow(crises))

for(i in 1:nrow(crises)){
  noticia <- crises$Noticia[i]
  ordem <- ordens[ordens$Aplicacao == "Crise" & ordens$Data == noticia, ]
  antes <- dados_diarios[dados_diarios$Data >= noticia - parametros$janela_normal &
                          dados_diarios$Data < noticia, ]
  depois <- dados_diarios[dados_diarios$Data >= noticia &
                           dados_diarios$Data < noticia + parametros$horizonte_crise, ]

  antes$LnInteresse <- log(antes$InteresseWeb)
  ordem_modelo <- c(ordem$p, ordem$d, ordem$q)
  sazonal_modelo <- list(order = c(ordem$P, ordem$D, ordem$Q), period = ordem$Periodo)
  ajuste <- arima(antes$LnInteresse, order = ordem_modelo, seasonal = sazonal_modelo)
  previsao <- predict(ajuste, n.ahead = nrow(depois))

  # Um passo nos dias anteriores que entram no AR(1)
  dias_excesso <- (nrow(antes)-parametros$janela_excesso+1):nrow(antes)
  media_ajustada <- numeric(length(dias_excesso))
  variancia_ajustada <- numeric(length(dias_excesso))

  for(j in seq_along(dias_excesso)){
    t <- dias_excesso[j]
    atualizado <- arima(antes$LnInteresse[1:(t-1)], order = ordem_modelo,
                        seasonal = sazonal_modelo, fixed = coef(ajuste), method = "ML")
    atualizado$sigma2 <- ajuste$sigma2
    um_passo <- predict(atualizado)
    media_ajustada[j] <- as.numeric(um_passo$pred)
    variancia_ajustada[j] <- as.numeric(um_passo$se)^2
  }

  media_ln <- c(media_ajustada, as.numeric(previsao$pred))
  variancia_ln <- c(variancia_ajustada, as.numeric(previsao$se)^2)
  margem <- quantil95 * sqrt(variancia_ln)
  observado <- c(antes$InteresseWeb[dias_excesso], depois$InteresseWeb)

  ordens_crises$Modelo[i] <- sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)₇",
                                   ordem$p, ordem$d, ordem$q, ordem$P, ordem$D, ordem$Q)
  normal_crises[[i]] <- data.frame(
    Data = c(antes$Data[dias_excesso], depois$Data), Crise = crises$Crise[i], Noticia = noticia,
    Grupo = crises$Grupo[i], Modelo = ordens_crises$Modelo[i], Observado = observado,
    Previsto = exp(media_ln + variancia_ln/2),
    Inferior = exp(media_ln - margem), Superior = exp(media_ln + margem),
    MediaLn = media_ln, VarianciaLn = variancia_ln, Excesso = log(observado) - media_ln
  )

  if(crises$Crise[i] == "Voo 1282"){
    ajuste_busca <- ajuste
    busca_antes <- antes
  }
}

normal_crises <- do.call(rbind, normal_crises)

ordens_crises
ajuste_busca

# SISTEMA DE ALERTAS

observado_alertas <- dados_diarios[dados_diarios$Data >= min(faixa_busca$Data) &
                                     dados_diarios$Data <= max(faixa_busca$Data), ]
alertas_busca <- faixa_busca[which(faixa_busca$Alarme), ]

grafico_alertas <- ggplot() +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = cor_trajetorias, alpha = 0.16) +
  geom_ribbon(data = faixa_busca,
              aes(x = Data, ymin = Inferior, ymax = Superior,
                  group = Trecho, fill = "IP SARIMA (95%)"),
              alpha = 0.34) +
  geom_line(data = observado_alertas,
            aes(x = Data, y = InteresseWeb, color = "Observado"),
            linewidth = 0.3) +
  geom_line(data = faixa_busca, aes(Data, Previsto, group = Trecho, color = "Previsto SARIMA"),
            linewidth = 0.5) +
  annotate("text", x = as.Date("2020-11-01"), y = 85, label = "Pandemia",
           size = 3.5, color = cor_principal) +
  geom_point(data = alertas_busca,
             aes(x = Data, y = Observado, color = "Busca excessiva"),
             size = 1.25) +
  scale_color_manual(values = c("Observado" = cor_principal,
                                "Previsto SARIMA" = cor_acf,
                                "Busca excessiva" = cor_referencia),
                     breaks = c("Observado", "Previsto SARIMA", "Busca excessiva")) +
  scale_fill_manual(values = c("IP SARIMA (95%)" = cor_acf)) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(observado_alertas$Data), max(observado_alertas$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(
    title = "Sistema de Alertas de Buscas Excessivas",
    subtitle = "SARIMA(p,0,q)(P,0,Q)₇ (01/01/2013 a 25/10/2026)",
    x = "Ano", y = "Interesse de busca"
  ) +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[16] Sistema de Alertas de Buscas Excessivas.png"),
       plot = grafico_alertas, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# PREVISÃO DO NORMAL NO VOO 1282

modelo_busca <- ordens_crises$Modelo[ordens_crises$Crise == "Voo 1282"]

normal_voo <- normal_crises[normal_crises$Crise == "Voo 1282", ]
noticia_voo <- normal_voo$Noticia[1]
normal_voo <- normal_voo[normal_voo$Data >= noticia_voo - parametros$janela_excesso, ]
normal_voo_apos <- normal_voo[normal_voo$Data >= noticia_voo, ]

grafico_normal_voo <- ggplot() +
  geom_ribbon(data = normal_voo_apos,
              aes(x = Data, ymin = Inferior, ymax = Superior,
                  fill = "IP SARIMA (95%)"),
              alpha = 0.25) +
  geom_vline(xintercept = noticia_voo, color = cor_trajetorias,
             linetype = "dashed", linewidth = 0.5) +
  geom_line(data = normal_voo,
            aes(x = Data, y = Observado, color = "Observado"),
            linewidth = 0.65) +
  geom_line(data = normal_voo_apos,
            aes(x = Data, y = Previsto, color = "Previsto"),
            linewidth = 0.86) +
  scale_color_manual(values = c("Observado" = cor_principal,
                                "Previsto" = cor_acf),
                     breaks = c("Observado", "Previsto")) +
  scale_fill_manual(values = c("IP SARIMA (95%)" = cor_acf)) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  scale_x_date(date_labels = "%d/%m",
               breaks = seq(min(normal_voo$Data), max(normal_voo$Data), by = "1 week"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(
    title = paste("Voo 1282 —", modelo_busca),
    subtitle = "Previsão a partir de 05/01/2024 (08/12/2023 a 18/02/2024)",
    x = "Data", y = "Interesse de busca"
  ) +
  tema_graficos

ggsave(file.path("Resultados", "[18] Voo 1282 - Previsão SARIMA.png"),
       plot = grafico_normal_voo, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# RESÍDUOS DO SARIMA

residuos_busca <- data.frame(
  Data = busca_antes$Data,
  Residuo = as.numeric(residuals(ajuste_busca))
)
residuos_busca$Padronizado <- residuos_busca$Residuo / sqrt(ajuste_busca$sigma2)

fac_residuos_busca <- data.frame(
  Defasagem = 0:28,
  Autocorrelacao = calcular_fac_empirica(residuos_busca$Residuo, 28),
  Limite = 1.96 / sqrt(nrow(residuos_busca))
)

# Graus de liberdade descontam os parâmetros AR e MA
ljung_busca <- Box.test(residuos_busca$Residuo, lag = 14, type = "Ljung-Box",
                        fitdf = sum(ajuste_busca$arma[1:4]))
ljung_busca

grafico_residuos_sarima <- ggplot(residuos_busca, aes(x = Data, y = Residuo)) +
  geom_hline(yintercept = 0, color = cor_trajetorias, linewidth = 0.4) +
  geom_line(color = cor_principal, linewidth = 0.65) +
  scale_x_date(date_labels = "%m/%Y",
               breaks = seq(min(residuos_busca$Data), max(residuos_busca$Data), by = "1 month"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = paste("Resíduos do", modelo_busca),
       subtitle = "Antes do voo 1282 (09/06/2023 a 04/01/2024)",
       x = "Data", y = "Resíduo de ln(Interesse de busca diário)") +
  tema_graficos

ggsave(file.path("Resultados", "[24] Resíduos do SARIMA.png"),
       plot = grafico_residuos_sarima, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_fac_residuos_sarima <- ggplot(fac_residuos_busca[fac_residuos_busca$Defasagem > 0, ], aes(x = Defasagem)) +
  geom_hline(yintercept = 0, color = cor_principal, linewidth = 0.42) +
  geom_hline(yintercept = c(-fac_residuos_busca$Limite[1], fac_residuos_busca$Limite[1]),
             color = cor_acf, linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = Defasagem, y = 0, yend = Autocorrelacao),
               color = cor_principal, linewidth = 0.52) +
  scale_x_continuous(breaks = seq(0, max(fac_residuos_busca$Defasagem), by = 7)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = paste("Função de Autocorrelação dos resíduos do", modelo_busca),
       subtitle = "Antes do voo 1282 (09/06/2023 a 04/01/2024)",
       x = "Defasagem (dias)", y = "Autocorrelação") +
  tema_graficos

ggsave(file.path("Resultados", "[25] FAC dos resíduos do SARIMA.png"),
       plot = grafico_fac_residuos_sarima, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_qq_residuos_sarima <- ggplot(residuos_busca, aes(sample = Padronizado)) +
  stat_qq_line(color = cor_referencia, linewidth = 0.52) +
  stat_qq(color = cor_principal, size = 1.3) +
  scale_x_continuous(labels = label_decimal) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = paste("QQ-plot dos resíduos do", modelo_busca),
       subtitle = "Antes do voo 1282 (09/06/2023 a 04/01/2024)",
       x = "Quantis teóricos", y = "Quantis dos resíduos padronizados") +
  tema_graficos

ggsave(file.path("Resultados", "[26] QQ-plot dos resíduos do SARIMA.png"),
       plot = grafico_qq_residuos_sarima, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")
