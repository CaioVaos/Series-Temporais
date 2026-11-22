# AR(1) NO EXCESSO DE INTERESSE

# Primeiro dia de uma sequência de dias abaixo do limite superior
dia_da_volta <- function(datas, valores, limite, dias_volta){

  seguidos <- 0

  for(t in seq_along(datas)){
    if(t > 1 && datas[t] - datas[t-1] != 1){
      seguidos <- 0
    }

    if(isTRUE(valores[t] <= limite[t])){
      seguidos <- seguidos + 1
      if(seguidos == dias_volta){
        return(datas[t-dias_volta+1])
      }
    } else {
      seguidos <- 0
    }
  }

  return(as.Date(NA))
}

# PREVISÃO DA VOLTA

previsoes_crises <- list()
resultados_crises <- list()
linha <- 0

for(i in 1:nrow(crises)){
  nome_crise <- crises$Crise[i]
  noticia <- crises$Noticia[i]
  normal <- normal_crises[normal_crises$Crise == nome_crise, ]
  depois <- normal[normal$Data >= noticia, ]

  dia0 <- depois$Data[which(depois$Observado > depois$Superior)[1]]
  if(is.na(dia0)){
    stop(paste("Sem primeiro excesso em", nome_crise))
  }

  observado <- depois[depois$Data >= dia0, ]
  volta_observada <- dia_da_volta(observado$Data, observado$Observado,
                                 observado$Superior, parametros$dias_volta)

  for(dia in parametros$dias_decisao){
    decisao <- dia0 + dia
    base_excesso <- normal[normal$Data >= noticia - parametros$janela_excesso &
                            normal$Data <= decisao, c("Data", "Excesso")]
    ajuste_ar <- arima(base_excesso$Excesso, order = c(1, 0, 0),
                       include.mean = FALSE, method = "ML")
    phi <- unname(coef(ajuste_ar)["ar1"])
    futuro <- depois[depois$Data > decisao, ]
    horizonte <- seq_len(nrow(futuro))

    # Previsão recursiva e variância das inovações acumuladas
    excesso_previsto <- phi^horizonte * tail(base_excesso$Excesso, 1)
    variancia_excesso <- ajuste_ar$sigma2 * cumsum(phi^(2*(horizonte-1)))
    margem <- qnorm(0.975) * sqrt(variancia_excesso)

    # Soma da referência SARIMA e do excesso AR(1) na escala ln
    # O intervalo mantém a referência SARIMA fixa
    previsao <- data.frame(
      Data = futuro$Data, Crise = nome_crise, Dia = dia, Decisao = decisao,
      Observado = futuro$Observado,
      Previsto = exp(futuro$MediaLn + excesso_previsto + variancia_excesso/2),
      Inferior = exp(futuro$MediaLn + excesso_previsto - margem),
      Superior = exp(futuro$MediaLn + excesso_previsto + margem),
      ExcessoPrevisto = excesso_previsto,
      InferiorExcesso = excesso_previsto - margem,
      SuperiorExcesso = excesso_previsto + margem
    )

    volta_prevista <- dia_da_volta(previsao$Data, previsao$Previsto,
                                  futuro$Superior, parametros$dias_volta)
    elegivel <- !is.na(volta_observada) && decisao < volta_observada
    medidas <- c(RMSE = NA, MAE = NA, R2 = NA)

    if(elegivel){
      avaliacao <- head(previsao, 30)
      medidas <- diagnostico(avaliacao$Observado, avaliacao$Observado - avaliacao$Previsto)
    }

    linha <- linha + 1
    previsoes_crises[[linha]] <- previsao
    resultados_crises[[linha]] <- data.frame(
      Crise = nome_crise, Noticia = noticia, Grupo = crises$Grupo[i],
      Dia0 = dia0, Dia = dia, Decisao = decisao,
      VoltaObservada = volta_observada, Confirmacao = volta_observada + parametros$dias_volta - 1,
      VoltaPrevista = volta_prevista, ErroDias = as.integer(volta_prevista - volta_observada),
      RMSE = medidas["RMSE"], MAE = medidas["MAE"], R2 = medidas["R2"],
      Phi = phi, Elegivel = elegivel, Horizonte = min(30, nrow(previsao)), row.names = NULL
    )

    if(nome_crise == "Voo 1282" && dia == parametros$dia_previsao){
      ajuste_excesso <- ajuste_ar
      excesso_ajuste <- base_excesso
    }
  }
}

previsoes_crises <- do.call(rbind, previsoes_crises)
resultados_crises <- do.call(rbind, resultados_crises)

ajuste_excesso
resultados_crises[resultados_crises$Dia == parametros$dia_previsao, ]

# PREVISÕES PARA O VOO 1282

previsao_dia_dois <- previsoes_crises[previsoes_crises$Crise == "Voo 1282" &
                                       previsoes_crises$Dia == parametros$dia_previsao, ]
retorno_voo <- resultados_crises[resultados_crises$Crise == "Voo 1282" &
                                  resultados_crises$Dia == parametros$dia_previsao, ]

grafico_excesso_voo <- ggplot() +
  geom_ribbon(data = previsao_dia_dois,
              aes(Data, ymin = InferiorExcesso, ymax = SuperiorExcesso,
                  fill = "IP do excesso AR (95%)"), alpha = 0.25) +
  geom_hline(yintercept = 0, color = cor_trajetorias, linewidth = 0.4) +
  geom_vline(xintercept = previsao_dia_dois$Decisao[1], color = cor_trajetorias,
             linetype = "dashed", linewidth = 0.5) +
  geom_line(data = normal_voo, aes(Data, Excesso, color = "Observado"), linewidth = 0.65) +
  geom_line(data = previsao_dia_dois, aes(Data, ExcessoPrevisto, color = "Previsto"), linewidth = 0.86) +
  scale_color_manual(values = c("Observado" = cor_principal, "Previsto" = cor_covariancia)) +
  scale_fill_manual(values = c("IP do excesso AR (95%)" = cor_covariancia)) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  scale_x_date(date_labels = "%d/%m", limits = as.Date(c("2023-12-08", "2024-02-18")),
               breaks = seq(as.Date("2023-12-08"), as.Date("2024-02-18"), by = "1 week"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Voo 1282 e AR(1) no excesso de interesse",
       subtitle = "Dois dias após o pico (08/01/2024)",
       x = "Data", y = "Excesso de interesse") +
  tema_graficos

ggsave(file.path("Resultados", "[19] Voo 1282 - Previsão AR(1) do excesso de interesse.png"),
       plot = grafico_excesso_voo, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# Referência SARIMA e excesso na escala ln
etapas_voo <- rbind(
  data.frame(Data = normal_voo$Data, Etapa = "SARIMA - Interesse de busca",
             Valor = normal_voo$Observado, Serie = "Observado"),
  data.frame(Data = normal_voo_apos$Data, Etapa = "SARIMA - Interesse de busca",
             Valor = normal_voo_apos$Previsto, Serie = "Previsto SARIMA"),
  data.frame(Data = normal_voo$Data, Etapa = "AR(1) - Excesso de interesse",
             Valor = normal_voo$Excesso, Serie = "Observado"),
  data.frame(Data = previsao_dia_dois$Data, Etapa = "AR(1) - Excesso de interesse",
             Valor = previsao_dia_dois$ExcessoPrevisto, Serie = "Previsto AR(1)")
)
faixas_etapas_voo <- rbind(
  data.frame(Data = normal_voo_apos$Data, Etapa = "SARIMA - Interesse de busca",
             Inferior = normal_voo_apos$Inferior, Superior = normal_voo_apos$Superior,
             Intervalo = "IP SARIMA (95%)"),
  data.frame(Data = previsao_dia_dois$Data, Etapa = "AR(1) - Excesso de interesse",
             Inferior = previsao_dia_dois$InferiorExcesso, Superior = previsao_dia_dois$SuperiorExcesso,
             Intervalo = "IP do excesso AR (95%)")
)
etapas_voo$Etapa <- factor(etapas_voo$Etapa,
                           levels = c("SARIMA - Interesse de busca", "AR(1) - Excesso de interesse"))
faixas_etapas_voo$Etapa <- factor(faixas_etapas_voo$Etapa, levels = levels(etapas_voo$Etapa))

grafico_etapas_voo <- ggplot(etapas_voo, aes(Data, Valor)) +
  geom_ribbon(data = faixas_etapas_voo,
              aes(x = Data, ymin = Inferior, ymax = Superior, fill = Intervalo),
              inherit.aes = FALSE, alpha = 0.22) +
  geom_vline(xintercept = previsao_dia_dois$Decisao[1], color = cor_trajetorias,
             linetype = "dashed", linewidth = 0.4) +
  geom_line(aes(color = Serie), linewidth = 0.65) +
  facet_wrap(~ Etapa, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Observado" = cor_principal, "Previsto SARIMA" = cor_acf,
                                "Previsto AR(1)" = cor_covariancia)) +
  scale_fill_manual(values = c("IP SARIMA (95%)" = cor_acf,
                               "IP do excesso AR (95%)" = cor_covariancia)) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  scale_x_date(date_labels = "%d/%m", limits = as.Date(c("2023-12-08", "2024-02-18")),
               breaks = seq(as.Date("2023-12-08"), as.Date("2024-02-18"), by = "1 week"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Voo 1282 — Referência SARIMA e excesso AR(1)",
       subtitle = "SARIMA até 04/01/2024 e AR até 08/01/2024", x = "Data", y = NULL) +
  tema_graficos +
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))

ggsave(file.path("Resultados", "[20] Voo 1282 - SARIMA e AR(1).png"),
       plot = grafico_etapas_voo, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# Previsão na escala original do interesse
grafico_previsoes_voo <- ggplot() +
  geom_ribbon(data = normal_voo_apos,
              aes(Data, ymin = Inferior, ymax = Superior, fill = "IP SARIMA (95%)"), alpha = 0.16) +
  geom_ribbon(data = previsao_dia_dois,
              aes(Data, ymin = Inferior, ymax = Superior, fill = "IP combinado dado SARIMA (95%)"), alpha = 0.16) +
  geom_line(data = normal_voo, aes(Data, Observado, color = "Observado"), linewidth = 0.65) +
  geom_line(data = normal_voo_apos, aes(Data, Previsto, color = "Previsto SARIMA"), linewidth = 0.75) +
  geom_line(data = previsao_dia_dois, aes(Data, Previsto, color = "Previsto SARIMA + AR(1)"), linewidth = 0.85) +
  geom_vline(xintercept = previsao_dia_dois$Decisao[1], color = cor_trajetorias,
             linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = c("Observado" = cor_principal, "Previsto SARIMA" = cor_acf,
                                "Previsto SARIMA + AR(1)" = cor_referencia)) +
  scale_fill_manual(values = c("IP SARIMA (95%)" = cor_acf,
                               "IP combinado dado SARIMA (95%)" = cor_referencia)) +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  scale_x_date(date_labels = "%d/%m", limits = as.Date(c("2023-12-08", "2024-02-18")),
               breaks = seq(as.Date("2023-12-08"), as.Date("2024-02-18"), by = "1 week"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = paste("Voo 1282 —", modelo_busca, "e AR(1)"),
       subtitle = "SARIMA até 04/01/2024 e AR até 08/01/2024",
       x = "Data", y = "Interesse de busca diário") +
  tema_graficos

ggsave(file.path("Resultados", "[21] Voo 1282 - Previsões SARIMA e SARIMA + AR.png"),
       plot = grafico_previsoes_voo, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# Previsão de retorno
grafico_decisao_dia_dois <- ggplot() +
  geom_ribbon(data = previsao_dia_dois,
              aes(Data, ymin = Inferior, ymax = Superior,
                  fill = "IP combinado dado SARIMA (95%)"), alpha = 0.22) +
  geom_vline(xintercept = previsao_dia_dois$Decisao[1], color = cor_trajetorias,
             linetype = "dashed", linewidth = 0.5) +
  geom_line(data = normal_voo_apos, aes(Data, Inferior, linetype = "IP SARIMA (95%)"),
            color = cor_acf, linewidth = 0.5) +
  geom_line(data = normal_voo_apos, aes(Data, Superior, linetype = "IP SARIMA (95%)"),
            color = cor_acf, linewidth = 0.5) +
  geom_line(data = normal_voo, aes(Data, Observado, color = "Observado"), linewidth = 0.65) +
  geom_line(data = previsao_dia_dois, aes(Data, Previsto, color = "Previsto"), linewidth = 0.86) +
  annotate("segment", x = retorno_voo$VoltaPrevista, xend = retorno_voo$VoltaPrevista,
           y = 0, yend = 58, color = cor_referencia, linetype = "dashed", linewidth = 0.4) +
  annotate("segment", x = retorno_voo$VoltaObservada, xend = retorno_voo$VoltaObservada,
           y = 0, yend = 48, color = cor_principal, linetype = "dashed", linewidth = 0.4) +
  annotate("text", x = retorno_voo$VoltaPrevista - 2, y = 61,
           label = paste0("Retorno previsto\n", format(retorno_voo$VoltaPrevista, "%d/%m/%Y")),
           color = cor_referencia, hjust = 1, size = 3.8) +
  annotate("text", x = retorno_voo$VoltaObservada + 2, y = 51,
           label = paste0("Início do retorno observado\n", format(retorno_voo$VoltaObservada, "%d/%m/%Y")),
           color = cor_principal, hjust = 0, size = 3.8) +
  scale_color_manual(values = c("Observado" = cor_principal, "Previsto" = cor_referencia)) +
  scale_linetype_manual(values = c("IP SARIMA (95%)" = "dashed")) +
  scale_fill_manual(values = c("IP combinado dado SARIMA (95%)" = cor_covariancia)) +
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) +
  scale_x_date(date_labels = "%d/%m", limits = as.Date(c("2023-12-08", "2024-02-18")),
               breaks = seq(as.Date("2023-12-08"), as.Date("2024-02-18"), by = "1 week"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = paste("Voo 1282 —", modelo_busca, "+ AR(1)"),
       subtitle = "SARIMA até 04/01/2024 e AR até 08/01/2024",
       x = "Data", y = "Interesse de busca") +
  tema_graficos

ggsave(file.path("Resultados", "[22] Voo 1282 - Previsão dois dias após o pico.png"),
       plot = grafico_decisao_dia_dois, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

tabela_retornos <- resultados_crises[, c("Crise", "Grupo", "Dia0", "Dia", "Decisao",
                                         "VoltaPrevista", "VoltaObservada", "Confirmacao",
                                         "ErroDias", "Elegivel", "Phi")]
tabela_retornos$Situacao <- ifelse(is.na(tabela_retornos$VoltaPrevista),
                                  "Sem retorno previsto no horizonte",
                                  "Retorno previsto no horizonte")
tabela_retornos$Elegivel <- ifelse(is.na(tabela_retornos$VoltaObservada),
                                  "Sem retorno observado no horizonte",
                                  ifelse(tabela_retornos$Elegivel, "Antes do início",
                                         "A partir do início"))
tabela_retornos$Phi <- round(tabela_retornos$Phi, 4)
names(tabela_retornos) <- c("Crise", "Conjunto", "Detecção da busca excessiva", "Dias após a detecção",
                           "Decisão", "Retorno previsto", "Início do retorno observado",
                           "Confirmação observada", "Erro em dias",
                           "Decisão em relação ao retorno observado", "Coeficiente AR(1)", "Situação")

write_xlsx(tabela_retornos, "Resultados/[23] Previsão de retorno nas crises.xlsx")

# RESÍDUOS DO AR(1)

residuos_excesso <- data.frame(
  Data = excesso_ajuste$Data,
  Residuo = as.numeric(residuals(ajuste_excesso))
)
residuos_excesso$Padronizado <- residuos_excesso$Residuo / sqrt(ajuste_excesso$sigma2)

fac_residuos_excesso <- data.frame(
  Defasagem = 0:14,
  Autocorrelacao = calcular_fac_empirica(residuos_excesso$Residuo, 14),
  Limite = 1.96 / sqrt(nrow(residuos_excesso))
)

ljung_excesso <- Box.test(residuos_excesso$Residuo, lag = 14, type = "Ljung-Box", fitdf = 1)
ljung_excesso

grafico_residuos_ar1 <- ggplot(residuos_excesso, aes(x = Data, y = Residuo)) +
  geom_hline(yintercept = 0, color = cor_trajetorias, linewidth = 0.4) +
  geom_line(color = cor_principal, linewidth = 0.65) +
  scale_x_date(date_labels = "%d/%m",
               breaks = seq(min(residuos_excesso$Data), max(residuos_excesso$Data), by = "1 week"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Resíduos do AR(1) no excesso de interesse",
       subtitle = "Dois dias após o pico (08/12/2023 a 08/01/2024)",
       x = "Data", y = "Resíduo do excesso de interesse") +
  tema_graficos

ggsave(file.path("Resultados", "[27] Resíduos do AR(1).png"),
       plot = grafico_residuos_ar1, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_fac_residuos_ar1 <- ggplot(fac_residuos_excesso[fac_residuos_excesso$Defasagem > 0, ], aes(x = Defasagem)) +
  geom_hline(yintercept = 0, color = cor_principal, linewidth = 0.42) +
  geom_hline(yintercept = c(-fac_residuos_excesso$Limite[1], fac_residuos_excesso$Limite[1]),
             color = cor_acf, linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = Defasagem, y = 0, yend = Autocorrelacao),
               color = cor_principal, linewidth = 0.52) +
  scale_x_continuous(breaks = seq(0, max(fac_residuos_excesso$Defasagem), by = 2)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Função de Autocorrelação dos resíduos do AR(1)",
       subtitle = "Dois dias após o pico (08/12/2023 a 08/01/2024)",
       x = "Defasagem (dias)", y = "Autocorrelação") +
  tema_graficos

ggsave(file.path("Resultados", "[28] FAC dos resíduos do AR(1).png"),
       plot = grafico_fac_residuos_ar1, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_qq_residuos_ar1 <- ggplot(residuos_excesso, aes(sample = Padronizado)) +
  stat_qq_line(color = cor_referencia, linewidth = 0.52) +
  stat_qq(color = cor_principal, size = 1.3) +
  scale_x_continuous(labels = label_decimal) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = paste("QQ-plot dos resíduos do", "AR(1)"),
       subtitle = "Dois dias após o pico (08/12/2023 a 08/01/2024)",
       x = "Quantis teóricos", y = "Quantis dos resíduos padronizados") +
  tema_graficos

ggsave(file.path("Resultados", "[29] QQ-plot dos resíduos do AR(1).png"),
       plot = grafico_qq_residuos_ar1, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")
