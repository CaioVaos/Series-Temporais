# ANÁLISE DESCRITIVA DO INTERESSE DE BUSCA

rotulo_media_diaria <- paste("Média móvel de", parametros$media_movel_dias, "dias")

grafico_interesse_diario <- ggplot(dados_diarios, aes(x = Data)) +
  geom_line(aes(y = InteresseWeb, color = "Observado"),
            linewidth = 0.27, alpha = 0.62) +
  geom_line(aes(y = MediaMovel, color = rotulo_media_diaria),
            linewidth = 0.78, na.rm = TRUE) +
  geom_vline(xintercept = as.Date("2024-01-05"), color = cor_trajetorias,
             linetype = "dashed", linewidth = 0.4) +
  annotate("text", x = as.Date("2024-01-05"), y = 72,
           label = "Voo 1282\n(05/01/2024)", hjust = 1.1, size = 3.8) +
  scale_color_manual(
    values = setNames(c(cor_trajetorias, cor_referencia),
                      c("Observado", rotulo_media_diaria)),
    breaks = c("Observado", rotulo_media_diaria)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = c(0.55, 0.78), alpha = c(0.85, 1)))) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(dados_diarios$Data), max(dados_diarios$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(
    title = "Interesse de busca diário pela Alaska Airlines",
    x = "Ano", y = "Interesse de busca"
  ) +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[01] Interesse de busca diário.png"),
       plot = grafico_interesse_diario, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_interesse_semanal <- ggplot(dados_semanais, aes(x = Data, y = InteresseWeb)) +
  geom_line(color = cor_principal, linewidth = 0.55) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(dados_semanais$Data), max(dados_semanais$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(
    title = "Interesse de busca semanal pela Alaska Airlines",
    x = "Ano", y = "Interesse de busca"
  ) +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[02] Interesse de busca semanal.png"),
       plot = grafico_interesse_semanal, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_interesse_mensal <- ggplot(dados_mensais, aes(x = Data, y = InteresseWeb)) +
  geom_line(color = cor_principal, linewidth = 0.65) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(dados_mensais$Data), max(dados_mensais$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(
    title = "Interesse de busca mensal pela Alaska Airlines",
    x = "Ano", y = "Interesse de busca"
  ) +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[03] Interesse de busca mensal.png"),
       plot = grafico_interesse_mensal, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# INTERESSE DE BUSCA POR DIA DA SEMANA

dias_semana <- subset(dados_diarios, Data < as.Date("2026-01-01"))
dias_semana$Ano <- as.integer(format(dias_semana$Data, "%Y"))
dias_semana$Dia <- as.integer(format(dias_semana$Data, "%u"))

interesse_por_dia <- data.frame(Ano = rep(2012:2025, each = 7),
                                Dia = rep(1:7, 14),
                                Observacoes = 0, Media = 0, Indice = 0)

# Cada ano é dividido pela sua própria média
for(ano in 2012:2025){
  dados_ano <- dias_semana[dias_semana$Ano == ano, ]
  media_ano <- calcular_media_amostral(dados_ano$InteresseWeb)

  for(dia in 1:7){
    valores <- dados_ano$InteresseWeb[dados_ano$Dia == dia]
    linha <- interesse_por_dia$Ano == ano & interesse_por_dia$Dia == dia
    interesse_por_dia$Observacoes[linha] <- length(valores)
    interesse_por_dia$Media[linha] <- calcular_media_amostral(valores)
    interesse_por_dia$Indice[linha] <- 100 * interesse_por_dia$Media[linha] / media_ano
  }
}

resumo_dias_semana <- data.frame(Dia = 1:7, Media = 0, Minimo = 0, Maximo = 0)

for(dia in 1:7){
  valores <- interesse_por_dia$Indice[interesse_por_dia$Dia == dia]
  resumo_dias_semana$Media[dia] <- calcular_media_amostral(valores)
  resumo_dias_semana$Minimo[dia] <- min(valores)
  resumo_dias_semana$Maximo[dia] <- max(valores)
}

interesse_por_dia
resumo_dias_semana

grafico_dias_semana <- ggplot(interesse_por_dia, aes(x = Dia, y = Indice)) +
  geom_hline(yintercept = 100, color = cor_trajetorias,
             linetype = "dashed", linewidth = 0.5) +
  geom_point(aes(color = "Cada ano"), size = 1.45, alpha = 0.75,
             position = position_jitter(width = 0.16, height = 0, seed = 1)) +
  geom_segment(data = resumo_dias_semana,
               aes(x = Dia - 0.25, xend = Dia + 0.25, y = Media, yend = Media,
                   color = "Média dos anos"),
               inherit.aes = FALSE, linewidth = 0.82) +
  scale_color_manual(values = c("Cada ano" = cor_principal,
                                "Média dos anos" = cor_referencia),
                     breaks = c("Cada ano", "Média dos anos")) +
  scale_x_continuous(breaks = 1:7,
                     labels = c("Segunda", "Terça", "Quarta", "Quinta",
                                "Sexta", "Sábado", "Domingo")) +
  scale_y_continuous(labels = label_decimal, breaks = scales::breaks_width(5)) +
  labs(title = "Interesse de busca por dia da semana",
       subtitle = "2012 a 2025",
       x = "Dia da semana", y = "Interesse de busca (% da média anual)") +
  tema_graficos

ggsave(file.path("Resultados", "[06] Interesse de busca por dia da semana.png"),
       plot = grafico_dias_semana, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# DECOMPOSIÇÃO MENSAL

mensal_antes <- subset(dados_mensais, Data < as.Date("2020-01-01"))
decomposicao_interesse <- data.frame(
  Data = mensal_antes$Data,
  Observado = log(mensal_antes$InteresseWeb),
  decompor_mensal(log(mensal_antes$InteresseWeb))
)

decomposicao_interesse_long <- data.frame(
  Data = rep(decomposicao_interesse$Data, 3),
  Termo = factor(rep(c("Tendência", "Sazonalidade", "Ruído"), each = nrow(decomposicao_interesse)),
                 levels = c("Tendência", "Sazonalidade", "Ruído")),
  Valor = c(decomposicao_interesse$Tendencia, decomposicao_interesse$Sazonalidade,
            decomposicao_interesse$Ruido)
)

grafico_decomposicao_interesse <- ggplot(decomposicao_interesse_long, aes(Data, Valor)) +
  geom_line(color = cor_principal, linewidth = 0.55, na.rm = TRUE) +
  facet_grid(Termo ~ ., scales = "free_y", switch = "y") +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date("2012-01-01"), as.Date("2019-01-01"), by = "1 year")) +
  scale_y_continuous(labels = label_decimal, breaks = scales::breaks_pretty(3)) +
  labs(title = "Decomposição de ln(Interesse de busca mensal)", x = "Ano", y = NULL) +
  tema_graficos +
  theme(strip.background = element_blank(), strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, size = 12)) +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[07] Decomposição de ln(Interesse de busca mensal).png"),
       plot = grafico_decomposicao_interesse, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# Trecho posterior à pandemia, estimado separadamente
mensal_depois <- subset(dados_mensais, Data >= as.Date("2021-07-01"))
decomposicao_interesse_depois <- data.frame(
  Data = mensal_depois$Data,
  Observado = log(mensal_depois$InteresseWeb),
  decompor_mensal(log(mensal_depois$InteresseWeb))
)
decomposicao_depois_long <- data.frame(
  Data = rep(decomposicao_interesse_depois$Data, 3),
  Termo = factor(rep(c("Tendência", "Sazonalidade", "Ruído"), each = nrow(decomposicao_interesse_depois)),
                 levels = c("Tendência", "Sazonalidade", "Ruído")),
  Valor = c(decomposicao_interesse_depois$Tendencia, decomposicao_interesse_depois$Sazonalidade,
            decomposicao_interesse_depois$Ruido)
)

grafico_decomposicao_depois <- ggplot(decomposicao_depois_long, aes(Data, Valor)) +
  geom_line(color = cor_principal, linewidth = 0.55, na.rm = TRUE) +
  facet_grid(Termo ~ ., scales = "free_y", switch = "y") +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date("2021-07-01"), as.Date("2026-07-01"), by = "1 year")) +
  scale_y_continuous(labels = label_decimal, breaks = scales::breaks_pretty(3)) +
  labs(title = "Decomposição de ln(Interesse de busca mensal)", x = "Ano", y = NULL) +
  tema_graficos +
  theme(strip.background = element_blank(), strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, size = 12)) +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[08] Decomposição mensal do interesse de busca após a pandemia.png"),
       plot = grafico_decomposicao_depois, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

decomposicao_interesse_long$Trecho <- "2012 a 2019"
decomposicao_depois_long$Trecho <- "Após a pandemia"
decomposicao_completa <- rbind(decomposicao_interesse_long, decomposicao_depois_long)

grafico_decomposicao_completa <- ggplot(decomposicao_completa, aes(Data, Valor, group = Trecho)) +
  annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, fill = cor_trajetorias, alpha = 0.18) +
  geom_line(color = cor_principal, linewidth = 0.55, na.rm = TRUE) +
  facet_grid(Termo ~ ., scales = "free_y", switch = "y") +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date("2012-01-01"), as.Date("2026-01-01"), by = "1 year")) +
  scale_y_continuous(labels = label_decimal, breaks = scales::breaks_pretty(3)) +
  labs(title = "Decomposição de ln(Interesse de busca mensal)",
       subtitle = "Trechos separados pelo intervalo excluído de 2020 a junho de 2021",
       x = "Ano", y = NULL) +
  tema_graficos +
  theme(strip.background = element_blank(), strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, size = 12),
        axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[09] Decomposição mensal do interesse de busca - Série completa.png"),
       plot = grafico_decomposicao_completa, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# SÉRIE ANTERIOR AO VOO 1282

noticia_voo <- crises$Noticia[crises$Crise == "Voo 1282"]
busca_antes <- dados_diarios[dados_diarios$Data >= noticia_voo - parametros$janela_normal &
                              dados_diarios$Data < noticia_voo, ]
busca_antes$LnInteresse <- log(busca_antes$InteresseWeb)
# ESTACIONARIEDADE ANTES DO VOO 1282

# ADF com constante e uma defasagem, KPSS de nível com defasagens curtas
adf_interesse <- ur.df(busca_antes$InteresseWeb, type = "drift")
summary(adf_interesse)
kpss_interesse <- ur.kpss(busca_antes$InteresseWeb)
summary(kpss_interesse)

adf_log_interesse <- ur.df(busca_antes$LnInteresse, type = "drift")
summary(adf_log_interesse)
kpss_log_interesse <- ur.kpss(busca_antes$LnInteresse)
summary(kpss_log_interesse)

testes_busca <- data.frame(
  Serie = rep(c("Interesse de busca", "ln(Interesse de busca diário)"), each = 2),
  Teste = rep(c("ADF", "KPSS"), 2),
  H0 = rep(c("Raiz unitária", "Estacionária em nível"), 2),
  Inicio = min(busca_antes$Data),
  Fim = max(busca_antes$Data),
  Observacoes = nrow(busca_antes),
  Defasagens = c(adf_interesse@lags, kpss_interesse@lag,
                adf_log_interesse@lags, kpss_log_interesse@lag),
  Estatistica = c(adf_interesse@teststat[1, "tau2"], kpss_interesse@teststat,
                  adf_log_interesse@teststat[1, "tau2"], kpss_log_interesse@teststat),
  Critico5 = c(adf_interesse@cval["tau2", "5pct"], kpss_interesse@cval[1, "5pct"],
               adf_log_interesse@cval["tau2", "5pct"], kpss_log_interesse@cval[1, "5pct"])
)

# LOG, FAC E FACP DO INTERESSE DE BUSCA

fac_busca <- data.frame(
  Defasagem = 0:28,
  Autocorrelacao = calcular_fac_empirica(busca_antes$LnInteresse, 28),
  Limite = 1.96 / sqrt(nrow(busca_antes))
)
facp_busca <- data.frame(
  Defasagem = 1:28,
  Autocorrelacao = as.numeric(pacf(busca_antes$LnInteresse, lag.max = 28, plot = FALSE)$acf),
  Limite = 1.96 / sqrt(nrow(busca_antes))
)

grafico_log_interesse_voo1282 <- ggplot(busca_antes, aes(x = Data, y = LnInteresse)) +
  geom_line(color = cor_principal, linewidth = 0.65) +
  scale_x_date(date_labels = "%m/%Y",
               breaks = seq(min(busca_antes$Data), max(busca_antes$Data), by = "1 month"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "ln(Interesse de busca diário) antes do voo 1282",
       x = "Data", y = "ln(Interesse de busca diário)") +
  tema_graficos

ggsave(file.path("Resultados", "[12] ln(Interesse de busca diário) antes do voo 1282.png"),
       plot = grafico_log_interesse_voo1282, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_fac_interesse_voo1282 <- ggplot(fac_busca[fac_busca$Defasagem > 0, ], aes(x = Defasagem)) +
  geom_hline(yintercept = 0, color = cor_principal, linewidth = 0.42) +
  geom_hline(yintercept = c(-fac_busca$Limite[1], fac_busca$Limite[1]),
             color = cor_acf, linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = Defasagem, y = 0, yend = Autocorrelacao),
               color = cor_principal, linewidth = 0.52) +
  scale_x_continuous(breaks = seq(0, max(fac_busca$Defasagem), by = 7)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Função de Autocorrelação de ln(Interesse de busca diário)",
       subtitle = "Antes do voo 1282 (09/06/2023 a 04/01/2024)",
       x = "Defasagem (dias)", y = "Autocorrelação") +
  tema_graficos

ggsave(file.path("Resultados", "[13] FAC de ln(Interesse de busca diário).png"),
       plot = grafico_fac_interesse_voo1282, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_facp_interesse_voo1282 <- ggplot(facp_busca, aes(x = Defasagem)) +
  geom_hline(yintercept = 0, color = cor_principal, linewidth = 0.42) +
  geom_hline(yintercept = c(-facp_busca$Limite[1], facp_busca$Limite[1]),
             color = cor_acf, linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = Defasagem, y = 0, yend = Autocorrelacao),
               color = cor_principal, linewidth = 0.52) +
  scale_x_continuous(breaks = seq(0, max(facp_busca$Defasagem), by = 7)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Função de Autocorrelação Parcial de ln(Interesse de busca diário)",
       subtitle = "Antes do voo 1282 (09/06/2023 a 04/01/2024)",
       x = "Defasagem (dias)", y = "Autocorrelação parcial") +
  tema_graficos

ggsave(file.path("Resultados", "[14] FACP de ln(Interesse de busca diário).png"),
       plot = grafico_facp_interesse_voo1282, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")
