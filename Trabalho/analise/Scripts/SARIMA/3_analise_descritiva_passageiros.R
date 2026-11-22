# ANÁLISE DESCRITIVA DOS PASSAGEIROS

passageiros_antes <- dados_passageiros[dados_passageiros$Data < as.Date("2020-03-01"), ]
n <- nrow(passageiros_antes)
n_treino <- floor(0.6*n)
n_validacao <- floor(0.2*n)
n_treino_validacao <- n_treino + n_validacao

treino <- passageiros_antes[1:n_treino, ]

ln_passageiros <- log(passageiros_antes$PassageirosMilhoes)

rotulo_media_passageiros <- paste("Média móvel de", parametros$media_movel_meses, "meses")

grafico_passageiros <- ggplot(dados_passageiros, aes(x = Data)) +
  geom_line(aes(y = PassageirosMilhoes, color = "Observado"),
            linewidth = 0.43, alpha = 0.68) +
  geom_line(aes(y = MediaMovel, color = rotulo_media_passageiros),
            linewidth = 0.82, na.rm = TRUE) +
  scale_color_manual(
    values = setNames(c(cor_trajetorias, cor_referencia),
                      c("Observado", rotulo_media_passageiros)),
    breaks = c("Observado", rotulo_media_passageiros)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = c(0.55, 0.78), alpha = c(0.85, 1)))) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(dados_passageiros$Data), max(dados_passageiros$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(
    title = "Nº de passageiros por mês",
    x = "Ano", y = "Nº de passageiros (milhões)"
  ) +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[04] Nº de passageiros por mês.png"),
       plot = grafico_passageiros, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# DECOMPOSIÇÃO MENSAL

decomposicao_passageiros <- data.frame(
  Data = passageiros_antes$Data,
  Observado = log(passageiros_antes$PassageirosMilhoes),
  decompor_mensal(log(passageiros_antes$PassageirosMilhoes))
)

decomposicao_passageiros_long <- data.frame(
  Data = rep(decomposicao_passageiros$Data, 3),
  Termo = factor(rep(c("Tendência", "Sazonalidade", "Ruído"), each = nrow(decomposicao_passageiros)),
                 levels = c("Tendência", "Sazonalidade", "Ruído")),
  Valor = c(decomposicao_passageiros$Tendencia, decomposicao_passageiros$Sazonalidade,
            decomposicao_passageiros$Ruido)
)

grafico_decomposicao_passageiros <- ggplot(decomposicao_passageiros_long, aes(Data, Valor)) +
  geom_line(color = cor_principal, linewidth = 0.55, na.rm = TRUE) +
  facet_grid(Termo ~ ., scales = "free_y", switch = "y") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = label_decimal, breaks = scales::breaks_pretty(3)) +
  labs(title = "Decomposição de ln(Nº mensal de passageiros em milhões)", x = "Ano", y = NULL) +
  tema_graficos +
  theme(strip.background = element_blank(), strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, size = 12)) +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[10] Decomposição de ln(Nº mensal de passageiros).png"),
       plot = grafico_decomposicao_passageiros, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# DIFERENCIAÇÕES DE ln(Nº mensal de passageiros em milhões)

ln_treino <- log(treino$PassageirosMilhoes)
diferenca_simples <- numeric(n_treino)
diferenca_sazonal <- numeric(n_treino)
duas_diferencas <- numeric(n_treino)
diferenca_simples[1] <- NA
diferenca_sazonal[1:12] <- NA
duas_diferencas[1:13] <- NA

for(t in 2:n_treino){
  diferenca_simples[t] <- ln_treino[t] - ln_treino[t-1]
}

for(t in 13:n_treino){
  diferenca_sazonal[t] <- ln_treino[t] - ln_treino[t-12]
}

for(t in 14:n_treino){
  duas_diferencas[t] <- diferenca_sazonal[t] - diferenca_sazonal[t-1]
}

passageiros_transformados <- data.frame(
  Data = treino$Data, PassageirosMilhoes = treino$PassageirosMilhoes,
  LnPassageiros = ln_treino, DiferencaSimples = diferenca_simples,
  DiferencaSazonal = diferenca_sazonal, DuasDiferencas = duas_diferencas
)

grafico_ln_passageiros <- ggplot(passageiros_transformados, aes(x = Data, y = LnPassageiros)) +
  geom_line(color = cor_principal, linewidth = 0.65) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(passageiros_transformados$Data), max(passageiros_transformados$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "ln(Nº mensal de passageiros)",
       subtitle = "Treino (01/01/2002 a 31/10/2012)",
       x = "Ano", y = "ln(Nº mensal de passageiros em milhões)") +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[30] ln(Nº mensal de passageiros).png"),
       plot = grafico_ln_passageiros, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_diferenca_passageiros <- ggplot(passageiros_transformados, aes(x = Data, y = DiferencaSimples)) +
  geom_hline(yintercept = 0, color = cor_trajetorias, linewidth = 0.4) +
  geom_line(color = cor_principal, linewidth = 0.65, na.rm = TRUE) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(passageiros_transformados$Data), max(passageiros_transformados$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "1ª diferenciação de ln(Nº mensal de passageiros)",
       subtitle = "Treino (01/02/2002 a 31/10/2012)",
       x = "Ano", y = "∇ ln(Nº mensal de passageiros em milhões)") +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[31] 1ª diferenciação de ln(Nº mensal de passageiros).png"),
       plot = grafico_diferenca_passageiros, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_duas_diferencas_passageiros <- ggplot(passageiros_transformados, aes(x = Data, y = DuasDiferencas)) +
  geom_hline(yintercept = 0, color = cor_trajetorias, linewidth = 0.4) +
  geom_line(color = cor_principal, linewidth = 0.65, na.rm = TRUE) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(passageiros_transformados$Data), max(passageiros_transformados$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Diferenciações de ln(Nº mensal de passageiros)",
       subtitle = "Mensal e sazonal de 12 meses (01/02/2003 a 31/10/2012)",
       x = "Ano", y = "∇∇₁₂ ln(Nº mensal de passageiros em milhões)") +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[32] 2ª etapa de diferenciação de ln(Nº mensal de passageiros).png"),
       plot = grafico_duas_diferencas_passageiros, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

# ESTACIONARIEDADE, FAC E FACP

adf_passageiros <- ur.df(passageiros_transformados$LnPassageiros, type = "drift")
summary(adf_passageiros)
kpss_passageiros <- ur.kpss(passageiros_transformados$LnPassageiros)
summary(kpss_passageiros)

adf_diferenca <- ur.df(passageiros_transformados$DiferencaSimples[-1], type = "drift")
summary(adf_diferenca)
kpss_diferenca <- ur.kpss(passageiros_transformados$DiferencaSimples[-1])
summary(kpss_diferenca)

adf_sazonal <- ur.df(passageiros_transformados$DiferencaSazonal[-(1:12)], type = "drift")
summary(adf_sazonal)
kpss_sazonal <- ur.kpss(passageiros_transformados$DiferencaSazonal[-(1:12)])
summary(kpss_sazonal)

adf_duas_diferencas <- ur.df(passageiros_transformados$DuasDiferencas[-(1:13)], type = "drift")
summary(adf_duas_diferencas)
kpss_duas_diferencas <- ur.kpss(passageiros_transformados$DuasDiferencas[-(1:13)])
summary(kpss_duas_diferencas)

testes_passageiros <- data.frame(
  Serie = rep(c("ln(Nº mensal de passageiros em milhões)", "∇ ln(Nº mensal de passageiros em milhões)",
                "∇₁₂ ln(Nº mensal de passageiros em milhões)", "∇∇₁₂ ln(Nº mensal de passageiros em milhões)"), each = 2),
  Teste = rep(c("ADF", "KPSS"), 4),
  H0 = rep(c("Raiz unitária", "Estacionária em nível"), 4),
  Amostra = "Treino",
  Inicio = rep(treino$Data[c(1, 2, 13, 14)], each = 2),
  Fim = max(treino$Data),
  Observacoes = rep(c(nrow(treino), nrow(treino)-1, nrow(treino)-12, nrow(treino)-13), each = 2),
  Defasagens = c(adf_passageiros@lags, kpss_passageiros@lag,
                adf_diferenca@lags, kpss_diferenca@lag,
                adf_sazonal@lags, kpss_sazonal@lag,
                adf_duas_diferencas@lags, kpss_duas_diferencas@lag),
  Estatistica = c(adf_passageiros@teststat[1, "tau2"], kpss_passageiros@teststat,
                  adf_diferenca@teststat[1, "tau2"], kpss_diferenca@teststat,
                  adf_sazonal@teststat[1, "tau2"], kpss_sazonal@teststat,
                  adf_duas_diferencas@teststat[1, "tau2"], kpss_duas_diferencas@teststat),
  Critico5 = c(adf_passageiros@cval["tau2", "5pct"], kpss_passageiros@cval[1, "5pct"],
               adf_diferenca@cval["tau2", "5pct"], kpss_diferenca@cval[1, "5pct"],
               adf_sazonal@cval["tau2", "5pct"], kpss_sazonal@cval[1, "5pct"],
               adf_duas_diferencas@cval["tau2", "5pct"], kpss_duas_diferencas@cval[1, "5pct"])
)
testes_passageiros$RejeitaH0 <- ifelse(testes_passageiros$Teste == "ADF",
                                      testes_passageiros$Estatistica < testes_passageiros$Critico5,
                                      testes_passageiros$Estatistica > testes_passageiros$Critico5)
testes_passageiros

tabela_testes_passageiros <- testes_passageiros
tabela_testes_passageiros$Estatistica <- round(tabela_testes_passageiros$Estatistica, 4)
tabela_testes_passageiros$RejeitaH0 <- ifelse(tabela_testes_passageiros$RejeitaH0, "Sim", "Não")
names(tabela_testes_passageiros) <- c("Série", "Teste", "Hipótese nula", "Amostra", "Início", "Fim", "Observações",
                                     "Defasagens", "Estatística", "Valor crítico de 5%",
                                     "Rejeita H0 a 5%")

write_xlsx(tabela_testes_passageiros, "Resultados/[33] Estacionariedade do Nº de passageiros.xlsx")

fac_passageiros <- data.frame(
  Defasagem = 0:36,
  Autocorrelacao = calcular_fac_empirica(passageiros_transformados$LnPassageiros, 36),
  Limite = 1.96 / sqrt(nrow(treino))
)
facp_passageiros <- data.frame(
  Defasagem = 1:36,
  Autocorrelacao = as.numeric(pacf(passageiros_transformados$LnPassageiros,
                                  lag.max = 36, plot = FALSE)$acf),
  Limite = 1.96 / sqrt(nrow(treino))
)

fac_diferencas <- data.frame(
  Defasagem = 0:36,
  Autocorrelacao = calcular_fac_empirica(passageiros_transformados$DuasDiferencas[-(1:13)], 36),
  Limite = 1.96 / sqrt(nrow(treino)-13)
)
facp_diferencas <- data.frame(
  Defasagem = 1:36,
  Autocorrelacao = as.numeric(pacf(passageiros_transformados$DuasDiferencas[-(1:13)],
                                  lag.max = 36, plot = FALSE)$acf),
  Limite = 1.96 / sqrt(nrow(treino)-13)
)

grafico_fac_ln_passageiros <- ggplot(fac_passageiros[fac_passageiros$Defasagem > 0, ], aes(x = Defasagem)) +
  geom_hline(yintercept = 0, color = cor_principal, linewidth = 0.42) +
  geom_hline(yintercept = c(-fac_passageiros$Limite[1], fac_passageiros$Limite[1]),
             color = cor_acf, linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = Defasagem, y = 0, yend = Autocorrelacao),
               color = cor_principal, linewidth = 0.52) +
  scale_x_continuous(breaks = seq(0, max(fac_passageiros$Defasagem), by = 6)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Função de Autocorrelação de ln(Nº mensal de passageiros)",
       subtitle = "Treino (01/01/2002 a 31/10/2012)",
       x = "Defasagem (meses)", y = "Autocorrelação") +
  tema_graficos

ggsave(file.path("Resultados", "[34] FAC de ln(Nº mensal de passageiros).png"),
       plot = grafico_fac_ln_passageiros, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_facp_ln_passageiros <- ggplot(facp_passageiros, aes(x = Defasagem)) +
  geom_hline(yintercept = 0, color = cor_principal, linewidth = 0.42) +
  geom_hline(yintercept = c(-facp_passageiros$Limite[1], facp_passageiros$Limite[1]),
             color = cor_acf, linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = Defasagem, y = 0, yend = Autocorrelacao),
               color = cor_principal, linewidth = 0.52) +
  scale_x_continuous(breaks = seq(0, max(facp_passageiros$Defasagem), by = 6)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Função de Autocorrelação Parcial de ln(Nº mensal de passageiros)",
       subtitle = "Treino (01/01/2002 a 31/10/2012)",
       x = "Defasagem (meses)", y = "Autocorrelação parcial") +
  tema_graficos

ggsave(file.path("Resultados", "[35] FACP de ln(Nº mensal de passageiros).png"),
       plot = grafico_facp_ln_passageiros, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_fac_duas_diferencas_passageiros <- ggplot(fac_diferencas[fac_diferencas$Defasagem > 0, ], aes(x = Defasagem)) +
  geom_hline(yintercept = 0, color = cor_principal, linewidth = 0.42) +
  geom_hline(yintercept = c(-fac_diferencas$Limite[1], fac_diferencas$Limite[1]),
             color = cor_acf, linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = Defasagem, y = 0, yend = Autocorrelacao),
               color = cor_principal, linewidth = 0.52) +
  scale_x_continuous(breaks = seq(0, max(fac_diferencas$Defasagem), by = 6)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Função de Autocorrelação de ∇∇₁₂ ln(Nº mensal de passageiros)",
       subtitle = "Treino (01/01/2002 a 31/10/2012)",
       x = "Defasagem (meses)", y = "Autocorrelação") +
  tema_graficos

ggsave(file.path("Resultados", "[36] FAC das diferenciações regular e sazonal.png"),
       plot = grafico_fac_duas_diferencas_passageiros, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")

grafico_facp_duas_diferencas_passageiros <- ggplot(facp_diferencas, aes(x = Defasagem)) +
  geom_hline(yintercept = 0, color = cor_principal, linewidth = 0.42) +
  geom_hline(yintercept = c(-facp_diferencas$Limite[1], facp_diferencas$Limite[1]),
             color = cor_acf, linetype = "dashed", linewidth = 0.5) +
  geom_segment(aes(xend = Defasagem, y = 0, yend = Autocorrelacao),
               color = cor_principal, linewidth = 0.52) +
  scale_x_continuous(breaks = seq(0, max(facp_diferencas$Defasagem), by = 6)) +
  scale_y_continuous(labels = label_decimal) +
  labs(title = "Função de Autocorrelação Parcial de ∇∇₁₂ ln(Nº mensal de passageiros)",
       subtitle = "Treino (01/01/2002 a 31/10/2012)",
       x = "Defasagem (meses)", y = "Autocorrelação parcial") +
  tema_graficos

ggsave(file.path("Resultados", "[37] FACP das diferenciações regular e sazonal.png"),
       plot = grafico_facp_duas_diferencas_passageiros, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")
