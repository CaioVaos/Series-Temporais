# Figuras para o relatório, depois de executar Scripts/1_main.R
# Mantém eixos, legendas e as anotações dentro das figuras

setwd("C:/Users/Richard Amarante/Documents/AnaliseDeSeriesTemporais/Código do Projeto Final")

ggsave("Resultados/Versão sem título/[01] Interesse de busca diário.png",
       plot = grafico_interesse_diario + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[02] Interesse de busca semanal.png",
       plot = grafico_interesse_semanal + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[03] Interesse de busca mensal.png",
       plot = grafico_interesse_mensal + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[04] Nº de passageiros por mês.png",
       plot = grafico_passageiros + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[06] Interesse de busca por dia da semana.png",
       plot = grafico_dias_semana + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[07] Decomposição de ln(Interesse de busca mensal).png",
       plot = grafico_decomposicao_interesse + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[08] Decomposição mensal do interesse de busca após a pandemia.png",
       plot = grafico_decomposicao_depois + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[09] Decomposição mensal do interesse de busca - Série completa.png",
       plot = grafico_decomposicao_completa + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[10] Decomposição de ln(Nº mensal de passageiros).png",
       plot = grafico_decomposicao_passageiros + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[11] Interesse de busca vs Nº de passageiros.png",
       plot = grafico_comparacao + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[12] ln(Interesse de busca diário) antes do voo 1282.png",
       plot = grafico_log_interesse_voo1282 + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[13] FAC de ln(Interesse de busca diário).png",
       plot = grafico_fac_interesse_voo1282 + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[14] FACP de ln(Interesse de busca diário).png",
       plot = grafico_facp_interesse_voo1282 + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[16] Sistema de Alertas de Buscas Excessivas.png",
       plot = grafico_alertas + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[18] Voo 1282 - Previsão SARIMA.png",
       plot = grafico_normal_voo + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[24] Resíduos do SARIMA.png",
       plot = grafico_residuos_sarima + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[25] FAC dos resíduos do SARIMA.png",
       plot = grafico_fac_residuos_sarima + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[26] QQ-plot dos resíduos do SARIMA.png",
       plot = grafico_qq_residuos_sarima + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[19] Voo 1282 - Previsão AR(1) do excesso de interesse.png",
       plot = grafico_excesso_voo + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[20] Voo 1282 - SARIMA e AR(1).png",
       plot = grafico_etapas_voo + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[21] Voo 1282 - Previsões SARIMA e SARIMA + AR.png",
       plot = grafico_previsoes_voo + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[22] Voo 1282 - Previsão dois dias após o pico.png",
       plot = grafico_decisao_dia_dois + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[27] Resíduos do AR(1).png",
       plot = grafico_residuos_ar1 + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[28] FAC dos resíduos do AR(1).png",
       plot = grafico_fac_residuos_ar1 + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[29] QQ-plot dos resíduos do AR(1).png",
       plot = grafico_qq_residuos_ar1 + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[30] ln(Nº mensal de passageiros).png",
       plot = grafico_ln_passageiros + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[31] 1ª diferenciação de ln(Nº mensal de passageiros).png",
       plot = grafico_diferenca_passageiros + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[32] 2ª etapa de diferenciação de ln(Nº mensal de passageiros).png",
       plot = grafico_duas_diferencas_passageiros + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[34] FAC de ln(Nº mensal de passageiros).png",
       plot = grafico_fac_ln_passageiros + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[35] FACP de ln(Nº mensal de passageiros).png",
       plot = grafico_facp_ln_passageiros + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[36] FAC das diferenciações regular e sazonal.png",
       plot = grafico_fac_duas_diferencas_passageiros + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[37] FACP das diferenciações regular e sazonal.png",
       plot = grafico_facp_duas_diferencas_passageiros + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[38] Nº de passageiros - Previsões mensais do Airline.png",
       plot = grafico_airline_historico + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[39] Nº de passageiros - Teste do Airline.png",
       plot = grafico_airline_teste + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[41] Resíduos do Airline.png",
       plot = grafico_residuos_airline + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[42] FAC dos resíduos do Airline.png",
       plot = grafico_fac_residuos_airline + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")

ggsave("Resultados/Versão sem título/[43] QQ-plot dos resíduos do Airline.png",
       plot = grafico_qq_residuos_airline + labs(title = NULL, subtitle = NULL),
       dpi = parametros$dpi_grafico, width = parametros$largura_grafico,
       height = parametros$altura_grafico, units = "in", bg = "white")
