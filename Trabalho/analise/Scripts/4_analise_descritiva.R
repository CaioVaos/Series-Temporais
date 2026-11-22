# ANÁLISE DESCRITIVA

# ESTATÍSTICAS DESCRITIVAS

resumo <- data.frame(
  Serie = c("Interesse de busca diário", "Interesse de busca semanal",
            "Interesse de busca mensal", "Nº de passageiros (milhões)"),
  Inicio = c(min(dados_diarios$Data), min(dados_semanais$Data),
             min(dados_mensais$Data), min(dados_passageiros$Data)),
  Fim = c(max(dados_diarios$Data), max(dados_semanais$Data),
          max(dados_mensais$Data), max(dados_passageiros$Data)),
  Observacoes = c(nrow(dados_diarios), nrow(dados_semanais),
                  nrow(dados_mensais), nrow(dados_passageiros)),
  Media = c(calcular_media_amostral(dados_diarios$InteresseWeb),
            calcular_media_amostral(dados_semanais$InteresseWeb),
            calcular_media_amostral(dados_mensais$InteresseWeb),
            calcular_media_amostral(dados_passageiros$PassageirosMilhoes)),
  Desvio = sqrt(c(calcular_variancia_amostral(dados_diarios$InteresseWeb),
                  calcular_variancia_amostral(dados_semanais$InteresseWeb),
                  calcular_variancia_amostral(dados_mensais$InteresseWeb),
                  calcular_variancia_amostral(dados_passageiros$PassageirosMilhoes))),
  Minimo = c(min(dados_diarios$InteresseWeb), min(dados_semanais$InteresseWeb),
             min(dados_mensais$InteresseWeb), min(dados_passageiros$PassageirosMilhoes)),
  Maximo = c(max(dados_diarios$InteresseWeb), max(dados_semanais$InteresseWeb),
             max(dados_mensais$InteresseWeb), max(dados_passageiros$PassageirosMilhoes))
)

resumo

# AUTOCOVARIÂNCIA E FAC DAS SÉRIES

autocovariancias_diarias <- data.frame(Defasagem = 0:14, Autocovariancia = 0)

for(h in 0:14){
  autocovariancias_diarias$Autocovariancia[h+1] <-
    calcular_autocovariancia_empirica(dados_diarios$InteresseWeb, h)
}

autocovariancias_diarias$Autocorrelacao <-
  autocovariancias_diarias$Autocovariancia / autocovariancias_diarias$Autocovariancia[1]
autocovariancias_diarias

autocovariancias_semanais <- data.frame(Defasagem = 0:12, Autocovariancia = 0)

for(h in 0:12){
  autocovariancias_semanais$Autocovariancia[h+1] <-
    calcular_autocovariancia_empirica(dados_semanais$InteresseWeb, h)
}

autocovariancias_semanais$Autocorrelacao <-
  autocovariancias_semanais$Autocovariancia / autocovariancias_semanais$Autocovariancia[1]
autocovariancias_semanais

autocovariancias_mensais <- data.frame(Defasagem = 0:24, Autocovariancia = 0)

for(h in 0:24){
  autocovariancias_mensais$Autocovariancia[h+1] <-
    calcular_autocovariancia_empirica(dados_mensais$InteresseWeb, h)
}

autocovariancias_mensais$Autocorrelacao <-
  autocovariancias_mensais$Autocovariancia / autocovariancias_mensais$Autocovariancia[1]
autocovariancias_mensais

autocovariancias_passageiros <- data.frame(Defasagem = 0:24, Autocovariancia = 0)

for(h in 0:24){
  autocovariancias_passageiros$Autocovariancia[h+1] <-
    calcular_autocovariancia_empirica(dados_passageiros$PassageirosMilhoes, h)
}

autocovariancias_passageiros$Autocorrelacao <-
  autocovariancias_passageiros$Autocovariancia / autocovariancias_passageiros$Autocovariancia[1]
autocovariancias_passageiros

# Sete dias para reduzir a oscilação dentro da semana
dados_diarios$MediaMovel <- calcular_media_movel(dados_diarios$InteresseWeb,
                                               parametros$media_movel_dias)

# Três meses para suavizar a variação curta sem apagar o ciclo anual
dados_passageiros$MediaMovel <- calcular_media_movel(dados_passageiros$PassageirosMilhoes,
                                                   parametros$media_movel_meses)

tabela_descritiva <- resumo
tabela_descritiva[, c("Media", "Desvio", "Minimo", "Maximo")] <-
  round(tabela_descritiva[, c("Media", "Desvio", "Minimo", "Maximo")], 3)
names(tabela_descritiva) <- c("Série", "Início", "Fim", "Observações", "Média",
                             "Desvio padrão", "Mínimo", "Máximo")

write_xlsx(tabela_descritiva, "Resultados/[05] Estatísticas descritivas.xlsx")

# PALETA DE CORES

cor_principal <- "#37474F"
cor_referencia <- "#C44E52"
cor_trajetorias <- "#B0B0B0"
cor_acf <- "#4C78A8"
cor_covariancia <- "#3F7F8F"

# TEMA DOS GRÁFICOS

tema_graficos <- theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    plot.subtitle = element_text(size = 13, hjust = 0.5,
                                 margin = margin(b = 10)),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    axis.line = element_line(linewidth = 0.55),
    axis.ticks = element_line(linewidth = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

label_decimal <- scales::label_number(decimal.mark = ",", big.mark = ".")

# COMPARAÇÃO NO PERÍODO COMUM

comparacao_mensal <- merge(dados_mensais[, c("Data", "InteresseWeb")],
                          dados_passageiros[, c("Data", "PassageirosMilhoes")],
                          by = "Data")

# Usando o mínimo e o máximo de cada série no período comum
comparacao_mensal$InteressePadronizado <-
  (comparacao_mensal$InteresseWeb - min(comparacao_mensal$InteresseWeb)) /
  (max(comparacao_mensal$InteresseWeb) - min(comparacao_mensal$InteresseWeb))

comparacao_mensal$PassageirosPadronizados <-
  (comparacao_mensal$PassageirosMilhoes - min(comparacao_mensal$PassageirosMilhoes)) /
  (max(comparacao_mensal$PassageirosMilhoes) - min(comparacao_mensal$PassageirosMilhoes))

grafico_comparacao <- ggplot(comparacao_mensal, aes(x = Data)) +
  geom_line(aes(y = InteressePadronizado, color = "Interesse de busca"),
            linewidth = 0.65) +
  geom_line(aes(y = PassageirosPadronizados, color = "Nº de passageiros"),
            linewidth = 0.65) +
  scale_color_manual(values = c("Interesse de busca" = cor_principal,
                                "Nº de passageiros" = cor_acf)) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(comparacao_mensal$Data), max(comparacao_mensal$Data), by = "1 year"),
               expand = expansion(mult = 0.015)) +
  scale_y_continuous(labels = label_decimal) +
  labs(
    title = "Interesse de busca vs Nº de passageiros",
    x = "Ano", y = "Valor padronizado"
  ) +
  tema_graficos +
  theme(axis.text.x = element_text(size = 10))

ggsave(file.path("Resultados", "[11] Interesse de busca vs Nº de passageiros.png"),
       plot = grafico_comparacao, dpi = parametros$dpi_grafico,
       width = parametros$largura_grafico, height = parametros$altura_grafico,
       units = "in", bg = "white")
