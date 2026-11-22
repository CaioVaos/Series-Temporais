# LEITURA DOS DADOS

# Carregando dados
dados_diarios <- read.csv("Dados/GoogleTrends_Daily_2012-2026.csv")
dados_semanais <- read.csv("Dados/GoogleTrends_Weekly_2012-2026.csv")
dados_mensais <- read.csv("Dados/GoogleTrends_Monthly_2012-2026.csv")
dados_passageiros <- read.csv("Dados/BTS_T100_Passageiros_Monthly_2002-2025.csv")
# Ordens SARIMA por janela
ordens <- read.csv("Dados/OrdensSARIMA.csv")

dados_diarios$Data <- as.Date(dados_diarios$Data)
dados_semanais$Data <- as.Date(dados_semanais$Data)
dados_mensais$Data <- as.Date(dados_mensais$Data)
dados_passageiros$Data <- as.Date(dados_passageiros$Data)
ordens$Data <- as.Date(ordens$Data)

# Passageiros por segmento de voo, AS e VX na classe F
dados_passageiros$PassageirosMilhoes <- dados_passageiros$Passageiros / 1000000

# Datas das crises e períodos de avaliação
crises <- data.frame(
  Crise = c("Avião roubado", "Neve em Seattle", "Falta de pilotos",
            "Tempestade de inverno", "Voo 2059", "Voo 1282",
            "Sistema de peso", "Pane de julho", "Pane de outubro"),
  Noticia = as.Date(c("2018-08-10", "2021-12-26", "2022-04-01",
                      "2022-12-23", "2023-10-22", "2024-01-05",
                      "2024-04-17", "2025-07-20", "2025-10-23")),
  Grupo = c(rep("Validação", 6), rep("Teste", 3))
)
