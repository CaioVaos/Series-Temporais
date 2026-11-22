# ANÁLISE DESCRITIVA DO EXCESSO DE INTERESSE

dia0_voo <- normal_voo$Data[which(normal_voo$Data >= noticia_voo &
                                  normal_voo$Observado > normal_voo$Superior)[1]]
decisao_voo <- dia0_voo + parametros$dia_previsao
excesso_ajuste <- normal_voo[normal_voo$Data >= noticia_voo - parametros$janela_excesso &
                             normal_voo$Data <= decisao_voo, c("Data", "Excesso")]

# ESTACIONARIEDADE DO EXCESSO DE INTERESSE

adf_excesso <- ur.df(excesso_ajuste$Excesso, type = "drift")
summary(adf_excesso)
kpss_excesso <- ur.kpss(excesso_ajuste$Excesso)
summary(kpss_excesso)

testes_excesso <- data.frame(
  Serie = "Excesso de interesse",
  Teste = c("ADF", "KPSS"),
  H0 = c("Raiz unitária", "Estacionária em nível"),
  Inicio = min(excesso_ajuste$Data),
  Fim = max(excesso_ajuste$Data),
  Observacoes = nrow(excesso_ajuste),
  Defasagens = c(adf_excesso@lags, kpss_excesso@lag),
  Estatistica = c(adf_excesso@teststat[1, "tau2"], kpss_excesso@teststat),
  Critico5 = c(adf_excesso@cval["tau2", "5pct"], kpss_excesso@cval[1, "5pct"])
)
testes_busca <- rbind(testes_busca, testes_excesso)
testes_busca$RejeitaH0 <- ifelse(testes_busca$Teste == "ADF",
                                testes_busca$Estatistica < testes_busca$Critico5,
                                testes_busca$Estatistica > testes_busca$Critico5)
testes_busca

tabela_testes_busca <- testes_busca
tabela_testes_busca$Estatistica <- round(tabela_testes_busca$Estatistica, 4)
tabela_testes_busca$RejeitaH0 <- ifelse(tabela_testes_busca$RejeitaH0, "Sim", "Não")
names(tabela_testes_busca) <- c("Série", "Teste", "Hipótese nula", "Início", "Fim", "Observações",
                               "Defasagens", "Estatística", "Valor crítico de 5%",
                               "Rejeita H0 a 5%")

write_xlsx(tabela_testes_busca, "Resultados/[15] Estacionariedade do interesse de busca.xlsx")
