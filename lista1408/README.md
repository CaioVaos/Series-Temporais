# Processos estocásticos em R – Lista de 14/08/2026

Análise de Séries Temporais (GET00127)

## Objetivo

Simular três processos estocásticos (**ruído branco**, **passeio aleatório**
e **AR(1)**), calcular manualmente média, autocovariância e função de
autocorrelação (FAC), comparar a FAC empírica com a teórica do AR(1) e
mostrar os resultados em gráficos (ggplot2).

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_procedural.R` | 1 | script procedural completo, com documentação de entradas, saídas, estruturas, funções e fluxo |
| `config.R` | 2 | parâmetros: `n`, `max_lag`, `sigma2`, `phi`, semente |
| `dados.R` | 2 | simulação: ruído branco, passeio aleatório, AR(1) |
| `processamento.R` | 2 | média, autocovariância, FAC empírica e teórica |
| `visualizacao.R` | 2 e 4 | gráficos com ggplot2 |
| `main.R` | 3 | pipeline: Configuração → Simulação → Cálculo → Análise → Exibição |
| `modelagem.md` | 3 | diagramas de arquitetura, dependências e sequência de chamadas |
| `eficiencia.R` | 4 | comparação loop x vetorização |

As interfaces (parâmetros, retornos, pré-condições) estão documentadas no
cabeçalho de cada arquivo `.R`.

## Como executar

Pré-requisito: R e o pacote ggplot2.

```r
install.packages("ggplot2")
```

Com a pasta do projeto como diretório de trabalho:

```
Rscript main.R          # pipeline completo (terminal + gráficos em graficos/)
Rscript eficiencia.R    # análise de eficiência
Rscript ex1_procedural.R  # versão do Exercício 1 (gráficos base do R)
```

No RStudio: `setwd("pasta_do_projeto")` e `source("main.R", encoding = "UTF-8")`.

Saída do `main.R`: médias empíricas, tabela da FAC do AR(1) (empírica x
teórica x diferença) e 6 PNGs em `graficos/`.

## Validação dos resultados

1. **Mesma semente, mesmos números:** `ex1_procedural.R` e `main.R` usam
   `set.seed(123)` e a mesma ordem de simulação, então dão as mesmas médias
   (ruído branco = -0.00857; passeio = 7.26278; AR(1) = 0.08951).
2. **Contra o R:** o `main.R` compara a FAC manual com `acf()`. A maior
   diferença obtida foi ≈ 2e-16 (só erro numérico de ponto flutuante).
3. **Contra a teoria:** no AR(1) com φ = 0.7, a FAC empírica decai como φ^h
   (lag 1: 0.619 contra 0.7; lag 2: 0.396 contra 0.49). A diferença vem da
   variação amostral com n = 200.
4. **Comportamento esperado:** ruído branco tem FAC ≈ 0 fora do lag 0;
   passeio aleatório tem FAC alta e decaindo devagar (não estacionário);
   AR(1) tem decaimento geométrico.

## Análise de eficiência (Exercício 4.3)

**Gargalos dos loops `for`:**
- `calcular_autocovariancia_empirica` percorre os n valores uma vez por lag
  (custo ≈ n × max_lag), com cada iteração interpretada pelo R.
- Ela chama `calcular_media_empirica` (outro loop de n passos) a cada lag, ou
  seja, a média é recalculada `max_lag + 1` vezes sem necessidade.
- O passeio aleatório também usa um loop para acumular os choques.

**Melhoria (em `eficiencia.R`):**
- `mean(serie)` no lugar do loop da média, calculada uma vez só.
- `sum(d[1:(n-h)] * d[(h+1):n])` no lugar do loop da autocovariância. O vetor
  `d` guarda os desvios; `d[1:(n-h)]` e `d[(h+1):n]` são o mesmo vetor
  deslocado h posições, então o produto elemento a elemento seguido de `sum()`
  dá exatamente Σ (X_t − X̄)(X_{t+h} − X̄). Com h = 0 a fórmula vira a
  variância, então o `if` deixa de ser necessário.
- `cumsum(eps)` no lugar do loop do passeio, pois X_t = ε_1 + … + ε_t.
- O AR(1) **continua com loop**, porque cada X_t depende do X_{t-1} já
  calculado e não dá para vetorizar com funções simples.
- O loop sobre os lags (no máximo `max_lag + 1` passos) foi mantido por ser curto.

**Resultado** (n = 100000, max_lag = 50; tempos variam por máquina):

| Operação | Loop `for` | Vetorizada | Ganho | Maior diferença |
|---|---|---|---|---|
| FAC | 0.38 s | 0.06 s | ≈ 7x | ≈ 1e-14 |
| Passeio aleatório | 0.16 s | 0.004 s | ≈ 40x | ≈ 1e-12 |

As diferenças são apenas arredondamento, então a matemática é preservada.
No tamanho original (n = 200) a diferença de tempo é imperceptível; o ganho
só importa em séries longas.