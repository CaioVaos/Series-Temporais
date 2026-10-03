# Série temporal com tendência polinomial em R – Lista de 21/08/2026

Análise de Séries Temporais (GET00127)

## Objetivo

Simular uma série temporal com tendência polinomial
(X_t = β1 + β2·t + β3·t² + ε_t), suavizá-la com **média móvel centrada**,
calcular manualmente média, autocovariância e **função de autocorrelação
(FAC)** e mostrar os resultados em gráficos (ggplot2).

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_procedural.R` | 1 | script procedural completo, com documentação de entradas, saídas, estruturas, funções e fluxo |
| `config.R` | 2 | `n`, `sigma`, `q`, `max_lag`, semente, vetor `t`, vetor `beta`, matriz `X` |
| `dados.R` | 2 | ruído branco e série com tendência |
| `processamento.R` | 2 | média móvel, média amostral, autocovariância e FAC |
| `visualizacao.R` | 2 e 4 | gráficos com ggplot2 |
| `main.R` | 3 | pipeline: Configuração → Simulação → Média Móvel → FAC → Saída |
| `modelagem.md` | 3 | diagramas de arquitetura, fluxo de dados e sequência de chamadas |
| `eficiencia.R` | 4 | comparação loop x vetorização |

As interfaces (parâmetros, tipos, retornos, pré-condições) estão no cabeçalho
de cada arquivo `.R`.

## Como executar

Pré-requisito: R e os pacotes ggplot2 e gridExtra.

```r
install.packages(c("ggplot2", "gridExtra"))
```

Com a pasta do projeto como diretório de trabalho:

```
Rscript main.R            # pipeline completo (console + gráficos em graficos/)
Rscript eficiencia.R      # análise de eficiência
Rscript ex1_procedural.R  # versão do Exercício 1 (gráficos base do R)
```

No RStudio: `setwd("pasta_do_projeto")` e `source("main.R", encoding = "UTF-8")`.

Saída do `main.R`: média amostral, autocovariância de lag 1, FAC (lags 0 a 20)
e 3 PNGs em `graficos/` (série + média móvel, FAC e o gráfico integrado dos dois).

## Validação dos resultados

1. **Mesma semente, mesmos números:** `ex1_procedural.R` e `main.R` usam
   `set.seed(123)` e dão os mesmos resultados (média = 30.68494;
   autocovariância lag 1 = 588.9894).
2. **Contra o R:** o `main.R` compara a FAC manual com `acf()`. A maior
   diferença é ≈ 3e-16 (erro de ponto flutuante).
3. **Média móvel:** com n = 90 e q = 3 saem n − 2q = 84 valores; a janela é de
   7 pontos e a curva segue a tendência crescente da série.
4. **FAC:** a tendência faz a FAC ficar alta e decair devagar (lag 1 ≈ 0.73,
   lag 20 ≈ 0.29). Isso é esperado, pois a série não é estacionária.

## Análise de eficiência (Exercício 4.3)

**Gargalos dos loops `for`:**
- Na simulação, os loops aninhados fazem n × (número de colunas) multiplicações
  interpretadas uma a uma pelo R.
- Na autocovariância, cada lag percorre a série inteira (custo ≈ n × max_lag)
  e ainda recalcula a média (outro loop de n passos) a cada lag.

**Melhoria (em `eficiencia.R`):**
- `cbind(1, t, t^2)` no lugar do loop que monta `X`.
- `X %*% beta` no lugar dos loops aninhados: o produto de cada linha de `X`
  por `beta` é feito de uma vez. `rnorm(n)` sorteia o ruído inteiro de uma vez.
- `sum(d[1:(n-h)] * d[(h+1):n])` no lugar do loop da autocovariância. `d` guarda
  os desvios (X_t − X̄); as duas fatias são o mesmo vetor deslocado h posições,
  então o produto elemento a elemento seguido de `sum()` dá exatamente
  Σ (X_t − X̄)(X_{t+h} − X̄). Com h = 0 vira a variância, sem precisar do `if`.
- `mean(serie)` calcula a média uma única vez.
- O loop sobre os lags foi mantido (são só `max_lag + 1` passos).
- A média móvel não foi vetorizada: com q = 3 a janela tem só 7 pontos, e o
  teste mostrou que não compensa.

**Resultado** (n = 100000, max_lag = 50; tempos variam por máquina):

| Operação | Loops `for` | Vetorizada | Ganho | Maior diferença |
|---|---|---|---|---|
| Simulação da série | 0.57 s | 0.006 s | ≈ 90x | 0 |
| FAC | 0.56 s | 0.07 s | ≈ 8x | ≈ 1e-14 |

As diferenças são só arredondamento, então a matemática é preservada.
Com n = 90 não há diferença perceptível de tempo; o ganho só importa em
séries longas.
