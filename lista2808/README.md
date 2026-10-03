# Decomposição clássica de séries temporais em R – Lista de 28/08/2026

Análise de Séries Temporais (GET00127)

## Objetivo

Simular uma série com **tendência polinomial** e **sazonalidade** (Fourier),
estimar o modelo por **mínimos quadrados ordinários (OLS)** com as equações
normais, fazer diagnóstico dos resíduos, **prever 12 períodos** à frente e
mostrar a decomposição aditiva (série, tendência, sazonalidade e resíduos)
em gráficos ggplot2.

Modelo: X_t = β0 + β1·t + β2·t² + Σ_k [α_k·cos(2πkt/s) + β_k·sen(2πkt/s)] + ε_t

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_procedural.R` | 1 | script procedural completo, com documentação, comentários dos loops de matriz/`solve()` e Fourier x dummies |
| `config.R` | 2 | parâmetros, semente, `t`, coeficientes |
| `dados.R` | 2 | matrizes de tendência, Fourier e dummies; ruído; série |
| `processamento.R` | 2 | OLS, produto matricial, média móvel, diagnóstico, previsão |
| `visualizacao.R` | 2 e 4 | gráficos com ggplot2 |
| `main.R` | 3 | pipeline: Config → Simulação/Matrizes → OLS → Diagnóstico → Previsão → Visualização |
| `modelagem.md` | 3 | arquitetura, fluxo de dados e sequência de chamadas |
| `eficiencia.R` | 4 | comparação loops `for` x `%*%` |

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

Saída do `main.R`: coeficientes (verdadeiro x estimado), diagnóstico
(Fourier x dummies), previsões e 2 PNGs em `graficos/` (decomposição em 4
painéis e série x média móvel x tendência OLS).

## Fourier x dummies

- **Fourier:** `a_k·cos(2πkt/s) + b_k·sen(2πkt/s)` para k = 1..K. Usa 2K
  colunas, dá um padrão suave e precisa de K < s/2.
- **Dummies:** uma coluna 0/1 por mês, menos o mês de referência (s − 1
  colunas). Cada mês tem efeito livre, mas com mais parâmetros. Não se usam
  as s colunas porque a soma delas é igual à coluna de 1s e X'X não teria inversa.

O `main.R` ajusta os dois e compara o diagnóstico.

## Validação dos resultados

1. **Mesma semente, mesmos números:** `ex1_procedural.R` e `main.R` usam
   `set.seed(42)` e dão os mesmos resultados (R² = 0.5979; RMSE = 5.1032).
2. **Contra o R:** os coeficientes do OLS manual foram comparados com `lm()`.
   A maior diferença é ≈ 3e-14 (arredondamento).
3. **Resíduos:** média ≈ 0, como esperado no OLS com intercepto. A variância
   (≈ 26.3) fica próxima de σ² = 25.
4. **Tendência e sazonalidade:** a tendência estimada acompanha a real. Os
   coeficientes sazonais saem mais distantes dos verdadeiros porque o ruído
   (σ = 5) é grande perto da amplitude sazonal (1 a 2).
5. **Previsão:** o RMSE contra o valor verdadeiro sem ruído é ≈ 0.88, bem
   abaixo do desvio do ruído (5).
6. **Fourier x dummies:** as dummies têm R² um pouco maior (0.6247 contra
   0.5979), porque usam mais parâmetros (11 contra 4).

## Análise de eficiência (Exercício 4.3)

**Gargalos dos loops `for`:**
- `calcular_XtX` usa 3 loops aninhados: custo ≈ p × p × n multiplicações.
  Cada uma é interpretada pelo R.
- `calcular_Xty` e `multiplicar_matriz_vetor` custam ≈ p × n.
- O custo cresce linearmente com n e com o quadrado do número de colunas.

**Melhoria (em `eficiencia.R`):**
- `t(X) %*% X` no lugar de `calcular_XtX` (`t()` é a transposta; `%*%` é o
  produto matricial, feito em código compilado).
- `t(X) %*% y` no lugar de `calcular_Xty`.
- `X %*% beta` no lugar de `multiplicar_matriz_vetor`.
- O `solve()` continua igual.
- Cada elemento continua sendo a mesma soma de produtos, então a matemática
  é preservada.

**Resultado** (n = 200000, 7 colunas; tempos variam por máquina):

| Operação | Loops `for` | `%*%` | Ganho |
|---|---|---|---|
| X'X | 0.68 s | 0.012 s | ≈ 57x |
| X'y | 0.08 s | 0.005 s | ≈ 16x |
| X %*% beta | 0.12 s | 0.003 s | ≈ 40x |

Os resultados são iguais (diferença relativa 0). Os coeficientes OLS (n = 120)
são idênticos entre loops e `%*%`, e diferem de `lm()` em ≈ 3e-14. Com n = 120
o tempo é imperceptível; o ganho só importa em séries longas.

**Por que ggplot2 (Exercício 4.2):** cada componente (série, tendência,
sazonalidade, resíduo) vira um gráfico com a mesma estética, e dá para empilhar
os painéis com o mesmo eixo de tempo. A legenda sai automática das cores
associadas a cada linha, e a exportação em alta resolução é uma linha
(`ggsave(..., dpi = 200)`).
