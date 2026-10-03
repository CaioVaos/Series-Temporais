# Modelagem do sistema (Exercício 3)

Paradigma: **estritamente procedural**. Só existem variáveis, vetores,
matrizes, funções simples e chamadas em sequência. Não há classes nem objetos.

## 3.2 (a) Arquitetura modular

```
+------------------------------------------------------------+
|                         main.R                             |
|   (coordenador: chama tudo na ordem; único que usa source) |
+------------------------------------------------------------+
     |            |                |                  |
     v            v                v                  v
+-----------+ +-----------+ +------------------+ +----------------+
| config.R  | | dados.R   | | processamento.R  | | visualizacao.R |
|-----------| |-----------| |------------------| |----------------|
| set.seed  | | simular_  | | calcular_media_  | | plot_serie_    |
| n, sigma  | |  ruido_   | |   movel          | |  media_movel   |
| q,max_lag | |  branco   | | calcular_media_  | | plot_fac_      |
| t, beta   | | simular_  | |   amostral       | |  empirica      |
| X (matriz)| |  serie_   | | calcular_auto-   | |                |
|           | |  com_     | |  covariancia_    | | (ggplot2 +     |
| (só       | |  tendencia| |  empirica        | |  gridExtra)    |
| variáveis)| |           | | calcular_auto-   | |                |
|           | |           | |  covariancias    | |                |
|           | |           | | calcular_fac_    | |                |
|           | |           | |  empirica        | |                |
+-----------+ +-----------+ +------------------+ +----------------+
```

| Arquivo | Responsabilidade única | Faz `source` de outro arquivo? |
|---|---|---|
| `config.R` | parâmetros, semente, t, beta, matriz X | não |
| `dados.R` | simular ruído e série com tendência | não |
| `processamento.R` | média móvel, média, autocovariância, FAC | não |
| `visualizacao.R` | gráficos (ggplot2) | não |
| `main.R` | ordem de execução e impressão | sim, os 4 acima |

Os cálculos (`processamento.R`) não desenham nada, e os gráficos
(`visualizacao.R`) não calculam nada. Nenhum código está duplicado entre módulos.

## 3.2 (b) Fluxo de dados

Dependência de código: apenas `main.R -> {config, dados, processamento, visualizacao}`.
Os quatro módulos **não dependem uns dos outros**. Os dados circulam por
argumentos e retornos, passando pelo `main.R`:

```
Configuração -> Simulação da Série -> Transformação (Média Móvel)
             -> Análise Estatística (FAC) -> Visualização e Saída

config.R
  n, sigma, q, max_lag, t (vetor), beta (vetor), X (matriz n x 3)
        |
        v
dados.R : simular_serie_com_tendencia(beta, X, sigma)
        |   (usa simular_ruido_branco(n, sigma) -> eps, vetor n)
        v
      Xt  (vetor de tamanho n)
        |
        +---------------------------+
        v                           v
processamento.R:               processamento.R:
calcular_media_movel(Xt, q)    calcular_media_amostral(Xt)
        |                      calcular_autocovariancia_empirica(Xt, 1)
        v                      calcular_fac_empirica(Xt, max_lag)
 m_q (vetor n - 2q)                 |
        |                           v
        |                  media_Xt, gamma_1, fac_Xt (vetor max_lag + 1)
        |                           |
        +-------------+-------------+
                      v
main.R: compara fac_Xt com acf() do R (maior diferença) e imprime no console
                      |
                      v
visualizacao.R: plot_serie_media_movel(t, Xt, m_q, q)  -> g1
                plot_fac_empirica(fac_Xt, ...)         -> g2
                      |
                      v
main.R: arrangeGrob(g1, g2) + ggsave -> graficos/*.png
```

## 3.2 (c) Sequência procedural de chamadas

```
main.R            config.R   dados.R                    processamento.R                   visualizacao.R
  | source(...)      |          |                            |                                 |
  |----------------->| define n, sigma, q, max_lag, t, beta, X, set.seed(123)                    |
  |                                                                                              |
  |-- simular_serie_com_tendencia(beta, X, sigma) --> |                                          |
  |                    simular_ruido_branco(n, sigma)   (rnorm em loop)                          |
  |                    loop t, loop j: valor += X[t,j]*beta[j]; Xt[t] = valor + eps[t]          |
  |<----------------- Xt (vetor n) ------------------|                                          |
  |                                                                                              |
  |-- calcular_media_movel(Xt, q) ---------------------------------->|                           |
  |                         loop t = q+1..n-q; loop j = t-q..t+q (soma / (2q+1))                 |
  |<------------------------------------------ m_q (vetor n-2q) ----|                           |
  |                                                                                              |
  |-- calcular_media_amostral(Xt) ---------------------------------->|                           |
  |<------------------------------------------ media_Xt ------------|                           |
  |-- calcular_autocovariancia_empirica(Xt, 1) --------------------->|                           |
  |<------------------------------------------ gamma_1 -------------|                           |
  |-- calcular_fac_empirica(Xt, max_lag) --------------------------->|                           |
  |                         calcular_autocovariancias -> calcular_autocovariancia_empirica       |
  |                         (h = 0..max_lag) -> calcular_media_amostral                         |
  |<------------------------------------------ fac_Xt (vetor) ------|                           |
  |                                                                                              |
  |-- loop h: |fac_Xt - acf(R)| ; cat(...) imprime no console                                    |
  |-- plot_serie_media_movel(t, Xt, m_q, q) ---------------------------------------------------->|
  |-- plot_fac_empirica(fac_Xt, ...) ----------------------------------------------------------->|
  |<----------------------------------------- objetos ggplot ------------------------------------|
  |-- arrangeGrob + ggsave -> graficos/*.png                                                     |
```

## 3.3 Por que é procedural

- Cada função recebe dados por argumento e devolve vetor/número; não guarda estado.
- Os dados são só vetores e uma matriz numéricos.
- O controle de fluxo é feito com `for` e `if`, em ordem, de cima para baixo no `main.R`.
