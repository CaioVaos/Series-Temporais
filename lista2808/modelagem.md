# Modelagem do sistema (Exercício 3)

Paradigma: **estritamente procedural**. Só existem variáveis, vetores,
matrizes, funções simples e chamadas em sequência. Não há classes nem objetos.

## 3.2 (a) Arquitetura modular

```
+------------------------------------------------------------+
|                         main.R                             |
|   (coordenador: chama tudo na ordem; único que usa source) |
+------------------------------------------------------------+
     |            |                 |                  |
     v            v                 v                  v
+-----------+ +--------------+ +------------------+ +----------------+
| config.R  | | dados.R      | | processamento.R  | | visualizacao.R |
|-----------| |--------------| |------------------| |----------------|
| set.seed  | | criar_matriz_| | calcular_media_  | | plot_serie_    |
| n_anos, s | |  tendencia   | |  movel           | |  ajustada      |
| n, sigma  | | criar_matriz_| | calcular_media_  | | plot_tendencia |
| t         | |  fourier     | |  amostral        | | plot_sazonali- |
| grau, K   | | criar_matriz_| | multiplicar_     | |  dade          |
| beta_*,   | |  dummies     | |  matriz_vetor    | | plot_residuos  |
| alpha,    | | simular_     | | calcular_XtX     | | plot_media_    |
| beta_h    | |  ruido_      | | calcular_Xty     | |  movel_tendencia|
| q, n_prev | |  branco      | | ajustar_ols      | |                |
| t_futuro  | | simular_serie| | ajustar_modelo   | | (ggplot2 +     |
|           | |              | | diagnostico      | |  gridExtra)    |
| (só       | |              | | prever           | |                |
| variáveis)| |              | |                  | |                |
+-----------+ +--------------+ +------------------+ +----------------+
```

| Arquivo | Responsabilidade única | Faz `source` de outro arquivo? |
|---|---|---|
| `config.R` | parâmetros, semente, tempo, coeficientes | não |
| `dados.R` | matrizes de modelo, ruído, série simulada | não |
| `processamento.R` | OLS, média móvel, diagnóstico, previsão | não |
| `visualizacao.R` | gráficos (ggplot2) | não |
| `main.R` | ordem de execução e impressão | sim, os 4 acima |

A estimação (`processamento.R`) não desenha nada e não constrói matrizes de
modelo: recebe `X_tendencia` e `X_sazonal` prontas por argumento. Os gráficos
(`visualizacao.R`) não calculam nada. Por isso os quatro módulos não dependem
uns dos outros.

## 3.2 (b) Fluxo de dados e matrizes

```
Configuração -> Simulação/Matrizes -> Estimação OLS
             -> Diagnóstico -> Previsão -> Visualização

config.R
  t (vetor n), grau, K, s, beta_tendencia, beta_sazonal, sigma, q, t_futuro
        |
        v
dados.R : criar_matriz_tendencia(t, grau) -> X_tendencia (n x 3)
          criar_matriz_fourier(t, s, K)   -> X_sazonal   (n x 2K)
          simular_serie(...)              -> Xt (vetor n)
        |
processamento.R : multiplicar_matriz_vetor -> tendencia_real, sazonalidade_real
        |
        v
processamento.R : ajustar_modelo(Xt, X_tendencia, X_sazonal)
   X_full = cbind(X_tendencia, X_sazonal)          (n x p)
   XtX = calcular_XtX(X_full)                      (p x p)
   Xty = calcular_Xty(X_full, Xt)                  (vetor p)
   beta_hat = solve(XtX, Xty)                      (vetor p)
   y_hat = X_full * beta_hat ; residuos = Xt - y_hat
        |
        v   modelo = lista(beta_hat, y_hat, tendencia, sazonalidade, residuos, X_full)
processamento.R : diagnostico(Xt, residuos) -> media, variancia, rmse, r2
        |
        v
dados.R : matrizes futuras X_tendencia_fut, X_sazonal_fut (t_futuro)
processamento.R : prever(modelo, X_tendencia_fut, X_sazonal_fut) -> previsoes
        |
        v
visualizacao.R : plot_*(vetores) -> objetos ggplot -> arrangeGrob -> ggsave
terminal       : cat() com coeficientes, diagnóstico e previsões
```

## 3.2 (c) Sequência procedural de chamadas

```
main.R         dados.R                    processamento.R                    visualizacao.R
  | source(...) (config.R define parâmetros e set.seed(42))
  |
  |-- criar_matriz_tendencia(t, grau) --> X_tendencia
  |-- criar_matriz_fourier(t, s, K) ----> X_sazonal
  |-- simular_serie(...) ---------------> Xt
  |       (simular_ruido_branco; loops i,j: tendência + sazonalidade + ruído)
  |
  |-- multiplicar_matriz_vetor(X_tendencia, beta_tendencia) -->| tendencia_real
  |-- multiplicar_matriz_vetor(X_sazonal, beta_sazonal) ------>| sazonalidade_real
  |
  |-- ajustar_modelo(Xt, X_tendencia, X_sazonal) -------------->|
  |        cbind -> ajustar_ols -> calcular_XtX, calcular_Xty, solve
  |        multiplicar_matriz_vetor (y_hat, tendência, sazonalidade)
  |        loop: residuos[i] = y[i] - y_hat[i]
  |<-------------------------------------------- modelo (lista) --|
  |-- calcular_media_movel(Xt, q) ----------------------------->| tendencia_media_movel
  |-- (modelo alternativo com criar_matriz_dummies + ajustar_modelo)
  |
  |-- diagnostico(Xt, modelo$residuos) ------------------------->| media, variancia, rmse, r2
  |-- validação: compara beta_hat com lm()
  |
  |-- criar_matriz_tendencia(t_futuro, grau) -> X_tendencia_fut
  |-- criar_matriz_fourier(t_futuro, s, K) ---> X_sazonal_fut
  |-- prever(modelo, X_tendencia_fut, X_sazonal_fut) ----------->| previsoes
  |-- compara previsoes com o valor verdadeiro sem ruído (RMSE)
  |
  |-- plot_serie_ajustada / plot_tendencia / plot_sazonalidade / plot_residuos ---->|
  |-- plot_media_movel_tendencia -------------------------------------------------->|
  |<------------------------------------------------------- objetos ggplot --------|
  |-- arrangeGrob + ggsave -> graficos/*.png
```

## 3.3 Por que é procedural

- Cada função recebe dados (vetores/matrizes) por argumento e devolve vetor,
  matriz ou lista; não guarda estado.
- O controle de fluxo é feito com `for` e `if`, em ordem, de cima para baixo no `main.R`.
