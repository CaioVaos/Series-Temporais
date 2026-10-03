# Modelagem do sistema (Exercício 3)

Paradigma: **estritamente procedural**. Só existem variáveis, vetores,
funções simples e chamadas em sequência. Não há classes nem objetos.

## 3.2 (a) Arquitetura dos módulos

```
+------------------------------------------------------------+
|                         main.R                             |
|   (coordenador: chama tudo na ordem; única que usa source) |
+------------------------------------------------------------+
     |            |                |                  |
     v            v                v                  v
+----------+ +----------+ +----------------+ +----------------+
| config.R | | dados.R  | | processamento.R| | visualizacao.R |
|----------| |----------| |----------------| |----------------|
| n        | | simular_ | | calcular_media_| | plot_serie_    |
| max_lag  | | ruido_   | |   empirica     | |   temporal     |
| sigma2   | |  branco  | | calcular_auto- | | plot_fac_      |
| phi      | | simular_ | |   covariancia_ | |   empirica     |
| set.seed | |  passeio_| |   empirica     | | plot_fac_      |
|          | |  aleat.  | | calcular_auto- | |   comparacao   |
| (só      | | simular_ | |   covariancias | |                |
| variáveis)| |  ar1    | | calcular_fac_  | | (usa ggplot2)  |
|          | |          | |   empirica     | |                |
|          | |          | | calcular_fac_  | |                |
|          | |          | |   teorica_ar1  | |                |
+----------+ +----------+ +----------------+ +----------------+
```

| Arquivo | Responsabilidade única | Faz `source` de outro arquivo? |
|---|---|---|
| `config.R` | parâmetros e semente | não |
| `dados.R` | simular séries | não |
| `processamento.R` | cálculos estatísticos | não |
| `visualizacao.R` | gráficos (ggplot2) | não |
| `main.R` | ordem de execução e impressão | sim, os 4 acima |

## 3.2 (b) Dependências e fluxo de dados

Dependência de código: apenas `main.R -> {config, dados, processamento, visualizacao}`.
Os quatro módulos **não dependem uns dos outros** (sem acoplamento).
Os dados circulam por argumentos e retornos de função, passando pelo `main.R`:

```
 config.R ──(n, sigma2, phi)──────────────┐
                                          v
                                   +-------------+
                                   |   main.R    |
                                   +-------------+
 dados.R:   simular_*(n, sigma2, phi) ──> eps, X, X_ar1 (vetores)
                                          |
 processamento.R: média, FAC (série, max_lag) ──> media_*, fac_*, fac_ar1_teorica
                                          |
 main.R (análise): |fac_ar1 - fac_ar1_teorica| e comparação com acf() do R
                                          |
 visualizacao.R: plot_*(vetores) ──> objetos ggplot ──> ggsave -> graficos/*.png
                                          |
 terminal: cat() com médias, tabela de FAC e diferenças
```

Pipeline linear:

```
Configuração -> Simulação de Séries -> Cálculo Estatístico
             -> Análise e Comparação -> Exibição de Resultados
 (config.R)      (dados.R)            (processamento.R)   (main.R)   (main.R + visualizacao.R)
```

## 3.2 (c) Sequência temporal de chamadas (AR(1))

```
main.R                config.R   dados.R            processamento.R             visualizacao.R
  |  source(...)         |          |                     |                           |
  |--------------------->| define n, max_lag, sigma2, phi, set.seed(123)             |
  |                                                                                   |
  |-- simular_ar1(n, phi, sigma2) --> |                                               |
  |                       simular_ruido_branco(n, sigma2)  (rnorm em loop)            |
  |                       loop t = 2..n: X[t] = phi*X[t-1] + eps[t]                   |
  |<-------------------- X_ar1 (vetor n) -----|                                       |
  |                                                                                   |
  |-- calcular_media_empirica(X_ar1) ------------------>|                             |
  |<------------------------ media_ar1 ----------------|                             |
  |-- calcular_fac_empirica(X_ar1, max_lag) ----------->|                             |
  |                         calcular_autocovariancias -> calcular_autocovariancia_    |
  |                         empirica (h = 0..max_lag) -> calcular_media_empirica      |
  |<------------------------ fac_ar1 (vetor max_lag+1) -|                             |
  |-- calcular_fac_teorica_ar1(phi, max_lag) ---------->|                             |
  |<------------------------ fac_ar1_teorica ----------|                             |
  |                                                                                   |
  |-- loop h: |fac_ar1 - fac_ar1_teorica| ; compara com acf() do R                    |
  |-- cat(...) imprime resultados                                                     |
  |-- plot_serie_temporal(X_ar1, ...) ----------------------------------------------->|
  |-- plot_fac_comparacao(fac_ar1, fac_ar1_teorica, ...) ---------------------------->|
  |<----------------------------- objetos ggplot ------------------------------------|
  |-- ggsave(...) salva os PNG em graficos/                                           |
```

## 3.3 Por que é procedural

- Cada função recebe dados por argumento e devolve um vetor/número; não guarda estado.
- Os dados são só vetores numéricos.
- O controle de fluxo é feito com `for` e `if`, em ordem, de cima para baixo no `main.R`.