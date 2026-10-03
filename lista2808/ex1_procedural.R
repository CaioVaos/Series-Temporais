############################################################
# EXERCÍCIO 1 – IMPLEMENTAÇÃO PROCEDURAL
# Decomposição clássica: tendência polinomial + sazonalidade + OLS
############################################################
#
# DOCUMENTAÇÃO DO SISTEMA
# ----------------------------------------------------------
# (a) ENTRADAS E PARÂMETROS GLOBAIS (seção 1 e seção 12)
#     n_anos = 10  -> número de anos
#     s      = 12  -> período sazonal (meses por ano)
#     n      = n_anos * s = 120 -> tamanho da série
#     sigma  = 5   -> desvio padrão do ruído branco
#     q      = 5   -> raio da média móvel centrada
#     K      = 2   -> número de harmônicos de Fourier
#     grau_tendencia = 2 -> polinômio de grau 2
#     beta_tendencia = c(2, 0.05, 0.001)  (const., t, t^2)
#     alpha  = c(2, 1)   -> coeficientes dos cossenos
#     beta_h = c(-1, 0.5)-> coeficientes dos senos
#     beta_sazonal       -> alpha e beta_h intercalados
#                           (cos1, sen1, cos2, sen2)
#     t      = 1:n -> vetor temporal
#     semente = 42
#
# (b) SAÍDAS
#     Matrizes de modelo : X_tendencia, X_sazonal, X_full
#     Série simulada     : Xt
#     Componentes reais  : tendencia_real, sazonalidade_real
#     Estimados (modelo) : beta_hat, y_hat, tendencia, sazonalidade,
#                          residuos
#     Diagnóstico        : média e variância dos resíduos, RMSE, R²
#     Média móvel        : tendencia_media_movel
#     Previsões          : previsoes (12 períodos à frente)
#     Gráficos           : observada x ajustada, tendência,
#                          sazonalidade, resíduos, média móvel x OLS
#
# (c) ESTRUTURAS DE DADOS
#     Vetores : t, beta_*, eps, Xt, y_hat, residuos, previsoes
#     Matrizes: X_tendencia (n x 3), X_sazonal (n x 2K),
#               X_full (n x (3+2K)), XtX (p x p), Xty (vetor p)
#
# (d) FUNÇÕES
#     Matrizes   : criar_matriz_tendencia, criar_matriz_fourier,
#                  criar_matriz_dummies
#     Simulação  : simular_ruido_branco, simular_serie
#     Filtro     : calcular_media_movel
#     Estatística: calcular_media_amostral
#     OLS        : multiplicar_matriz_vetor, calcular_XtX,
#                  calcular_Xty, ajustar_ols, ajustar_modelo
#     Diagnóstico: diagnostico
#     Previsão   : prever
#     Gráficos   : plot_serie_ajustada, plot_tendencia,
#                  plot_sazonalidade, plot_residuos
#
# (e) FLUXO DE EXECUÇÃO
#     1. Semente e parâmetros
#     2. Definição das funções
#     3. Vetor t, beta, matrizes X_tendencia e X_sazonal
#     4. Simulação de Xt e componentes reais
#     5. Média móvel
#     6. Ajuste OLS (monta X_full, XtX, Xty, solve)
#     7. Diagnóstico e coeficientes no console
#     8. Gráficos (4 painéis + média móvel x tendência OLS)
#     9. Previsão dos próximos 12 períodos
#
# SAZONALIDADE: FOURIER x DUMMIES
# ----------------------------------------------------------
# Modelo: X_t = tendência(t) + sazonalidade(t) + eps_t
# com sazonalidade de período s (aqui s = 12).
#
# FOURIER
#   sazonalidade(t) = soma_{k=1}^{K} [ a_k*cos(2*pi*k*t/s)
#                                    + b_k*sen(2*pi*k*t/s) ]
#   - cada harmônico k é uma onda que se repete s/k vezes por ano;
#   - computacionalmente: 2 colunas (cos e sen) por harmônico,
#     ou seja 2K colunas em X_sazonal;
#   - poucos parâmetros (2K) e padrão suave; K deve ser < s/2
#     (com K = s/2 o seno vira coluna de zeros);
#   - funciona para qualquer s (inclusive períodos longos).
#
# DUMMIES
#   sazonalidade(t) = soma_{m=2}^{s} g_m * D_m(t)
#   onde D_m(t) = 1 se o instante t cai no "mês" m, e 0 caso contrário.
#   - mês = ((t-1) %% s) + 1 ; o mês 1 é a referência (fica no
#     intercepto), por isso são s-1 colunas (evita a "armadilha da
#     dummy": se usasse as s colunas, a soma delas seria igual à
#     coluna de 1s e X'X não teria inversa);
#   - cada mês tem um efeito livre (s-1 parâmetros): padrão mais
#     flexível, porém com mais parâmetros;
#   - com K = s/2 (sem a coluna de seno nula), Fourier gera o mesmo
#     espaço de colunas que as dummies: mesmo ajuste.
############################################################

############################################################
# AULA PRÁTICA – DECOMPOSIÇÃO CLÁSSICA DE SÉRIES TEMPORAIS
# TENDÊNCIA E SAZONALIDADE
############################################################

rm(list = ls())

set.seed(42)

############################################################
# 1. PARÂMETROS GERAIS
############################################################

n_anos <- 10

s <- 12

n <- n_anos * s

sigma <- 5

############################################################
# 2. FUNÇÕES DE CONSTRUÇÃO DAS MATRIZES
############################################################

############################################################
# MATRIZ DE TENDÊNCIA POLINOMIAL
############################################################

criar_matriz_tendencia <- function(t, grau){
  
  # LOOPS ANINHADOS: monta a matriz de tendência (n x (grau+1)).
  # - loop de fora (i): percorre os instantes de tempo (linhas);
  # - loop de dentro (j): percorre as potências 0, 1, ..., grau;
  # - X[i, j+1] = t_i^j  (coluna 1 = 1, coluna 2 = t, coluna 3 = t^2).
  # Usamos j+1 porque o R indexa a partir de 1 e a potência começa em 0.
  
  n <- length(t)
  
  numero_colunas <- grau + 1
  
  X <- matrix(0,nrow = n,ncol = numero_colunas)
  
  for(i in 1:n){
    
    for(j in 0:grau){
      X[i,j+1] <- t[i]^j
    }
  }
  return(X)
}

############################################################
# MATRIZ DE FOURIER
############################################################

criar_matriz_fourier <- function(t, periodo, K){
  
  # LOOPS ANINHADOS: para cada instante i e cada harmônico k,
  # preenche duas colunas: cos(2*pi*k*t/periodo) na coluna 2k-1 e
  # sen(2*pi*k*t/periodo) na coluna 2k.
  
  n <- length(t)
  
  numero_colunas <- 2 * K
  
  X <- matrix(0,nrow = n,ncol = numero_colunas)
  
  for(i in 1:n){
    
    for(k in 1:K){
      
      coluna_cos <- 2*k - 1
      coluna_sin <- 2*k
      
      X[i,coluna_cos] <-cos(2 * pi * k * t[i] / periodo)
      
      X[i,coluna_sin] <-sin(2 * pi * k * t[i] / periodo)
    }
  }
  return(X)
}

############################################################
# MATRIZ DE DUMMIES SAZONAIS
############################################################

criar_matriz_dummies <- function(t, periodo){
  
  # Para cada instante i: descobre o "mês" (1 a periodo) com %%.
  # Se o mês for o 1 (referência), a linha fica toda zero.
  # Senão, coloca 1 na coluna mes-1.
  
  n <- length(t)
  
  numero_colunas <- periodo - 1
  
  X <- matrix(0,nrow = n,ncol = numero_colunas)
  
  for(i in 1:n){
    
    mes <- ((t[i] - 1) %% periodo) + 1
    
    if(mes != 1){
      
      coluna <- mes - 1
      
      X[i,coluna] <- 1
    }
  }
  
  return(X)
}

############################################################
# 3. RUÍDO BRANCO
############################################################

simular_ruido_branco <- function(n, sigma){
  
  eps <- numeric(n)
  
  for(i in 1:n){
    
    eps[i] <- rnorm(1,mean = 0,sd = sigma
    )
  }
  
  return(eps)
}

############################################################
# 4. SIMULAÇÃO DA SÉRIE TEMPORAL
############################################################

simular_serie <- function(beta_tendencia,
                          X_tendencia,
                          X_sazonal,
                          beta_sazonal,
                          sigma){
  
  n <- nrow(X_tendencia)
  
  numero_colunas_tendencia <-ncol(X_tendencia)
  
  numero_colunas_sazonal <-ncol(X_sazonal)
  
  eps <- simular_ruido_branco(n, sigma)
  
  Xt <- numeric(n)
  
  for(i in 1:n){
    
    valor_tendencia <- 0
    
    # LOOPS ANINHADOS (produto matriz x vetor, elemento a elemento):
    # para cada instante i, soma X_tendencia[i,j]*beta_tendencia[j]
    # (valor da tendência) e depois X_sazonal[i,j]*beta_sazonal[j]
    # (valor da sazonalidade). Xt[i] = tendência + sazonalidade + ruído.
    
    for(j in 1:numero_colunas_tendencia){
      
      valor_tendencia <-valor_tendencia +X_tendencia[i,j] *beta_tendencia[j]
    }
    
    valor_sazonal <- 0
    
    for(j in 1:numero_colunas_sazonal){
      
      valor_sazonal <-valor_sazonal +X_sazonal[i,j] *beta_sazonal[j]
    }
    
    Xt[i] <-valor_tendencia + valor_sazonal + eps[i]
  }
  return(Xt)
}

############################################################
# 5. MÉDIA MÓVEL
############################################################

############################################################
# MÉDIA MÓVEL CENTRADA DE RAIO q
############################################################

calcular_media_movel <- function(serie,q){
  
  n <- length(serie)
  
  numero_medias <- n - 2*q
  
  medias <- numeric(numero_medias)
  
  if(numero_medias > 0){
    
    for(t in (q+1):(n-q)){
      
      soma <- 0
      
      for(j in (t-q):(t+q)){
        
        soma <- soma + serie[j]
      }
      
      medias[t-q] <-soma / (2*q + 1)
    }
  }
  return(medias)
}

############################################################
# 6. MÉDIA AMOSTRAL
############################################################

calcular_media_amostral <- function(serie){
  
  n <- length(serie)
  
  soma <- 0
  
  for(i in 1:n){
    soma <- soma + serie[i]
  }
  
  media <- soma / n
  
  return(media)
}

############################################################
# 7. ESTIMAÇÃO OLS
############################################################

############################################################
# PRODUTO MATRIZ-VETOR
############################################################

multiplicar_matriz_vetor <- function(X,beta){
  
  # PRODUTO MATRIZ x VETOR com 2 loops:
  # - loop de fora (i): escolhe a linha i de X;
  # - loop de dentro (j): soma X[i,j]*beta[j] (produto interno
  #   da linha i com beta);
  # - o resultado[i] é esse produto interno.
  
  n <- nrow(X)
  
  numero_colunas <- ncol(X)
  
  resultado <- numeric(n)
  
  for(i in 1:n){
    
    valor <- 0
    
    for(j in 1:numero_colunas){
      
      valor <-valor + X[i,j] * beta[j]
    }
    
    resultado[i] <- valor
  }
  
  return(resultado)
}

############################################################
# MATRIZ X TRANSPOSTA VEZES X
############################################################

calcular_XtX <- function(X){
  
  # CÁLCULO DE X'X (X transposta vezes X) com 3 loops aninhados.
  # Não se cria a transposta: ler a coluna i de X é o mesmo que ler
  # a linha i de X'.
  # - loops i e j: escolhem a posição (i,j) do resultado (p x p);
  # - loop k: percorre as n linhas, somando X[k,i] * X[k,j];
  # - ou seja, resultado[i,j] é o PRODUTO INTERNO da coluna i com
  #   a coluna j de X.
  # Custo: p * p * n multiplicações.
  
  numero_linhas <- nrow(X)
  
  numero_colunas <- ncol(X)
  
  resultado <- matrix(0,nrow = numero_colunas,ncol = numero_colunas)
  
  for(i in 1:numero_colunas){
    
    for(j in 1:numero_colunas){
      
      soma <- 0
      
      for(k in 1:numero_linhas){
        
        soma <-soma + X[k,i] * X[k,j]
        
      }
      
      resultado[i,j] <- soma
    }
  }
  
  return(resultado)
}

############################################################
# MATRIZ X TRANSPOSTA VEZES y
############################################################

calcular_Xty <- function(X,y){
  
  # CÁLCULO DE X'y com 2 loops:
  # - loop i: escolhe a coluna i de X;
  # - loop k: soma X[k,i] * y[k] (produto interno da coluna i com y);
  # - resultado[i] guarda esse valor.
  # Custo: p * n multiplicações.
  
  numero_linhas <- nrow(X)
  
  numero_colunas <- ncol(X)
  
  resultado <- numeric(numero_colunas)
  
  for(i in 1:numero_colunas){
    
    soma <- 0
    
    for(k in 1:numero_linhas){
      
      soma <-soma + X[k,i] * y[k]
    }
    
    resultado[i] <- soma
  }
  
  return(resultado)
}

############################################################
# ESTIMAÇÃO DOS COEFICIENTES POR OLS
############################################################

ajustar_ols <- function(X,y){
  
  # EQUAÇÕES NORMAIS: (X'X) beta = X'y
  # 1) XtX e Xty vêm dos loops acima;
  # 2) solve(XtX, Xty) resolve o sistema linear de p equações
  #    (por eliminação/decomposição LU). É melhor do que calcular
  #    a inversa de XtX e multiplicar, porque é mais estável;
  # 3) as.numeric() transforma o resultado em vetor simples.
  # Pré-condição: XtX precisa ser invertível (X com colunas
  # linearmente independentes).
  
  XtX <- calcular_XtX(X)
  
  Xty <- calcular_Xty(X,y)
  
  beta_hat <- solve(XtX,Xty)
  
  beta_hat <- as.numeric(beta_hat)
  
  return(beta_hat)
}

############################################################
# 8. AJUSTE DO MODELO COMPLETO
############################################################

ajustar_modelo <- function(t, y, grau_tendencia = 2, periodo = 12, K = 2, metodo_sazonal = "fourier"){
  
  t <- as.numeric(t)
  
  y <- as.numeric(y)
  
  ##########################################################
  # MATRIZ DE TENDÊNCIA
  ##########################################################
  
  X_tendencia <- criar_matriz_tendencia(t, grau_tendencia)
  
  ##########################################################
  # MATRIZ DE SAZONALIDADE
  ##########################################################
  
  if(metodo_sazonal == "fourier"){
    
    X_sazonal <- criar_matriz_fourier(t, periodo, K)
    
  } else if(metodo_sazonal == "dummies"){
    
    X_sazonal <-criar_matriz_dummies(t, periodo)
    
  } else {
    
    stop("Método sazonal inválido")
  }
  
  ##########################################################
  # MATRIZ COMPLETA
  ##########################################################
  
  X_full <-cbind(X_tendencia, X_sazonal)
  
  ##########################################################
  # ESTIMAÇÃO DOS COEFICIENTES
  ##########################################################
  
  beta_hat <-ajustar_ols(X_full, y)
  
  ##########################################################
  # VALORES AJUSTADOS
  ##########################################################
  
  y_hat <-multiplicar_matriz_vetor(X_full, beta_hat)
  
  ##########################################################
  # NÚMERO DE COEFICIENTES DA TENDÊNCIA
  ##########################################################
  
  numero_colunas_tendencia <-ncol(X_tendencia)
  
  ##########################################################
  # COEFICIENTES DA TENDÊNCIA
  ##########################################################
  
  beta_tendencia_hat <-numeric(numero_colunas_tendencia)
  
  for(i in 1:numero_colunas_tendencia){
    
    beta_tendencia_hat[i] <- beta_hat[i]
    
  }
  
  ##########################################################
  # COEFICIENTES DA SAZONALIDADE
  ##########################################################
  
  numero_colunas_sazonal <-
    ncol(X_sazonal)
  
  beta_sazonal_hat <-
    numeric(numero_colunas_sazonal)
  
  for(i in 1:numero_colunas_sazonal){
    
    beta_sazonal_hat[i] <-beta_hat[numero_colunas_tendencia + i]
  }
  
  ##########################################################
  # TENDÊNCIA ESTIMADA
  ##########################################################
  
  tendencia_hat <-multiplicar_matriz_vetor(X_tendencia, beta_tendencia_hat)
  
  ##########################################################
  # SAZONALIDADE ESTIMADA
  ##########################################################
  
  sazonalidade_hat <-multiplicar_matriz_vetor(X_sazonal, beta_sazonal_hat)
  
  ##########################################################
  # RESÍDUOS
  ##########################################################
  
  n <- length(y)
  
  residuos <- numeric(n)
  
  for(i in 1:n){
    
    residuos[i] <- y[i] - y_hat[i]
  }
  
  return(
    list(
      beta_hat = beta_hat,
      y_hat = y_hat,
      tendencia = tendencia_hat,
      sazonalidade = sazonalidade_hat,
      residuos = residuos,
      X_full = X_full,
      grau_tendencia = grau_tendencia,
      periodo = periodo,
      K = K,
      metodo_sazonal = metodo_sazonal
    )
  )
}

############################################################
# 9. DIAGNÓSTICO
############################################################

diagnostico <- function(y,residuos){
  
  n <- length(y)
  
  ##########################################################
  # SOMA DOS QUADRADOS DOS RESÍDUOS
  ##########################################################
  
  ss_res <- 0
  
  for(i in 1:n){
    ss_res <- ss_res + residuos[i]^2
  }
  
  ##########################################################
  # MÉDIA DE y
  ##########################################################
  
  media_y <- calcular_media_amostral(y)
  
  ##########################################################
  # SOMA TOTAL DOS QUADRADOS
  ##########################################################
  
  ss_tot <- 0
  
  for(i in 1:n){
    
    d <- y[i] - media_y
    
    ss_tot <-ss_tot + d^2
  }
  
  ##########################################################
  # MÉDIA DOS RESÍDUOS
  ##########################################################
  
  media_residuos <-calcular_media_amostral(residuos)
  
  ##########################################################
  # VARIÂNCIA DOS RESÍDUOS
  ##########################################################
  
  soma_quadrados <- 0
  
  for(i in 1:n){
    
    d <-residuos[i] - media_residuos
    
    soma_quadrados <- soma_quadrados + d^2
  }
  
  variancia_residuos <-soma_quadrados / (n - 1)
  
  ##########################################################
  # RMSE
  ##########################################################
  
  rmse <-sqrt(ss_res / n)
  
  ##########################################################
  # R²
  ##########################################################
  
  r2 <-1 - ss_res / ss_tot
  
  return(list(media = media_residuos, variancia = variancia_residuos, rmse = rmse, r2 = r2))
}

############################################################
# 10. PREVISÃO
############################################################

prever <- function(modelo,t_futuro){
  
  t_futuro <- as.numeric(t_futuro)
  
  grau_tendencia <-modelo$grau_tendencia
  
  periodo <-modelo$periodo
  
  K <-modelo$K
  
  metodo_sazonal <-modelo$metodo_sazonal
  
  ##########################################################
  # MATRIZ DE TENDÊNCIA FUTURA
  ##########################################################
  
  X_tendencia_fut <-criar_matriz_tendencia(t_futuro,grau_tendencia)
  
  ##########################################################
  # MATRIZ SAZONAL FUTURA
  ##########################################################
  
  if(metodo_sazonal == "fourier"){
    
    X_sazonal_fut <-criar_matriz_fourier(t_futuro, periodo, K)
    
  } else {
    
    X_sazonal_fut <-criar_matriz_dummies(t_futuro, periodo)
  }
  
  ##########################################################
  # MATRIZ FUTURA COMPLETA
  ##########################################################
  
  X_fut <-cbind(X_tendencia_fut, X_sazonal_fut)
  
  ##########################################################
  # PREVISÕES
  ##########################################################
  
  previsoes <- multiplicar_matriz_vetor(X_fut, modelo$beta_hat)
  
  return(previsoes)
}

############################################################
# 11. PLOTS
############################################################

############################################################
# SÉRIE OBSERVADA + AJUSTADA
############################################################

plot_serie_ajustada <- function(t,serie,ajustada){
  
  plot(
    t,
    serie,
    type = "l",
    col = "blue",
    lwd = 2,
    xlab = "Tempo",
    ylab = "X_t",
    main = "Série Observada vs Ajustada"
  )
  
  lines(
    t,
    ajustada,
    col = "red",
    lwd = 2,
    lty = 2
  )
  
  legend(
    "topleft",
    legend = c(
      "Série observada",
      "Série ajustada"
    ),
    col = c(
      "blue",
      "red"
    ),
    lwd = c(
      2,
      2
    ),
    lty = c(
      1,
      2
    ),
    bty = "n"
  )
  
  grid()
}

############################################################
# TENDÊNCIA
############################################################

plot_tendencia <- function(t, tendencia_estimada, tendencia_real = NULL){
  
  plot(
    t,
    tendencia_estimada,
    type = "l",
    col = "red",
    lwd = 2,
    xlab = "Tempo",
    ylab = "Tendência",
    main = "Tendência"
  )
  
  if(!is.null(tendencia_real)){
    
    lines(
      t,
      tendencia_real,
      col = "blue",
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c(
        "Tendência estimada",
        "Tendência real"
      ),
      col = c(
        "red",
        "blue"
      ),
      lwd = 2,
      bty = "n"
    )
  }
  
  grid()
}

############################################################
# SAZONALIDADE
############################################################

plot_sazonalidade <- function(t, sazonalidade_estimada, sazonalidade_real = NULL){
  
  plot(
    t,
    sazonalidade_estimada,
    type = "l",
    col = "green",
    lwd = 2,
    xlab = "Tempo",
    ylab = "Sazonalidade",
    main = "Sazonalidade"
  )
  
  if(!is.null(sazonalidade_real)){
    
    lines(
      t,
      sazonalidade_real,
      col = "blue",
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c(
        "Sazonalidade estimada",
        "Sazonalidade real"
      ),
      col = c(
        "green",
        "blue"
      ),
      lwd = 2,
      bty = "n"
    )
  }
  
  grid()
}

############################################################
# RESÍDUOS
############################################################

plot_residuos <- function(t, residuos){
  
  plot(
    t,
    residuos,
    type = "l",
    col = "black",
    lwd = 2,
    xlab = "Tempo",
    ylab = "Resíduo",
    main = "Resíduos"
  )
  
  abline(
    h = 0,
    col = "red",
    lty = 2
  )
  
  grid()
}

############################################################
# 12. EXEMPLO DE USO
############################################################

############################################################
# VETOR DE TEMPO
############################################################

t <- 1:n

############################################################
# COEFICIENTES DA TENDÊNCIA
############################################################

beta_tendencia <-c(2,0.05,0.001)

grau_tendencia <- 2

############################################################
# MATRIZ DE TENDÊNCIA
############################################################

X_tendencia <-criar_matriz_tendencia(t, grau_tendencia)

############################################################
# COEFICIENTES SAZONAIS
############################################################

alpha <-c(2,1)

beta_h <-c(-1,0.5)

K <- 2

############################################################
# MATRIZ DE FOURIER
############################################################

X_sazonal <-criar_matriz_fourier(t, s, K)

############################################################
# VETOR DE COEFICIENTES SAZONAIS
############################################################

beta_sazonal <- numeric(2 * K)

for(k in 1:K){
  
  coluna_cos <- 2*k - 1
  coluna_sin <- 2*k
  
  beta_sazonal[coluna_cos] <-alpha[k]
  
  beta_sazonal[coluna_sin] <-beta_h[k]
}

############################################################
# SIMULAÇÃO DA SÉRIE
############################################################

Xt <-simular_serie(beta_tendencia = beta_tendencia,
    X_tendencia = X_tendencia,
    X_sazonal = X_sazonal,
    beta_sazonal = beta_sazonal,
    sigma = sigma)

############################################################
# TENDÊNCIA REAL
############################################################

tendencia_real <-multiplicar_matriz_vetor(X_tendencia, beta_tendencia)

############################################################
# SAZONALIDADE REAL
############################################################

sazonalidade_real <-multiplicar_matriz_vetor(X_sazonal, beta_sazonal)

############################################################
# MÉDIA MÓVEL
############################################################

q <- 5

tendencia_media_movel <-calcular_media_movel(Xt, q)

############################################################
# 13. AJUSTE DO MODELO
############################################################

modelo <-ajustar_modelo(t = t, y = Xt, grau_tendencia = 2, periodo = 12, K = 2, metodo_sazonal = "fourier")

############################################################
# 14. DIAGNÓSTICO
############################################################

resultado_diagnostico <-diagnostico(y = Xt ,residuos = modelo$residuos)

cat("Média dos resíduos:", resultado_diagnostico$media, "\n")

cat("Variância dos resíduos:", resultado_diagnostico$variancia, "\n")

cat("RMSE:", resultado_diagnostico$rmse, "\n")

cat("R²:",resultado_diagnostico$r2,"\n")

############################################################
# 15. COEFICIENTES ESTIMADOS
############################################################

cat("Coeficientes estimados:\n")

print(modelo$beta_hat)

############################################################
# 16. GRÁFICOS
############################################################

par(mfrow = c(4,1),mar = c(4,4,2,2))

############################################################
# SÉRIE OBSERVADA + AJUSTADA
############################################################

plot_serie_ajustada(t = t, serie = Xt, ajustada = modelo$y_hat)

############################################################
# TENDÊNCIA
############################################################

plot_tendencia(t = t, tendencia_estimada = modelo$tendencia, tendencia_real = tendencia_real)

############################################################
# SAZONALIDADE
############################################################

plot_sazonalidade(t = t, sazonalidade_estimada = modelo$sazonalidade, sazonalidade_real = sazonalidade_real)

############################################################
# RESÍDUOS
############################################################

plot_residuos(t = t, residuos = modelo$residuos)

par(mfrow = c(1,1))

############################################################
# 17. COMPARAÇÃO ENTRE MÉDIA MÓVEL E TENDÊNCIA ESTIMADA
############################################################

plot(
  t,
  Xt,
  type = "l",
  col = "blue",
  lwd = 1.5,
  xlab = "Tempo",
  ylab = "X_t",
  main = "Série Temporal, Média Móvel e Tendência"
)

tempo_media <-
  (q+1):(n-q)

lines(
  tempo_media,
  tendencia_media_movel,
  col = "red",
  lwd = 2
)

lines(
  t,
  modelo$tendencia,
  col = "green",
  lwd = 2
)

legend(
  "topleft",
  legend = c(
    "Série Xt",
    paste0("Média móvel q=",q),
    "Tendência estimada por OLS"
  ),
  col = c(
    "blue",
    "red",
    "green"
  ),
  lwd = c(
    1.5,
    2,
    2
  ),
  bty = "n"
)

grid()

############################################################
# 18. PREVISÃO
############################################################

############################################################
# NOVOS INSTANTES DE TEMPO
############################################################

t_futuro <- (n+1):(n+12)

############################################################
# PREVISÕES
############################################################

previsoes <-prever(modelo, t_futuro)

cat("Previsões para os próximos 12 períodos:\n")

print(previsoes)

############################################################
# FIM
############################################################
