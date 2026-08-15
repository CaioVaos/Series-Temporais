############################################################
# VERIFICAÇÃO DE PERTENCIMENTO A UM CILINDRO
############################################################

# Verifica um ----

verifica_cilindro <- function(trajetoria, tempos_fix, limite_inferior, limite_superior){
  
  pertence <- TRUE
  
  for(j in 1:length(tempos_fix)){
    
    t <- tempos_fix[j]
    
    valor <- trajetoria[t]
    
    limite_inf <- limite_inferior[j]
    limite_sup <- limite_superior[j]
    
    if(valor < limite_inf || valor > limite_sup){
      pertence <- FALSE
    }
  }
  
  return(pertence)
}

# Verifica varias ----

verifica_cilindro_matriz <- function(trajetorias, tempos_fix, limite_inferior, limite_superior){
  
  m <- ncol(trajetorias)
  
  pertence_cilindro <- logical(m)
  
  for(i in 1:m){
    pertence_cilindro[i] <- verifica_cilindro(trajetorias[, i],
                                               tempos_fix,
                                               limite_inferior,
                                               limite_superior)
  }
  
  return(pertence_cilindro)
}
