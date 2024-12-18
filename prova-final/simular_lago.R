contar_primos <- function(limite) {
  contagem_2 <- 0
  for (i in 1:limite){
    contagem <- 0
    for (j in 1:i){
      if (i %% j == 0){
        contagem = contagem + 1
      }
      if (contagem == 2){
        contagem_2 <- contagem_2 + 1
      }
    }
  }
  return ( contagem_2)
}
contar_primos(4)

contar_primos(2)
contar_primos(3)

####################################################################3
simular_lago <- function(dias, peixes_iniciais = 50, estacao, ph){
  capacidadetotal <- 10^5
  reproducaobasal <- 0.24
  peixes_atuais <- peixes_iniciais
  dia_atual <- 0
  switch (estacao,
          "Primavera" = fatorrep <- 0.00,
          "Verão" = fatorrep <- -0.02,
          "Outono" = fatorrep <- 0.05,
          "Inverno" = fatorrep <- -0.05
  )
  if (5.9<ph & ph<6.6) {
    percentualpescado <- 0.18
  } 
  if (6.5<ph & ph<7.1) {
    percentualpescado <- 0.19
  } 
  if (7.0<ph & ph<7.6) {
    percentualpescado <- 0.03
  }
  if (7.5<ph & ph<8.1) {
    percentualpescado <- 0.04
  }
  taxa_de_reproducao_ajustada <- reproducaobasal + fatorrep
  
  peixes_pescados <- peixes_atuais * percentualpescado
  
  dias_x_peixes <- data.frame(dia = c(0), peixes = c(peixes_iniciais))
  
  while (dia_atual<1000 & dia_atual<dias) {
    
    peixes_atuais <- ceiling(peixes_atuais*(1+taxa_de_reproducao_ajustada)-peixes_pescados)
    if (peixes_atuais>capacidadetotal) {
      peixes_atuais <- 10^5
    } else{peixes_atuais <- peixes_atuais}
    dia_atual <- dia_atual + 1
    
    novodia <- data.frame(dia_atual,peixes_atuais)
    names(novodia) <- c("dia","peixes")
    dias_x_peixes <- rbind(dias_x_peixes,novodia)
  }
  return(dias_x_peixes)
}


# Exemplo de uso
simular_lago(dias = 30,peixes_iniciais = 50000,estacao = "Verão",ph = 7.0)



# R1
simular_lago(30,50,"Inverno",7.3)

# R2
simular_lago(50,50,"Inverno",7.3)

# R3
simular_lago(65,50,"Verão",6.7)

# R4
simular_lago(70,50,"Verão",6.7)
