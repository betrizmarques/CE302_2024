verificar_se_eh_primo <- function(numero){
  if (is.na(numero) || numero < 2) {
    return(FALSE) 
  }
  lista <- 1:numero
  contagem <-  0
  for (i in lista) {
    if (numero %% i == 0){
      contagem <- contagem +1
    }
  }
  return(contagem == 2)
}
 

eh_quadrado_perfeito <- function(numero){
  if (numero < 0){
    return (FALSE)
  }
  raiz = sqrt(numero)
  return (raiz == floor(raiz))
}


varrer_matriz <- function(matriz){
  m <- matriz 
  for (i in 1:nrow(matriz)){
    for (j in 1:ncol(matriz)){
      valor <- matriz[i, j]
      if (verificar_se_eh_primo(valor)){
        m[i,j] <- valor*3
      }
      else if (eh_quadrado_perfeito(valor)){
        novo_valor <- valor-14
        if (novo_valor < 0){
          m[i,j] <- abs(novo_valor)^5
        } else
          m[i,j] <- novo_valor
      } else if (valor < 0){
        m[i,j] <- sqrt(abs(valor))
      }
      
    }
  }
  return(m)
}

A <- matrix(c(-5, 7, 8, 9, -2, -1, 5, 3, -8, 6, -7, 4, -10, -6, 0, 2), nrow = 4, byrow = FALSE)
B <- matrix(c(-11, 17, -2, -12, -8, 0, 4, 10, -6, -20, -19, 20, -3, -17, -9, -10, -18, 15, -15, 1), nrow = 4, byrow = FALSE)
C <- matrix(c(6, 5, -19, 15, 25, 30, -3, 16, -24, -2, -5, 13, -6, -18, 1, -28), nrow = 4, byrow = FALSE)

nova_A <- varrer_matriz(A)
nova_B <- varrer_matriz(B)
nova_C <- varrer_matriz(C)

resultado <- nova_A %>% diag() %>% sum()
resultado


resultado_2 <- nova_C %>% max() %>% abs()
resultado_2

resultado_3 <- sum(nova_C[, 1])
resultado_3

a_primos <- sum(sapply(A, verificar_se_eh_primo))
a_primos

primos_totais <- sum(sapply(c(A, B, C), verificar_se_eh_primo))
primos_totais


resultados <- function(vetor){
  media = sum(vetor)/length(vetor)
  desvio = vetor - media
  quadrados = desvio^2
  variancia = sum(quadrados)/(length(vetor)-1)
  desvio_padrao = sqrt(variancia)
  dma = sum(abs(desvio))/(length(vetor)-1)
  as2 = 3*((media-median(vetor))/sqrt(variancia))
  return(variancia, dma, as2)
}
