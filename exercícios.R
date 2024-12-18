#OBJETOS EM R ----------------------------------------------------------------

require(dplyr)
dados <- read.csv("Dataset_FireWatch_Brazil_2024.csv")

#Imprima na tela as 9 primeiras observações.
head(dados,9)

#Imprima as últimas 3 observações.
tail(dados)

#Quantas observações temos?
nrow(dados)

#Quantas variáveis temos?
ncol(dados)

#Apresente o sumário dos dados.
summary(dados)

#Apresente a estrutura dos dados.
str(dados)

#Quantos biomas estão sendo afetados?
length(unique(dados$bioma))

# Qual a média de avg_numero_dias_sem_chuva para os estados da região sul 
#e da região norte?
estados_norte <- toupper(c("Amazonas", "Pará", "Acre" , "Roraima", "Rondônia" , "Amapá",
"Tocantins"))
estados_sul <- toupper(c("Paraná", "Santa Catarina", "Rio Grande do Sul"))

media_norte <- dados %>% 
  filter(estado %in% estados_norte) %>% 
  summarise(media_dias_sem_chuva_norte = mean(avg_numero_dias_sem_chuva, na.rm = TRUE))


media_norte

media_sul <- dados %>%
  filter(estado%in% estados_sul) %>%
  summarise(media_dias_sem_chuva_sul = mean(avg_numero_dias_sem_chuva, na.rm = TRUE))  

media_sul

#TRANSFORMAÇÃO DE DADOS -------------------------------------------------------
#ex 1

#Selecione as variáveis data, tipo_de_ocorrencia, automovel, bicicleta, onibus, 
#caminhao, moto, trator, outros e total.
car_crash <- read.csv("Brazil Total highway crashes 2010 - 2023 (2).csv.gz")
dados_filtrados <- car_crash %>% 
  select(data, tipo_de_ocorrencia, automovel, bicicleta, onibus, caminhao, moto, 
         trator_maquinas, outros)

#Selecione todas as variáveis que contenham a palavra feridos.
dados_filtrados <- car_crash %>% 
  select(contains("feridos")) %>% 
  glimpse()

#Selecione todas as variáveis numéricas.
dados_filtrados <- car_crash %>% 
  select(where(is.numeric)) %>% 
  glimpse()

#Selecione todas as variáveis lógicas.
dados_filtrados <- car_crash %>% 
  select(where(is.logical)) %>% 
  glimpse()

#Selecione todas as variáveis que terminem com a letra o.
dados_filtrados <- car_crash %>% 
  select(ends_with("o")) %>% 
  glimpse()

#Selecione todas as variáveis que iniciem com a letra t.
dados_filtrados <- car_crash %>% 
  select(starts_with("t")) %>% 
  glimpse()

#Filtre as observações com pelo menos 5 carros E 3 motos envolvidos no acidente.
dados_filtrados <- car_crash %>% 
  filter(automovel>=5, moto==3) %>% 
  glimpse()

#Filtre as observações com pelo menos 5 carros OU 3 motos envolvidos no acidente.
dados_filtrados <- car_crash %>% 
  filter(automovel >=5 | moto ==3) %>% 
  glimpse()

#Filtre as observações com vítimas.
dados_filtrados <- car_crash %>% 
  filter(tipo_de_ocorrencia %like% "com vítima") %>% 
  glimpse()

#Filtre as observações com pelo menos 5 carros OU 3 motos envolvidos no acidente 
#E que ocorreram em alguma das seguintes operadoras: “Autopista Regis Bittencourt”, 
#“Autopista Litoral Sul”, “Via Sul”.


autopistas <- c("Autopista Regis Bittencourt", "Autopista Litoral Sul", "Via Sul")
dados_filtrados <- car_crash %>% 
  filter(automovel>=5 | moto == 3) %>% 
  filter(lugar_acidente%in% autopistas) %>% 
  glimpse()

#ex 2 
#Utilizando o banco de dados starwars faça o que se pede:

# Qual é o número total de espécies únicas presentes? Qual a frequência de
#indivíduos por espécie?
total_especies <- starwars %>% 
  filter(!is.na(species)) %>% 
  summarise(n_especies = n_distinct(species)) %>% 
  pull(n_especies)

total_especies

freq_por_especie <- starwars %>% 
  group_by(species) %>% 
  summarise(n = n())

#Calcule a altura média de personagens masculinos e femininos.
altura_media_por_sexo <- starwars %>% 
  filter(sex %in% c("male", "female") ) %>% 
  group_by(sex) %>% 
  summarise(media = mean(height, na.rm = TRUE))

#Qual é a média de idade dos personagens de cada espécie para personagens masculinos?

idade_media_masculinos <- starwars %>% 
  filter(sex == "male") %>% 
  group_by(sex) %>% 
  summarise(media = mean(birth_year, na.rm = TRUE))

# Para cada espécie presente na base de dados, identifique o personagem mais 
#velho e sua idade correspondente.
mais_velho <- starwars %>% 
  group_by(species) %>% 
  summarise(max = ifelse(all(is.na(birth_year)), NA, max(birth_year, na.rm = TRUE)))

#ex 3 
#Utilizando o banco de dados car_crash formate a coluna data em uma data (dd-mm-yyyy);

require(tidyverse)
require(lubridate)
car_crash <- read.csv("Brazil Total highway crashes 2010 - 2023 (2).csv.gz")

car_crash <- car_crash %>%
  mutate(data = dmy(data),              # Converte para Date
         data = format(data, "%d-%m-%Y"))  # Reaplica o formato desejado


str(car_crash$data)
#Utilizando o banco de dados car_crash formate a coluna horario para o horário 
#do acidente (hh:mm:ss)

car_crash <- car_crash %>%
  mutate(horario = hms(horario))

#Qual o mês com maior quantidade de acidentes?

car_crash <- car_crash %>% 
  mutate(mes = month(data, label = TRUE, abbr = FALSE))

mes_com_mais_acidentes <- car_crash %>% 
  group_by(mes) %>% 
  summarise(n_acidentes = n()) %>% 
  filter(n_acidentes == max(n_acidentes))
mes_com_mais_acidentes

#Qual horário acontecem menos acidentes?
horario_com_menos_acidentes <- car_crash %>% 
  mutate(hora = hour(horario)) %>% 
  group_by(hora) %>% 
  summarise(n_acidentes_por_hora = n()) %>% 
  filter(n_acidentes_por_hora == min(n_acidentes_por_hora))
horario_com_menos_acidentes

#Qual a média, desvio padrão, mediana, Q1 e Q3 para a quantidade de indivíduos
#classificados como levemente feridos por mês/ano?
car_crash <- car_crash %>% 
  mutate(mes_ano = format(dmy(data), "%m/%Y"))

estatisticas <- car_crash %>% 
  group_by(mes_ano) %>% 
  summarise(media = mean(levemente_feridos, na.rm = TRUE),
            mediana = quantile(levemente_feridos, 0.5, TYPE = 5, na.rm = TRUE),
            Q1 = quantile(levemente_feridos, 0.25, type = 5, na.rm = TRUE),
            Q2 = quantile(levemente_feridos, 0.75, type = 5, na.rm =TRUE)
            )

estatisticas  

#Quantos acidentes com vítimas fatais aconteceram, por mês/ano, em mediana entre
#as 6:00am e 11:59am.

filtro <- car_crash %>% 
  filter(horario >= hms("06:00:00") & horario <= hms("11:59:00"))


analise_vitmas_por_mes <- filtro %>% 
  filter(mortos>0) %>% 
  group_by(mes_ano) %>% 
  summarise(mediana = median(mortos))

analise_vitmas_por_mes



#LOOPINGS 
#Suponha o lançamento de um dado não viesado, com seis faces. Quantas vezes devo 
#lançar o dado para obter a face 5?

set.seed(1234)

dado <- seq(1:6)

n_lancamento = 0
sorteio = 0 

while (sorteio !=5){
  sorteio = sample(dado, 1)
  n_lancamento = n_lancamento +1
  
  cat(paste0("\n\nLançamento: ", n_lancamento, "\nValor Sorteado: ", sorteio))
}


n_lancamento


n_lancamento = 0
while (sorteio != 7){
  sorteio = sample(dado, 1)
  n_lancamento = n_lancamento+1
  
  cat(paste0("\n\nLançamento: ", n_lancamento, "\nValor Sorteado: ", sorteio))
  
  if(n_lancamento == 100){
    break
  }
}


valor_sorteado = numeric()
n_lancamento = 0

while (sorteio!=7){
  n_lancamento = n_lancamento +1
  valor_sorteado[n_lancamento] = sample(dado, 1)
  
  cat(paste0("\n\nLançamento: ", n_lancamento, "\nValor Sorteado: ", sorteio))
  
  if(n_lancamento == 100){
    break
  }
}

valor_sorteado

#Crie uma função que calcule os n primeiros números da sequência de Fibonacci. 
#A sequência de Fibonacci começa com 0 e 1, e os números subsequentes são a soma
#dos dois anteriores (0, 1, 1, 2, 3, 5, 8, …).


fibonacci <- function(n){
  if (n==0){
    return(c())
  }
  else if (n == 1){
    return(c(0))
  }
  
  fib <- numeric(n)
  fib[1] <- 0
  fib[2] <- 1
  
  for (i in 3:n){
    fib[i] <- fib[i-1]+fib[i-2]
  }
  return(fib)
  
}

fibonacci(10)


eh_primo <- function