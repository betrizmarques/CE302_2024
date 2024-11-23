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
