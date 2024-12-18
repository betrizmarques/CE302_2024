#exercícios 
require(tidyverse)

car_crash <- read.csv('dados/Brazil Total highway crashes 2010 - 2023 (2).csv.gz') 

dados <- car_crash %>% 
  select(data,tipo_de_ocorrencia,automovel,bicicleta,caminhao,onibus,moto,outros,trator_maquinas)


feridos <- car_crash %>% 
  select(contains('feridos'))

variaveis_numericas <- car_crash %>% 
  select(where(is.numeric))

variaveis_logicas <- car_crash %>% 
  select(where(is.logical))

  
terminam_com_o <- car_crash %>% 
  select(ends_with("o"))


iniciam_com_t <- car_crash %>% 
  select(starts_with("t"))

filtro <- car_crash %>% 
  filter(automovel>=5, moto == 3)

# Filtrar observações com pelo menos 5 carros OU 3 motos envolvidos E em operadoras específicas

operadoras <- c("Autopista Regis Bittencourt", "Autopista Litoral Sul", "Via Sul")
filtro_avancado <- car_crash %>%
  filter((automovel >= 5 | moto >= 3) & operadora %in% operadoras)