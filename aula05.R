require(tidyverse)
require(magrittr)
require(data.table)

files = system("ls archive", intern = TRUE)

filmes = list()
for(i in 1:length(files)){
  nome = gsub(x = files[i], 
              pattern = ".csv", 
              replacement = "")
  
  filmes[[i]] <- fread(paste0("/home/est/absm24/CE302/CE302_2024/archive/",  files[i])) %>% 
    mutate(tipo = nome)
  
  filmes[[i]]$year %<>% as.integer()
  filmes[[i]]$`gross(in $)` %<>% as.numeric()
  
}

filmes %<>% bind_rows()

filmes %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo) + ## Adicionamos cor
  geom_point()

filmes %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo) + 
  geom_point() + 
  theme_minimal() ## Incluímos tema

filmes %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo) + 
  geom_point() + 
  facet_wrap(vars(tipo)) + ## Fazemos o gráfico separado por tipo
  theme_minimal()

filmes %>% 
  filter(tipo %in% "animation") %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo, 
       size = rating) + 
  geom_point() + 
  theme_minimal()


filmes %>% 
  filter(tipo %in% "animation") %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo, 
       size = rating) + 
  geom_point() + 
  scale_size_continuous(range=c(0.01, 2)) + 
  theme_minimal()

filmes %>% 
  filter(tipo %in% "animation") %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo, 
       size = rating) + 
  geom_point() + 
  scale_size_continuous(range=c(0.01, 2)) + 
  labs(x = "Ano", 
       y = "Investimento ($)") + 
  theme_minimal()

filmes %>% 
  filter(tipo %in% "animation") %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo, 
       fill = tipo) + 
  geom_point() + 
  geom_smooth(method = "loess") + # O método aqui se refere ao método utilizado para estimação da curva
  scale_size_continuous(range=c(0.01, 2)) + 
  labs(x = "Ano", 
       y = "Investimento ($)") + 
  theme_minimal()
