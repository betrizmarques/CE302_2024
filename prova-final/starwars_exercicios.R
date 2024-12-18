dados <- starwars


especies_unicas <- length(unique(dados$species))
especies_unicas

#ou
numero_especies_unicas <- dados %>%
  summarise(total_especies = n_distinct(species, na.rm = TRUE)) %>%
  pull(total_especies)
numero_especies_unicas


altura_media <- dados %>% 
  filter(sex == "female" | sex == "male") %>% 
  summarise(altura_media = mean(height, na.rm = T))
altura_media


media_idade_male <- dados %>% 
  filter(sex == "male") %>% 
  group_by(species) %>% 
  summarise(media_idade = mean(birth_year, na.rm = T))
media_idade_male


personagem_mais_velho <- starwars %>%
  filter(!is.na(birth_year) & !is.na(species)) %>%
  group_by(species) %>%
  slice_max(birth_year, with_ties = FALSE) %>%
  select(species, name, birth_year) %>%
  rename(idade = birth_year) %>%
  ungroup()
personagem_mais_velho

