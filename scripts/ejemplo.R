# Author: Víctor Suárez-Lledó
# Date: Tue Jan 24 2019
# Summary: Example script 
# --------------


# Librerías ---------------------------------------------------------------

library(tidyverse)
library(tidytext)


# Data --------------------------------------------------------------------

# Abre cuadro de diálogo 
file.choose() 

# Carga el archivo .rds de ejemplo
vox_datos <- read_rds(path = "F:\\UCA Social Network Analysis\\data\\vox_rest_api_example.rds")


# Text as data ------------------------------------------------------------

# Selección de tweets cuyo nombre de usuario es "mar_feliciano"
vox_datos %>% 
  filter(screen_name == "mar_feliciano") -> vox_datos_mar_feliciano

# Crear lista de palabras o tokens
vox_datos %>% 
  select(text) %>% 
  mutate(text = stringi::stri_trans_general(text, "Latin-ASCII")) %>% # Elimina acentos 
  unnest_tokens(word, text, token = "tweets") %>% # Crea tokens
  count(word, sort = TRUE) -> lista_palabras # Añade columna con frecuencias
  
# Stop words 
stop_words_personal <- bind_rows( # Combina data frames con stop words
  stop_words, # Lista de stop words en tidytext package
  tibble( # Crea data frame 
    word =tm::stopwords("spanish"), # Stop words en tm package
    lexicon = "custom" 
  )
)  

# Elimina de nuestra lista de palabras aquellas que aparecen como stop words en stop_words_personal
lista_palabras %>% 
  filter(!word %in% stop_words_personal$word)-> lista_final 


# Visualización -----------------------------------------------------------

# Gráfico de frecuencias 
lista_final %>% 
  top_n(10) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) +
  geom_col() +
  coord_flip()
  
# Ngrams de dos palabras
vox_datos %>% 
  select(text) %>% 
  mutate(text = stringi::stri_trans_general(text, "Latin-ASCII")) %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  count(word, sort = TRUE) %>% 
  separate(word, c("word1","word2"), sep = " ")

