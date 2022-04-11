# geocode dos portos da antaq
# 01/08/2020
# V: 1.0

require(pacman)
p_load(tidyverse,
       ggmap)
register_google('AIzaSyAVl2qUBYD6osU_gHSDJqEp21IiKreQQoo')

here('data/raw/extra/portos_antaq.xlsx') %>%
  readxl::read_xlsx() %>%
  transmute(nome_porto = paste0('PORTO DE ', NOME)) %>%
  # pull eh necessario pra transformar uma tible num simples vetor
  # caso contrario geocode nao pega
  pull(1) %>%
  geocode(location = .,
          output = 'latlon',
          source = 'google',
          override_limit = T) %>%
  cbind(here('data/raw/extra/portos_antaq.xlsx ') %>%
                             readxl::read_xlsx() %>%
                             transmute(nome_porto = NOME,
                                       tipo_porto = TIPO,
                                       instalacao = INSTALAÇÃO),
                           .
        ) %>%
  write_rds(here('data/raw/extra/portos_georef.rds'))