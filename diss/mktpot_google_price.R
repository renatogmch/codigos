# Tentativa de pegar distancia poir rotas no google maps.
# Problema: preco. 0.005 dolar/request. Sao 5570*5570 requests para todos o municipios.
# Salvando aqui pra ter o historico
library(googleway)
set_key() # key da minha conta, projeto dissertacao
# readline(prompt="Enter GMaps API Key: ") %>% set_key() # pra colocar nova key
pib_anual <- here('data/raw/extra/pib_per_capita_anual_mun_2017.csv') %>% # Seleciona arquivo
  readr::read_delim(skip=3, # pula 3 primeiras linhas,
                    n_max=5570, # retira ultimas linhas com anotacoes
                    delim = ',',
                    col_types = cols('c', 'c', '-', '-', '-', '-', 'n','-', 'n',
                                     '-', 'n','-', 'n','-', 'n','-', 'n', '-') # especifica o tipo de coluna na importacao
  ) %>%
  rename(cod_mun = Cód.,
         nome_mun = Município
  ) %>%
  mutate(nome_cidade = paste0('Cidade de ',nome_mun))



pib_rotas <- pib_anual %>% # criar coluna adicionando a cada linha 'Cidade de ' antes do nome pra fazer o geocode no gmapsdistance
  slice(1:10) %>%
  select(nome_cidade)

teste <- google_distance(pib_rotas$nome_cidade,
                pib_rotas$nome_cidade,
                mode = 'driving',
                key = 'AIzaSyAVl2qUBYD6osU_gHSDJqEp21IiKreQQoo') 

write_rds(teste, path = here('/data/raw/extra/teste_gmaps.rds'))

# carregar teste_gmaps pra tentar os plucks
teste_pib_rotas_old <- read_rds(here('/data/processed/pib_per_capita_anual_mun_2016_geocoded.rds'))
teste_pib_rotas <- read_rds(here('/data/raw/extra/teste_gmaps.rds')) %>%
  pluck('rows', 'elements', 'distance') 



  foreach(i = teste_pib_rotas,
          .packages = 'dplyr') %dopar% {
            i %>%
            pluck('distance') %>%
              select(value)
          } %>%
  bind_cols()