# Script do geocoding das firmas para a variavel MKTPOT
# funcao para pegar lat e lon dos municipios pelo nome
# necessica do google api key pra funcionar
# key pega no google cloud plataform, subsecao credentials
{
  require(pacman)
  p_load("tidyverse",
         "gmapsdistance"
         )
  gc()
}

set.api.key('AIzaSyBrAwk4BPHlLwAo_c9czSXR3uY-6IfB11Y') # key da minha conta, projeto dissertacao

# readline(prompt="Enter GMaps API Key: ") %>% set.api.key() # pra colocar nova key

here('data/raw/extra/pib_per_capita_anual_mun_2017.xlsx') %>% # Seleciona arquivo
  readxl::read_excel(skip=3, # pula 3 primeiras linhas
                     col_types = c('text', 'text', 'skip', 'skip', 'skip', 'skip', 'numeric','skip', 'numeric',
                                   'skip', 'numeric','skip', 'numeric','skip', 'numeric','skip', 'numeric') # especifica o tipo de coluna na importacao
  ) %>%
  rename(cod_mun = ...1,
         nome_mun = ...2
  ) %>%
  # dplyr::slice(-5571) %>% # tira linha que diz a fonte ao final da tabela
  dplyr::slice(1:5) %>% # so as primeiras linhas pra teste
  {pib_municipal <<- .} %>% # salvar resultado intermediario
  mutate(nome_cidade = paste0('Cidade de ',nome_mun)) %>% # criar coluna adicionando a cada linha 'Cidade de ' antes do nome pra fazer o geocode no gmapsdistance
  {pib_municipal_rotas <<- bind_cols(., # matriz de distancias com rotas
                                     gmapsdistance(str_replace_all(.$nome_mun, ' ', '+'), # gmapsdistance calcula ROTA em KM entre origem e destino
                                                   str_replace_all(.$nome_mun, ' ', '+'), # str_replace_all troca os espacos por '+' pra o geocode pegar
                                                   mode = 'driving' # usa rotas ao inves de distancias retas
                                     ) %>% 
                                       pluck('Distance') # pluck extrai apenas as distancias da lista resultante
  )
  }
  # parei aqui