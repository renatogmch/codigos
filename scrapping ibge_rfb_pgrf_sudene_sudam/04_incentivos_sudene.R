# Leitura dos dados de laudos concedidos pela SUDENE
# Dados do site da SUDENE
# Renato Gomes Chaves
# Criação: 19/06/2024

# Pacotes ======================================================================
{
  # install.packages('pacman')
  library(pacman)
  # remotes::install_github("georgevbsantiago/qsacnpj")
  # Opcao pra o print ser completo de todas as colunas:
  p_load(tidyverse,
         remotes,
         qsacnpj,
         janitor,
         readxl,
         here,
         foreach,
         xlsx)
  options(dplyr.width = Inf)
}

# Arquivos da SUDENE
sudene_2013a2022 <- read_excel(here('Data/dados_sudene/INCENTIVOS20132022SETORESPRIORITRIOSPARAPUBLICAOSITESUDENE.xlsx'), 
                               skip = 7,
                               n_max = 3737,
                               col_types = 'text') %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), ~ stringi::stri_trans_general(., "latin-ascii"))) %>% 
  # Filtrar apenas 75% IRPJ e retirar laudos de 2023 que ja tem no outro arquivo
  filter(str_detect(incentivo, 'IRPJ$'),
         !str_detect(data_da_aprovacao, '2023$')) %>% 
  select(cnpj,
         razao_social,
         data_da_aprovacao,
         prazo_do_beneficio,
         tipo_projeto,
         municipio,
         uf,
         enquadramento
  )

sudene_2023 <- read_excel(here('Data/dados_sudene/incentivos2023.xlsx'), 
                          skip = 6) %>% 
  janitor::clean_names() %>% 
  mutate(across(everything(), ~ stringi::stri_trans_general(., "latin-ascii"))
         ) %>% 
  # filtrar apenas projetos de 75% IRPJ
  filter(str_detect(incentivo, 'IRPJ$')) %>% 
  select(cnpj,
         razao_social,
         data_da_aprovacao = dt_laudo,
         prazo_do_beneficio,
         tipo_projeto,
         municipio,
         uf,
         enquadramento
  )

# Empilhar os dados e salvar o resultado
bind_rows(sudene_2013a2022,
          sudene_2023) %>% 
  write_rds(here('data/processed/incentivos_sudene_2013a2023.rds'))