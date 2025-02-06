# Criacao do DB primario para uso, juntado ESTABELECIMENTOS e EMPRESAS
# Filtrados para apenas estados do Norte e Nordeste
# Renato Gomes Chaves

# Pacotes ======================================================================
{
  # install.packages('pacman')
  library(pacman)
  Sys.unsetenv("GITHUB_PAT")
  remotes::install_github("georgevbsantiago/qsacnpj")
  # Opcao pra o print ser completo de todas as colunas:
  p_load(tidyverse,
         remotes,
         qsacnpj,
         janitor,
         readxl,
         writexl,
         here,
         foreach)
  options(dplyr.width = Inf)
}

# Carregar dados ===============================================================
# Dados do lucro real
lucro_real <- read_delim(here('Data/dados_rfb_cnpj/Lucro Real.csv'),
                         col_types = cols(.default = "c"),
                         delim = ",",
                         escape_double = FALSE,
                         locale = locale(encoding = "ISO-8859-2"),
                         trim_ws = TRUE) %>% 
  janitor::clean_names() %>% 
  # filtrar apenas lucro presumido estritamente
  # e filtrar apenas ano mais recente
  filter(forma_de_tributacao == 'LUCRO REAL',
         ano == '2023') %>% 
  select(ano,
         cnpj,
         forma_de_tributacao) %>% 
  mutate(cnpj = str_remove_all(cnpj, '[\\.\\-\\/]')) %>% 
  # retirar cnpj duplicados internamente por ano
  # o group_by fas o distinct procurar apenas no mesmo ano
  group_by(ano) %>% 
  distinct(cnpj,
           .keep_all = T) %>% 
  pivot_wider(names_from = ano,
              values_from = forma_de_tributacao,
              names_prefix = 'tributacao_')

# Dados do lucro pesumido
lucro_presumido <- foreach(file_list = list.files(here('Data/dados_rfb_cnpj/Lucro Presumido/'),
                                                  pattern = "^Lucro"),
                           .combine = rbind) %do% {
                             read_delim(paste0('Data/dados_rfb_cnpj/Lucro Presumido/', file_list),
                                        col_types = cols(.default = "c"),
                                        delim = ",",
                                        escape_double = FALSE,
                                        locale = locale(encoding = "ISO-8859-2"),
                                        trim_ws = TRUE) %>% 
                               janitor::clean_names()
                           } %>% 
  # filtrar apenas lucro presumido estritamente
  # e filtrar apenas ano mais recente
  filter(forma_de_tributacao == 'LUCRO PRESUMIDO',
         ano == '2023') %>% 
  select(ano,
         cnpj,
         forma_de_tributacao) %>% 
  mutate(cnpj = str_remove_all(cnpj, '[\\.\\-\\/]')) %>% 
  # retirar cnpj duplicados internamente por ano
  # o group_by fas o distinct procurar apenas no mesmo ano
  group_by(ano) %>% 
  distinct(cnpj,
           .keep_all = T) %>% 
  pivot_wider(names_from = ano,
              values_from = forma_de_tributacao,
              names_prefix = 'tributacao_')

# juntando presumido e real em uma tibble
# tem algumas que aparecem nos dois, preservei como lucro real
lucro_presumido %>% 
  filter(!cnpj %in% lucro_real$cnpj) %>% 
  rbind(lucro_real) %>% 
  write_rds(here('data/processed/tributacao_2023'))
