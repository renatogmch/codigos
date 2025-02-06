# Criacao do DB primario para uso, juntado ESTABELECIMENTOS e EMPRESAS
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
# 01 indice de empresas lucro real no ano mais recente (2022)
tributacao_2023 <- read_rds(here('data/processed/tributacao_2023'))

# 02 Indice de CNAEs
cnae_subclasses <- read_excel("Data/dados_ibge/cnae_subclasses_2_3_Estrutura_Detalhada.xlsx",
                              col_types = 'text',
                              skip = 4,
                              col_names = c('SECAO',
                                            'DIVISAO',
                                            'GRUPO',
                                            'CLASSE',
                                            'SUBCLASSE',
                                            'CNAE')
) %>%
  mutate(across(SECAO:SUBCLASSE, ~ str_remove_all(., "[\\.\\-\\/]")),
         across(everything(), ~ stringi::stri_trans_general(., "latin-ascii"))
  ) %>% 
  unite('numero_cnae', SECAO:SUBCLASSE,
        remove = T,
        na.rm = T) %>% 
  mutate(cnae_com_nome = paste(numero_cnae, CNAE, sep = ' - '))

# 03 Incentivadas SUDENE e SUDAM
incentivadas_sudene <- read_rds(here('data/processed/incentivos_sudene_2013a2023.rds'))
incentivadas_sudam <- read_rds(here('data/processed/incentivos_sudam_2010a2023.rds')) 

# Criar o DB completo ==========================================================
estabelecimentos <- foreach(file_list = list.files(here('Data/processed/'),
                                                    pattern = "^estabelecimentos"),
                             .combine = 'rbind') %do% {
                               read_rds(paste0('data/processed/', file_list)) %>%
                                 janitor::clean_names() %>% 
                                 # Remover caracteres especiais do campo complemento
                                 mutate(complemento = str_remove_all(complemento, '[\\;]'))
                             } %>% 
  # criar col pro join com empresas
  mutate(cnpj_basico = str_sub(cnpj, 1, 8)) 

empresas <- foreach(file_list = list.files(here('Data/processed/'),
                                           pattern = "^empresas"),
                    .combine = 'rbind') %do% {
                      read_rds(paste0('data/processed/', file_list)) %>%
                        janitor::clean_names()
                    }

# Salvar o resultado de 7gb em rds
left_join(estabelecimentos,
          empresas,
          by = 'cnpj_basico') %>% 
  # retirar CNPJs abertos para eleicoes
filter(!str_detect(razao_social, '^ELEIC')) %>% 
  # adicionar nomes dos cnaes primarios
  left_join(cnae_subclasses,
            join_by('cnae_fiscal_principal' == 'numero_cnae')) %>% 
  # adicionar regimes tributarios REAL e PRESUMIDO
  left_join(tributacao_2023, by = 'cnpj') %>% 
  # mutate pra criar as seguintes colunas
  # status_incentivada: acusar se o cnpj aparece nas planilhas da SUDAM ou SUDENE
  # data_de_inicio_atividade: data para formato yyyy-mm-dd
  # data_situacao_cadastral: data para formato yyyy-mm-dd
  # tributacao_2023: colocar 'SIMPLES/MEI/OUTRO' nos NAs 
  mutate(status_incentivada = if_else(cnpj %in% incentivadas_sudene$cnpj |
                                        cnpj %in% incentivadas_sudam$cnpj,
                                      'Incentivada', 'Nao Incentivada'),
         data_de_inicio_atividade = ymd(data_de_inicio_atividade),
         data_situacao_cadastral = ymd(data_situacao_cadastral),
         tributacao_2023 = replace_na(tributacao_2023,'SIMPLES/MEI/OUTRO')) %>% 
  select(cnpj,
         nome_fantasia,
         razao_social,
         status_incentivada,
         tributacao_2023,
         data_de_inicio_atividade,
         situacao_cadastral,
         data_situacao_cadastral,
         municipio,
         uf,
         ddd_1,
         telefone_1,
         ddd_2,
         telefone_2,
         correio_eletronico,
         tipo_de_logradouro,
         logradouro,
         numero,
         complemento,
         bairro,
         cep,
         capital_social,
         cnae_principal = cnae_com_nome,
         cnae_secundaria = cnae_fiscal_secundaria) %>% 
  write_rds(here('data/processed/lista_estab_empresas.rds'))

lista_estab_empresas <- read_rds(here('data/processed/lista_estab_empresas.rds'))


