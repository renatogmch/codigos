# Leitura dos dados de laudos concedidos pela SUDAM
# Dados do site da SUDAM or ano, em PDF
# Foram convertidos em xlsx manualmente e manipulados
# Renato Gomes Chaves
# Criação: 18/06/2024

# Pacotes ======================================================================
{
  # install.packages('pacman')
  library(pacman)
  remotes::install_github("georgevbsantiago/qsacnpj")
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

# carregar dados ===============================================================
# dados de 2010 a 2020
sudam_01 <- foreach(files = list.files(here('Data/dados_sudam/excel'), pattern = "^Rela"),
                    year = c('2010',
                             '2011',
                             '2012',
                             '2012',
                             '2014',
                             '2015',
                             '2016',
                             '2017',
                             '2018',
                             '2019',
                             '2020'),
                    .combine = rbind
) %do% {
  read_excel(paste0('Data/dados_sudam/excel/', files),
             col_types = 'text') %>% 
    janitor::clean_names() %>% 
    mutate(across(everything(), ~ stringi::stri_trans_general(., 'latin-ascii')),
           ano = year,
           cnpj_mf = str_remove_all(cnpj_mf, "[\\.\\/\\-]")
    ) %>% 
    select(cnpj = cnpj_mf,
           empresa,
           ano,
           modalidade,
           municipio,
           uf,
           produto_servico
    )
}

# dados de 2020 a 2023
sudam_02 <- foreach(files = list.files(here('Data/dados_sudam/excel'), pattern = "^Planilha - Aprovados 20"),
                    year = c('2021',
                             '2022',
                             '2023'),
                    .combine = rbind
) %do% {
  read_excel(paste0('Data/dados_sudam/excel/', files),
             col_types = 'text') %>% 
    janitor::clean_names() %>% 
    mutate(across(everything(), ~ stringi::stri_trans_general(., 'latin-ascii')),
           across(contains('cnpj'), ~ str_remove_all(., "[\\.\\/\\-]")),
           ano = year
    ) %>% 
    select(cnpj = contains('cnpj'),
           empresa,
           ano,
           modalidade,
           municipio,
           uf,
           produto_servico
    )
}

# empilhando os dados e salvando o resultado
bind_rows(sudam_01, sudam_02) %>% 
  mutate(prazo_do_beneficio = paste0(as.double(ano), ' a ', as.double(ano) + 10)
  ) %>% 
  write_rds(here('data/processed/incentivos_sudam_2010a2023.rds'))
