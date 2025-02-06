# Criacao do DB de divida ativa da uniao
# Renato Gomes Chaves
# Criação: 06/2024

# Pacotes ======================================================================
{
  # install.packages('pacman')
  library(pacman)
  # Sys.unsetenv("GITHUB_PAT")
  # remotes::install_github("georgevbsantiago/qsacnpj")
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

# juntar os arquivos e filtrar apenas empresas dos estados abaixo
foreach(file_list = list.files(here('Data/dados_pgrf_dau'),
                               pattern = '^arquivo_lai_SIDA'),
        .combine = 'rbind',
        estados_norte_nordeste = c('MG', 'BA', 'GO', 'PE', 'CE', 'MT', 'ES',
                                   'PA', 'MA', 'RN', 'PB', 'AL', 'AM', 'PI',
                                   'SE', 'RO', 'TO', 'AC', 'AP', 'RR')
        ) %do% {
          read_delim(paste0('Data/dados_pgrf_dau/', file_list),
                     col_types = cols(.default = "c"),
                     delim = ";",
                     # n_max = 100000,
                     escape_double = FALSE,
                     locale = locale(encoding = "ISO-8859-2"),
                     trim_ws = TRUE) %>% 
            janitor::clean_names()  %>% 
            filter(uf_devedor %in% c('MG', 'BA', 'GO', 'PE', 'CE',
                                     'MT', 'ES', 'PA', 'MA', 'RN',
                                     'PB', 'AL', 'AM', 'PI', 'SE',
                                     'RO', 'TO', 'AC', 'AP', 'RR'),
                   str_detect(tipo_pessoa, 'dica$'),
                   str_detect(tipo_devedor, 'PRINCIPAL')
            ) %>% 
            mutate(across(everything(),
                          ~ stringi::stri_trans_general(., "latin-ascii")),
                   cnpj = str_remove_all(cpf_cnpj, "[\\.\\/\\-]"),
                   valor_consolidado = as.double(valor_consolidado)
            )
        } %>%
  write_rds(here('data/processed/dividas_ativas_cnpj_norte_nordeste.rds'))
