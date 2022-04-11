# Leitura dos dados rais estabelecimentos
# 24/10/2019

{
  packages = c("tidyverse",
               "here",
               "foreach",
               "doParallel"
  )
  package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  no_cores <- detectCores() - 2
  registerDoParallel(cores=no_cores)
  rm(package.check, packages, no_cores)
  gc()
}

# RAIS Estab ----
c(read_lines('data/raw/estab/Estb2006.txt', n_max=1, locale = locale(encoding = "ISO-8859-1")),
  read_lines('data/raw/estab/Estb2008.txt', n_max=1, locale = locale(encoding = "ISO-8859-1")),
  read_lines('data/raw/estab/Estb2010.txt', n_max=1, locale = locale(encoding = "ISO-8859-1")),
  read_lines('data/raw/estab/Estb2012.txt', n_max=1, locale = locale(encoding = "ISO-8859-1")),
  read_lines('data/raw/estab/Estb2014.txt', n_max=1, locale = locale(encoding = "ISO-8859-1")),
  read_lines('data/raw/estab/Estb2016id.txt', n_max=1, locale = locale(encoding = "ISO-8859-1"))
)

# 2006 a 2010 ----
foreach(files = here('data/raw/estab') %>% list.files(pattern = '[06|08|10].[t|T][x|X][t|T]'),
        delims = c('|','|',';'),
        year = c('2006','2008','2010'),
        .packages = c('dplyr','here','readr')) %dopar% {
           files %>%
             as.character() %>%
             paste0('data/raw/estab/',.) %>%
            # Leitura dos dados brutos, apenas as colunas selecionadas em cols_only
             read_delim(delim = delims, 
                        escape_double = FALSE, 
                        locale = locale(encoding = "ISO-8859-1"), 
                        trim_ws = TRUE,
                        # n_max = 100,
                        col_types = cols_only(IDENTIFICAD = 'c',
                                              MUNICIPIO = 'c',
                                              ESTOQUE = 'i',
                                              TAMESTAB = 'i',
                                              `IND ATIV ANO` = 'i',
                                              `CLAS CNAE 20` = 'c')
                        ) %>%
            # Muda os nomes das colunas
             setNames(c('cnpj',
                        'cod_mun',
                        'vinculos',
                        'tam_estab',
                        'ind_ativ',
                        'cnae')
                      ) %>%
            # Salva os resultados indiviruais em .rds
            saveRDS(here(paste0('/data/processed/estab',year,'.rds')))
        }

# 2012 a 2016 ----
foreach(files = here('data/raw/estab') %>% list.files(pattern = '[12|14|ID].[t|T][x|X][t|T]'),
        year = c('2012','2014','2016'),
        .packages = c('dplyr','here','readr')) %dopar% {
          files %>%
            as.character() %>%
            paste0('data/raw/estab/',.) %>%
            # Leitura dos dados brutos, apenas as colunas selecionadas em cols_only
            read_delim(delim = ';', 
                       escape_double = FALSE, 
                       locale = locale(encoding = "ISO-8859-1"), 
                       trim_ws = TRUE,
                       col_types = cols_only(`CNPJ / CEI` = 'c',
                                             Município = 'c',
                                             `Qtd Vínculos Ativos` = 'i',
                                             `Tamanho Estabelecimento` = 'i',
                                             `Ind Atividade Ano` = 'i',
                                             `CNAE 2.0 Classe` = 'c')
            ) %>%
            # Muda os nomes das colunas
            setNames(c('cnpj',
                       'cod_mun',
                       'vinculos',
                       'tam_estab',
                       'ind_ativ',
                       'cnae')
            ) %>%
            # Salva os resultados indiviruais em .rds
            saveRDS(here(paste0('/data/processed/estab',year,'.rds')))
        }
