# Leitura dos dados rais trabalhadores
# 22/10/2019

{
  options(repos='http://cran.rstudio.com/')
  require(pacman)
  p_load(char = c("tidyverse",
                  "readstata13",
                  "here",
                  "foreach",
                  "doParallel"
                  )
  )
  gc()
  no_cores <- detectCores()
  registerDoParallel(cores=no_cores)
  # stopImplicitCluster()
  # aumentar max mem ram com pagefile
  utils::memory.limit(32000)
  print(paste0('Memoria RAM maxima: ', memory.limit()))
  print(paste0('Cores utilizados: ', no_cores))
  rm(no_cores)
}


# Variaveis das bases RAIS Trab ------------------------------------------------
{
  spec(read_delim('data/raw/trab/2006/sp06.txt','; ', n_max=1, locale = locale(encoding = "ISO-8859-1", decimal_mark = ',')))
  spec(read_delim('data/raw/trab/2008/BA2008ID.txt',';' ,n_max=1, locale = locale(encoding = "ISO-8859-1", decimal_mark = ',')))
  spec(read_delim('data/raw/trab/2008/sp08 1.txt',';' ,n_max=1, locale = locale(encoding = "ISO-8859-1", decimal_mark = ',')))
  spec(read_delim('data/raw/trab/2010/SP2010ID1.txt',';' ,n_max=1, locale = locale(encoding = "ISO-8859-1", decimal_mark = ',')))
  spec(read_delim('data/raw/trab/2012/SP2012ID.txt',';' ,n_max=1, locale = locale(encoding = "ISO-8859-1", decimal_mark = ',')))
  here('data/raw/trab/Bases da RAIS Stata/rais2014.dta') %>% read.dta13(encoding = "ISO-8859-1", select.cols = 'cpf') %>% spec() %>% system.time()
  spec(read_delim('data/raw/trab/2016/SP2017ID.txt',';' ,n_max=1, locale = locale(encoding = "ISO-8859-1", decimal_mark = ','))) #%>% system.time()
}

# As bases de dados dos trabalhadores sao enormes e estao divididas em arquivos
# individuais entre regioes (em 2006) ou estados (2008 adiante). Por motivos de
# RAM, cada ano requer uma funcao individual para carregamento e manipulacao, 
# sendo o resultado de cada iteracao em loop "foreach" salvo arquivo em 
# individual de nome trab[ANO].rds


# RAIS Trab 2006 ---------------------------------------------------------------
# Leitura de cada arquivo de txt individual na pasta 2006
foreach(files = list.files('data/raw/trab/2006', pattern = '.TXT'),
        .packages = c('tidyverse','here')) %dopar% {
             files %>%
             as.character() %>%
             paste0('data/raw/trab/2006/',.) %>%
             # Leitura dos dados selecionando as variaves em cols_only
             read_delim(delim = ";",
                      escape_double = FALSE,
                      locale = locale(encoding = "ISO-8859-1", decimal_mark = ','),
                      trim_ws = TRUE,
                      # n_max = 100,
                      col_types = cols_only(IDENTIFICAD = 'c',
                                            TAMESTAB = 'd',
                                            CPF = 'c',
                                            MUNICIPIO = 'c',
                                            `REM MED (R$)` = 'd',
                                            `GRAU INSTR` = 'd',
                                            `CLAS CNAE 20` = 'c')) %>%
             # Renomeando e reordenando as variaveis
             select('cnpj' = IDENTIFICAD,
                    'tam_estab' = TAMESTAB,
                    'cod_mun' = MUNICIPIO,
                    'cpf' = CPF,
                    'rem_med' = `REM MED (R$)`,
                    'grau_instr' = `GRAU INSTR`,
                    'cnae' = `CLAS CNAE 20`) %>%
            write_rds(here(paste0('data/processed/trab_individual/2006/',files,'_processed.rds')))
        } 
gc()
# Re-ler cada arquivo rds indidivual e junta-los em um unico
foreach(files = list.files('data/processed/trab_individual/2006', pattern = '.rds'),
        .packages = c('tidyverse','here')) %do% {
          files %>%
            as.character() %>%
            paste0('data/processed/trab_individual/2006/',.) %>%
            read_rds()
        } %>%
  # Combina as linhas da lista que foi resultado do foreach anterior
  bind_rows() %>%
  write_rds(here('data/processed/trab2006.rds'))


# RAIS Trab 2008 ---------------------------------------------------------------
# Leitura de cada arquivo de txt individual na pasta 2008
# todos os arquivos "**2008ID.TXT" tem variaves com mesmos nomes, mas "sp08" tem nomes diferentes
# primeiro para os arquivos "**2008ID.TXT"
foreach(files = list.files('data/raw/trab/2008', pattern = '08ID.TXT$'),
        .packages = c('tidyverse','here')) %dopar% {
          files %>%
            as.character() %>%
            paste0('data/raw/trab/2008/',.) %>%
            # Leitura dos dados selecionando as variaves em cols_only
            read_delim(delim = ";",
                       escape_double = FALSE,
                       locale = locale(encoding = "ISO-8859-1", decimal_mark = ','),
                       trim_ws = TRUE,
                       # n_max = 100,
                       col_types = cols_only(IDENTIFICAD = 'c',
                                             TAMESTAB = 'd',
                                             CPF = 'c',
                                             MUNICIPIO = 'c',
                                             `REM MED (R$)` = 'd',
                                             `GR INSTRUCAO` = 'd',
                                             `CLAS CNAE 20` = 'c')
            ) %>%
            # Renomeando e reordenando as variaveis
            select('cnpj' = IDENTIFICAD,
                   'tam_estab' = TAMESTAB,
                   'cod_mun' = MUNICIPIO,
                   'cpf' = CPF,
                   'rem_med' = `REM MED (R$)`,
                   'grau_instr' =`GR INSTRUCAO`,
                   'cnae' = `CLAS CNAE 20`) %>%
            write_rds(here(paste0('data/processed/trab_individual/2008/',files,'_processed.rds')))
        }
# agora para os "sp 08"
foreach(files = list.files('data/raw/trab/2008', pattern = '^sp08'),
        .packages = c('tidyverse','here')) %dopar% {
          files %>%
            as.character() %>%
            paste0('data/raw/trab/2008/',.) %>%
            # Leitura dos dados selecionando as variaves em cols_only
            read_delim(delim = ";",
                       escape_double = FALSE,
                       locale = locale(encoding = "ISO-8859-1", decimal_mark = ','),
                       trim_ws = TRUE,
                       # n_max = 100,
                       col_types = cols_only(IDENTIFICAD = 'c',
                                             TAMESTAB = 'd',
                                             CPF = 'c',
                                             MUNICIPIO = 'c',
                                             `REM MED (R$)` = 'd',
                                             `GRAU INSTR` = 'd',
                                             `CLAS CNAE 20` = 'c')
            ) %>%
            # Renomeando e reordenando as variaveis
            select('cnpj' = IDENTIFICAD,
                   'tam_estab' = TAMESTAB,
                   'cod_mun' = MUNICIPIO,
                   'cpf' = CPF,
                   'rem_med' = `REM MED (R$)`,
                   'grau_instr' =`GRAU INSTR`,
                   'cnae' = `CLAS CNAE 20`) %>%
            write_rds(here(paste0('data/processed/trab_individual/2008/',files,'_processed.rds')))
        }
# Re-ler cada arquivo rds indidivual e junta-los em um unico
foreach(files = list.files('data/processed/trab_individual/2008', pattern = '.rds'),
        .packages = c('tidyverse','here')) %dopar% {
          files %>%
            as.character() %>%
            paste0('data/processed/trab_individual/2008/',.) %>%
            read_rds()
        } %>%
  # Combina as linhas da lista que foi resultado do foreach anterior
  bind_rows() %>%
  write_rds(here('data/processed/trab2008.rds'))

# RAIS Trab 2010 ---------------------------------------------------------------
# Leitura de cada arquivo de txt individual na pasta 2010
# todos os arquivos "**2008ID.TXT" tem variaves com mesmos nomes, mas "Sp" tem 2 IDs
foreach(files = list.files('data/raw/trab/2010', pattern = '.TXT$'),
        .packages = c('tidyverse','here')) %dopar% {
          files %>%
            as.character() %>%
            paste0('data/raw/trab/2010/',.) %>%
            # Leitura dos dados selecionando as variaves em cols_only
            read_delim(delim = ";",
                       escape_double = FALSE,
                       locale = locale(encoding = "ISO-8859-1", decimal_mark = ','),
                       trim_ws = TRUE,
                       # n_max = 100, #limita as linhas lidas para testes
                       col_types = cols_only(IDENTIFICAD = 'c',
                                             TAMESTAB = 'd',
                                             CPF = 'c',
                                             MUNICIPIO = 'c',
                                             `REM MED (R$)` = 'd',
                                             `GR INSTRUCAO` = 'd',
                                             # para 2010, so tinha cnae subclasse
                                             `SB CLAS 20` = 'c')
            ) %>%
            # Renomeando e reordenando as variaveis
            transmute('cnpj' = IDENTIFICAD,
                       'tam_estab' = TAMESTAB,
                       'cod_mun' = MUNICIPIO,
                       'cpf' = CPF,
                       'rem_med' = `REM MED (R$)`,
                       'grau_instr' =`GR INSTRUCAO`,
                      # removendo os dois ultimos digitos da subclasse pra virar classe
                       'cnae' = str_remove(.$`SB CLAS 20`,'[0-9][0-9]$')) %>%
            write_rds(here(paste0('data/processed/trab_individual/2010/',files,'_processed.rds')))
        } 
# Re-ler cada arquivo rds indidivual e junta-los em um unico
foreach(files = list.files('data/processed/trab_individual/2010', pattern = '.rds'),
        .packages = c('tidyverse','here')) %dopar% {
          files %>%
            as.character() %>%
            paste0('data/processed/trab_individual/2010/',.) %>%
            read_rds()
        } %>%
  # Combina as linhas da lista que foi resultado do foreach anterior
  bind_rows() %>%
  write_rds(here('data/processed/trab2010.rds'))

# RAIS Trab 2012 ---------------------------------------------------------------
foreach(files = list.files('data/raw/trab/2012', pattern = '2012ID.txt'),
        .packages = c('tidyverse','here')) %dopar% {
          files %>%
            as.character() %>%
            paste0('data/raw/trab/2012/',.) %>%
            # Leitura dos dados selecionando as variaves em cols_only
            read_delim(delim = ";",
                       escape_double = FALSE,
                       locale = locale(encoding = "ISO-8859-1", decimal_mark = ','),
                       trim_ws = TRUE,
                       # n_max = 100, #limita as linhas lidas para testes
                       col_types = cols_only(`CNPJ / CEI` = 'c',
                                             `Tamanho Estabelecimento` = 'd',
                                             CPF = 'c',
                                             Município  = 'c',
                                             `Vl Remun Média Nom` = 'd',
                                             `Escolaridade após 2005` = 'c',
                                             `CNAE 2.0 Classe` = 'c')
            ) %>%
            # Renomeando e reordenando as variaveis
            transmute('cnpj' = `CNPJ / CEI`,
                      'tam_estab' = `Tamanho Estabelecimento`,
                      'cod_mun' = Município,
                      'cpf' = CPF,
                      'rem_med' = `Vl Remun Média Nom`,
                      'grau_instr' = as.double(.$`Escolaridade após 2005`),
                      'cnae' = `CNAE 2.0 Classe`
                      ) %>%
          write_rds(here(paste0('data/processed/trab_individual/2012/',files,'_processed.rds')))
        } 
# Re-ler cada arquivo rds indidivual e junta-los em um unico
foreach(files = list.files('data/processed/trab_individual/2012', pattern = '.rds'),
        .packages = c('tidyverse','here')) %dopar% {
          files %>%
            as.character() %>%
            paste0('data/processed/trab_individual/2012/',.) %>%
            read_rds()
        } %>%
  # Combina as linhas da lista que foi resultado do foreach anterior
  bind_rows() %>%
  write_rds(here('data/processed/trab2012.rds'))

# RAIS Trab 2014 ---------------------------------------------------------------
# Este ano esta disponivel apenas em arquivo .dta
# spec(read_delim('data/processed/trab_individual/2014/trab2014.txt',';' ,n_max=1, locale = locale(encoding = "ISO-8859-1", decimal_mark = ',')))
here('data/raw/trab/Bases da RAIS Stata/rais2014.dta') %>% 
  read.dta13(select.cols = c('municpio',
                              'escolaridadeaps2005',
                              'tamanhoestabelecimento',
                              'vlremunmdianom',
                              'cpf',
                              'cnpjcei',
                              'cnae20classe')
                              ) %>%
  as_tibble() %>%
  transmute('cnpj' = as.character(cnpjcei),
             'tam_estab' = as.double(tamanhoestabelecimento),
             'cod_mun' = as.character(municpio),
             'cpf' = as.character(cpf),
            # gsub substitui virgula por ponto pra sep decimal
             'rem_med' = as.double(gsub(',','\\.',vlremunmdianom)),
             'grau_instr' = as.double(escolaridadeaps2005),
             'cnae' = as.character(cnae20classe)
  ) %>%
  write_rds(here('data/processed/trab2014.rds'))


read_rds('data/processed/trab_indidivual/2014/trab2014.rds') %>% 
  # O group_by agrupa a base de trabalhadores por firmas
  group_by(cnpj) %>%
  # Criacao das novas variaves
  summarise(tam_estab = min(tam_estab),
            cod_mun = min(cod_mun),
            # vinculos eh a contagem de linhas (cpfs) para cada cnpj
            vinculos = n(),
            # remuneracao media é a soma das remuneracoes medias de cada funcionario 
            # dividido pelo num de funcionarios
            rem_med = sum(rem_med)/n(),
            # pro_superior eh a proporcao de funcionarios com ensino superior do total
            prop_superior = length(cpf[grau_instr>7])/length(cpf),
            cnae = min(cnae)
  ) %>%
  write_rds(here(paste0('data/processed/firmas_','2014','.rds')))

# RAIS Trab 2016 ---------------------------------------------------------------
# Estava faltando SP2016. Acabei tendo que usar SP2017 a contragosto.
foreach(files = list.files('data/raw/trab/2016', pattern = '.txt'),
        .packages = c('tidyverse','here')) %dopar% {
          files %>%
            as.character() %>%
            paste0('data/raw/trab/2016/',.) %>%
            # Leitura dos dados selecionando as variaves em cols_only
            read_delim(delim = ";",
                       escape_double = FALSE,
                       locale = locale(encoding = "ISO-8859-1", decimal_mark = ','),
                       trim_ws = TRUE,
                       # n_max = 100, #limita as linhas lidas para testes
                       col_types = cols_only(`CNPJ / CEI` = 'c',
                                             `Tamanho Estabelecimento` = 'd',
                                             CPF = 'c',
                                             Município  = 'c',
                                             `Vl Remun Média Nom` = 'd',
                                             `Escolaridade após 2005` = 'd',
                                             `CNAE 2.0 Classe` = 'c')
            ) %>%
            # Renomeando e reordenando as variaveis
            select('cnpj' = `CNPJ / CEI`,
                   'tam_estab' = `Tamanho Estabelecimento`,
                   'cod_mun' = Município,
                   'cpf' = CPF,
                   'rem_med' = `Vl Remun Média Nom`,
                   'grau_instr' =`Escolaridade após 2005`,
                   'cnae' = `CNAE 2.0 Classe`
            ) %>%
            write_rds(here(paste0('data/processed/trab_individual/2016/',files,'_processed.rds')))
        } 
# Re-ler cada arquivo rds indidivual e junta-los em um unico
foreach(files = list.files('data/processed/trab_individual/2016', pattern = '.rds'),
        .packages = c('tidyverse','here')) %do% {
          files %>%
            as.character() %>%
            paste0('data/processed/trab_individual/2016/',.) %>%
            read_rds()
        } %>%
  # Combina as linhas da lista que foi resultado do foreach anterior
  bind_rows() %>%
  write_rds(here('data/processed/trab2016.rds'))


# Manipulacao ------------------------------------------------------------------
# Summarise por CNPJ, criacao de variaveis por ano
foreach(files = here('data/processed/') %>% list.files(pattern = '^trab20'),
        year = c('2006','2008','2010','2012','2014','2016'),
        .packages = c('here','tidyverse')
        ) %do% {
          files %>%
            paste0('data/processed/',.) %>%
            read_rds() %>% 
            # O group_by agrupa a base de trabalhadores por firmas
            group_by(cnpj) %>%
            # Criacao das novas variaves
            summarise(tam_estab = min(tam_estab),
                      cod_mun = min(cod_mun),
                      # vinculos eh a contagem de linhas (cpfs) para cada cnpj
                      vinculos = n(),
                      # remuneracao media eh a soma das remuneracoes anuais
                      # medias de cada funcionario 
                      # dividido pelo num de funcionarios
                      rem_med = sum(rem_med)/n(),
                      # simples total de trabalhadores com superior
                      total_superior = length(cpf[grau_instr>7]),
                      # prop_superior eh a proporcao de funcionarios com ensino
                      # superior do total
                      prop_superior = length(cpf[grau_instr>7]) / length(cpf),
                      cnae = min(cnae)
            ) %>%
            write_rds(here(paste0('data/processed/firmas_',year,'.rds')))
        }

# juntar cada firmas_[ANO] criadas acima para criar o DB definitivo
{
  foreach(files = here('data/processed/') %>%
                      #^ significa 'comeca com...'
                      list.files(pattern = '^firmas_'),
                    year = c('2006','2008','2010','2012','2014','2016'),
                    .packages = c('tidyverse','here')
  ) %dopar% {
    files %>%
      paste0('data/processed/',.) %>% # local do arquivo
      read_rds() %>%
      # renomear as colunas que variam anualmente.
      # o !! serve para pegar o resultado do paste sem ser string
      # o operador ':=' n sei pra q serve. mais infos:
      # https://github.com/tidyverse/dplyr/issues/1600#issuecomment-346419050
      # o duplo '_' serve de separator
      rename(!!paste0('vinculos__',year) := vinculos,
             !!paste0('rem_med__',year) := rem_med,
             !!paste0('total_superior__',year) := total_superior,
             !!paste0('prop_superior__',year) := prop_superior,
             !!paste0('tam_estab__',year) := tam_estab)
  } %>%
    set_names(c('2006',
                '2008',
                '2010',
                '2012',
                '2014',
                '2016')) %>% 
  write_rds(., here('data/processed/firmas.rds'))
}