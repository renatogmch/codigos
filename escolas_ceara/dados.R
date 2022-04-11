# Script para load dos dados ----
{
  require(pacman)
  p_load("here",
         "tidyverse",
         "pipeR",
         "readr",
         "data.table",
         "doParallel",
         "foreach",
         'janitor',
         'sidrar'
         )
  no_cores <- floor(detectCores()*0.75)
  registerDoParallel(cores = no_cores)
}

# IDEB #########################################################################
foreach(file = here('data/raw') %>%
          list.files(pattern = "^divulgacao_anos_"),
        .packages = c('tidyverse',
                      'here',
                      'janitor')
) %dopar% {
  file %>% 
    as.character() %>%
    paste0('data/raw/', .) %>% 
    # importar tudo como chr
    readxl::read_excel(na = "-",
                       col_types = 'text') %>% 
    # ajeitar o nome das vars
    clean_names() %>%
    # tirar notas de rodape selecionando apenas celulas que tenham
    # a sigla da uf
    filter(str_detect(sigla_da_uf, '^[A-Z][A-Z]$')) %>% 
    mutate(across(starts_with('ideb_'), as.numeric)) %>% 
    setNames(c('uf',
               'cod_mun',
               'nome_mun',
               'cod_escola',
               'nome_escola',
               'rede',
               'ideb__2005',
               'ideb__2007',
               'ideb__2009',
               'ideb__2011',
               'ideb__2013',
               'ideb__2015',
               'ideb__2017',
               'ideb__2019')
    )
} %>%
  # ordem dos nomes dos elementos da lista esta nessa ordem pq eh como o
  # foreach ve os arquivos na pasta
  set_names(c('finais','iniciais')) %>% 
  # lapply pra criar as vars de crescimento(growth) em ambos os dfs
  lapply(. %>%
           mutate(growth__2005=ideb__2007/ideb__2005,
                  growth__2007=ideb__2009/ideb__2007,
                  growth__2009=ideb__2011/ideb__2009,
                  growth__2011=ideb__2013/ideb__2011,
                  growth__2013=ideb__2015/ideb__2013,
                  growth__2015=ideb__2017/ideb__2015,
                  growth__2017=ideb__2019/ideb__2017
           )
  ) %>%
  saveRDS(here('data/processed/ideb.rds'))

# ESCOLAS ######################################################################
# processamento dos dados do censo escolar
foreach(escolas = here('data/raw') %>%
          list.files(pattern = "CENSOESC_20[0-1][0-9][.]CSV$"),
        n = c('2007','2009','2011','2013','2015','2017', '2019'),
        .packages=c('tidyverse','here')
  ) %dopar% {
   # print(n)
   escolas %>%
     as.character() %>%
     paste0('data/raw/', .) %>%
     read_delim("|",
                # n_max = 10000,
                col_types = cols(.default = "c"),
                escape_double = FALSE,
                locale = locale(encoding = "UTF-8"),
                trim_ws = TRUE) %>%
     # as_tibble() %>%
     select(
       # renomear as colunas que variam anualmente.
       # o !! serve para pegar o resultado do paste sem ser string
       # := operator in data.table to add the columns by reference (https://stackoverflow.com/a/7914941)
       # https://github.com/tidyverse/dplyr/issues/1600#issuecomment-346419050
       # https://www.rdocumentation.org/packages/data.table/versions/1.12.8/topics/%3A%3D
       # o duplo '_' serve de separator
            # Codigo da escola
            cod_escola = matches('PK_COD_ENTIDADE|CO_ENTIDADE'),
            # Codigo do municipio
            cod_mun = matches('FK_COD_MUNICIPIO|CO_MUNICIPIO'),
            # Localizacao da escola (rural/urbana)
            !!paste0('fc_localizacao__', n) := matches('ID_LOCALIZACAO$|TP_LOCALIZACAO$'),
            # Dependencia adminstrativa da escola (publica/privada). fc = factor
            !!paste0('fc_dependencia_adm__', n) := matches('ID_DEPENDENCIA_ADM|TP_DEPENDENCIA$'),
            # Se tem o nao agua
            !!paste0('id_agua__', n) := contains('AGUA_INEXISTENTE'),
            # se tem ou nao energia
            !!paste0('id_energia__', n) := contains('ENERGIA_INEXISTENTE'),
            # se tem ou nao esgoto
            !!paste0('id_esgoto__', n) := contains('ESGOTO_INEXISTENTE'),
            # se tem ou nao internet
            !!paste0('id_internet__', n) := matches('ID_INTERNET|IN_INTERNET$'),
            # se tem ou nao biblioteca
            !!paste0('id_biblioteca__', n) := matches('ID_BIBLIOTECA|IN_BIBLIOTECA$'),
            # se tem ou nao cozinha
            !!paste0('id_cozinha__', n) := contains('COZINHA'),
            # se tem ou nao alimentacao para os alunos
            !!paste0('id_alimentacao__', n) := matches('ID_ALIMENTACAO|IN_ALIMENTACAO$'),
            # se tem ou nao laboratorio de ciencias
            !!paste0('id_lab_ciencias__', n) := contains('LABORATORIO_CIENCIAS'),
            # se tem ou nao laboratorio de informatica
            !!paste0('id_lab_inform__', n) := contains('LABORATORIO_INFORMATICA'),
            # Numero de PCs disponivel para os alunos
            !!paste0('nu_comp_alunos__', n) := matches('NUM_COMP_ALUNOS|NU_COMP_ALUNO|QT_DESKTOP_ALUNO'),
            # Numero de funcionarios da escola
            !!paste0('nu_funcionarios__', n) := contains(c('QT_PROF_', 'FUNCIONARIO')),
            # Numeto de salas uitilizadas para aula
            !!paste0('nu_salas__', n) := matches('NUM_SALAS_UTILIZADAS|NU_SALAS_UTILIZADAS|QT_SALAS_UTILIZADAS$')
    ) %>% 
    # transformar de chr em num
    mutate(across(starts_with('nu_'), as.numeric)) %>% 
    # em 2019 tem varias vars que discriminam numero de funcionarios
    # portanto o mutate so afeta o elemento `2019` da lista
    # criando uma var que Ã© a soma de todas 'nu_funcionarios__[ANO]'
    mutate(!!paste0('nu_funcionarios__', n) := rowSums(across(starts_with('nu_funcionarios__'))),
          .keep = 'unused') %>% 
    # recode nos factors
    mutate(across(starts_with('fc_dependencia_adm'), ~ recode(.,
                                                        '1' = 'Federal',
                                                        '2' = 'Estadual',
                                                        '3' = 'Municipal',
                                                        '4' = 'Privada')
                  ),
           across(starts_with('fc_localizacao'), ~ recode(.,
                                                          '1' = 'Urbana',
                                                          '2' = 'Rural')
                  ),
           # inverter 0's e 1's nas vars dummy agua, esgoto e energia
           # as vars sao para casos contararios, ou seja,
           # id_agua = ID_AGUA_INEXISTENTE = 0 significava que tem agua encanada
           # na escola
           across(contains(c('id_agua',
                             'id_energia',
                             'id_esgoto')),
                  ~ case_when(. == '1' ~ 'Nao Possui',
                              . == '0' ~ 'Possui')
                  )
           )
} %>% 
  set_names(c('2007','2009','2011','2013','2015','2017', '2019')) %>% 
  saveRDS(here('data/processed/escolas.rds'))

# TURMAS #######################################################################
foreach(i = here('data/raw') %>% 
          list.files(pattern = "TURMAS_20[0-1][0-9][.]CSV$"),
        n = list('2007','2009','2011','2013','2015','2017', '2019'),
        .packages=c('tidyverse','here')) %dopar% {
                    i %>%
                      as.character() %>%
                      paste0('data/raw/',.) %>%
                      read_delim("|",
                                 # n_max = 1000,
                                 col_types = cols(.default = "c"),
                                 escape_double = FALSE,
                                 locale = locale(encoding = "UTF-8"),
                                 trim_ws = TRUE) %>%
                      as_tibble() %>%
                      select(cod_escola = matches('PK_COD_ENTIDADE|CO_ENTIDADE'),
                             cod_turma = matches('PK_COD_TURMA|ID_TURMA'),
                             !!paste0('nu_matriculas__',n) := matches('NUM_MATRICULAS|NU_MATRICULAS|QT_MATRICULAS')
                      ) %>% 
            # transformar de chr em num
            mutate(across(starts_with('nu_'), as.numeric))
                  } %>%
  set_names(c('2007','2009','2011','2013','2015','2017', '2019')) %>%
  saveRDS(here('data/processed/turmas.rds'))

# DOCENTES POR REGIAO ##########################################################
# metodo: contar numero de docentes superiores e dividir pelo total
# de docentes por escola por ano. Identificador de superior muda por ano:
# 2007-2016, superior >= 6
# 2015-2019, superior == 4
# nome da var: prop_docentes_superior
{
  # Nordeste ===================================================================
  foreach(norte = here('data/raw') %>%
            list.files(pattern = "DOCENTES_NORDESTE_20[0-1][0-9][.]CSV$"),
          year = c('2007','2009','2011','2013','2015','2017', '2019'),
          .packages=c('readr','dplyr','here')
  ) %do% {
    print(year)
    norte %>%
      as.character() %>%
      paste0('data/raw/',.) %>%
      read_delim("|",
                 # n_max = 1000,
                 escape_double = FALSE,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = "UTF-8"),
                 trim_ws = TRUE) %>%
      select(cod_escola = matches('PK_COD_ENTIDADE|CO_ENTIDADE'),
             cod_docente = matches('FK_COD_DOCENTE|CO_PESSOA_FISICA|ID_DOCENTE'),
             escolaridade = matches('FK_COD_ESCOLARIDADE|TP_ESCOLARIDADE')
      ) %>% 
      # transformar de chr em num
      mutate(across(matches('escolaridade'), as.numeric)) %>% 
      # criar var de prop docentes superior por escola
      group_by(cod_escola) %>% # Passo seguinte lida com a mudanca na variavel superior. (ver anexo dos dados)
      summarise(!!paste0('prop_docentes_superior__', year) := case_when(year == '2015' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2017' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2019' ~ length(cod_docente[escolaridade==4]),
                                                                        TRUE ~ length(cod_docente[escolaridade>=6])
                                                                        )/length(cod_docente)
      )
  } %>%
    set_names(c('2007','2009','2011','2013','2015','2017', '2019')) %>%
    write_rds(file = here('data/processed/docentes_reg_ne.rds'))
  
  
  
  # Norte ======================================================================
  foreach(norte = here('data/raw') %>%
            list.files(pattern = "DOCENTES_NORTE_20[0-1][0-9][.]CSV$"),
          year = c('2007','2009','2011','2013','2015','2017', '2019'),
          .packages=c('readr','dplyr','here')
  ) %do% {
    print(year)
    norte %>%
      as.character() %>%
      paste0('data/raw/',.) %>%
      read_delim("|",
                 # n_max = 1000,
                 escape_double = FALSE,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = "UTF-8"),
                 trim_ws = TRUE) %>%
      select(cod_escola = matches('PK_COD_ENTIDADE|CO_ENTIDADE'),
             cod_docente = matches('FK_COD_DOCENTE|CO_PESSOA_FISICA|ID_DOCENTE'),
             escolaridade = matches('FK_COD_ESCOLARIDADE|TP_ESCOLARIDADE')
      ) %>% 
      # transformar de chr em num
      mutate(across(matches('escolaridade'), as.numeric)) %>% 
      # criar var de prop docentes superior por escola
      group_by(cod_escola) %>% # Passo seguinte lida com a mudanca na variavel superior. (ver anexo dos dados)
      summarise(!!paste0('prop_docentes_superior__', year) := case_when(year == '2015' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2017' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2019' ~ length(cod_docente[escolaridade==4]),
                                                                        TRUE ~ length(cod_docente[escolaridade>=6])
                                                                        )/length(cod_docente)
      )
  } %>%
    set_names(c('2007','2009','2011','2013','2015','2017', '2019')) %>%
    write_rds(file = here('data/processed/docentes_reg_nt.rds'))
  
  
  # Centro-Oeste ===============================================================
  foreach(norte = here('data/raw') %>%
            list.files(pattern = "DOCENTES_CO_20[0-1][0-9][.]CSV$"),
          year = c('2007','2009','2011','2013','2015','2017', '2019'),
          .packages=c('readr','dplyr','here')
  ) %do% {
    print(year)
    norte %>%
      as.character() %>%
      paste0('data/raw/',.) %>%
      read_delim("|",
                 # n_max = 1000,
                 escape_double = FALSE,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = "UTF-8"),
                 trim_ws = TRUE) %>%
      select(cod_escola = matches('PK_COD_ENTIDADE|CO_ENTIDADE'),
             cod_docente = matches('FK_COD_DOCENTE|CO_PESSOA_FISICA|ID_DOCENTE'),
             escolaridade = matches('FK_COD_ESCOLARIDADE|TP_ESCOLARIDADE')
      ) %>% 
      # transformar de chr em num
      mutate(across(matches('escolaridade'), as.numeric)) %>% 
      # criar var de prop docentes superior por escola
      group_by(cod_escola) %>% # Passo seguinte lida com a mudanca na variavel superior. (ver anexo dos dados)
      summarise(!!paste0('prop_docentes_superior__', year) := case_when(year == '2015' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2017' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2019' ~ length(cod_docente[escolaridade==4]),
                                                                        TRUE ~ length(cod_docente[escolaridade>=6])
                                                                        )/length(cod_docente)
      )
  } %>%
    set_names(c('2007','2009','2011','2013','2015','2017', '2019')) %>%
    write_rds(file = here('data/processed/docentes_reg_co.rds'))
  
  # Sul ========================================================================
  foreach(norte = here('data/raw') %>%
            list.files(pattern = "DOCENTES_SUL_20[0-1][0-9][.]CSV$"),
          year = c('2007','2009','2011','2013','2015','2017', '2019'),
          .packages=c('readr','dplyr','here')
  ) %do% {
    print(year)
    norte %>%
      as.character() %>%
      paste0('data/raw/',.) %>%
      read_delim("|",
                 # n_max = 1000,
                 escape_double = FALSE,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = "UTF-8"),
                 trim_ws = TRUE) %>%
      select(cod_escola = matches('PK_COD_ENTIDADE|CO_ENTIDADE'),
             cod_docente = matches('FK_COD_DOCENTE|CO_PESSOA_FISICA|ID_DOCENTE'),
             escolaridade = matches('FK_COD_ESCOLARIDADE|TP_ESCOLARIDADE')
      ) %>% 
      # transformar de chr em num
      mutate(across(matches('escolaridade'), as.numeric)) %>% 
      # criar var de prop docentes superior por escola
      group_by(cod_escola) %>% # Passo seguinte lida com a mudanca na variavel superior. (ver anexo dos dados)
      summarise(!!paste0('prop_docentes_superior__', year) := case_when(year == '2015' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2017' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2019' ~ length(cod_docente[escolaridade==4]),
                                                                        TRUE ~ length(cod_docente[escolaridade>=6])
                                                                        )/length(cod_docente)
      )
  } %>%
    set_names(c('2007','2009','2011','2013','2015','2017', '2019')) %>%
    write_rds(file = here('data/processed/docentes_reg_sl.rds'))
  
  # Sudeste ====================================================================
  foreach(norte = here('data/raw') %>%
            list.files(pattern = "DOCENTES_SUDESTE_20[0-1][0-9][.]CSV$"),
          year = c('2007','2009','2011','2013','2015','2017', '2019'),
          .packages=c('readr','dplyr','here')
  ) %do% {
    print(year)
    norte %>%
      as.character() %>%
      paste0('data/raw/',.) %>%
      read_delim("|",
                 # n_max = 1000,
                 escape_double = FALSE,
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = "UTF-8"),
                 trim_ws = TRUE) %>%
      select(cod_escola = matches('PK_COD_ENTIDADE|CO_ENTIDADE'),
             cod_docente = matches('FK_COD_DOCENTE|CO_PESSOA_FISICA|ID_DOCENTE'),
             escolaridade = matches('FK_COD_ESCOLARIDADE|TP_ESCOLARIDADE')
      ) %>% 
      # transformar de chr em num
      mutate(across(matches('escolaridade'), as.numeric)) %>% 
      # criar var de prop docentes superior por escola
      group_by(cod_escola) %>% # Passo seguinte lida com a mudanca na variavel superior. (ver anexo dos dados)
      summarise(!!paste0('prop_docentes_superior__', year) := case_when(year == '2015' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2017' ~ length(cod_docente[escolaridade==4]),
                                                                        year == '2019' ~ length(cod_docente[escolaridade==4]),
                                                                        TRUE ~ length(cod_docente[escolaridade>=6])
                                                                        )/length(cod_docente)
      )
  } %>%
    set_names(c('2007','2009','2011','2013','2015','2017', '2019')) %>%
    write_rds(file = here('data/processed/docentes_reg_sd.rds'))
  
  # Brasil =====================================================================
  foreach(regioes = here('data/processed') %>%
          list.files(pattern = "docentes_reg_[a-z][a-z][.]rds$"),
          year = c('2007','2009','2011','2013','2015','2017', '2019'),
         .verbose = T,
         # juntar tudo por linhas
         .combine = rbind,
         .packages = c('tidyverse')
  ) %dopar% {
     regioes %>%
       as.character() %>%
       paste0('data/processed/',.) %>%
       read_rds() %>% 
       reduce(inner_join, by = 'cod_escola')
  } %>% 
    saveRDS(file = here('data/processed/docentes_br.rds'))
}

# PIB ##########################################################################
# PIB per capita municipal por ano
get_sidra(5938,
          variable = 37,
          period = c('2007',
                     '2009',
                     '2011',
                     '2013',
                     '2015',
                     '2017'),
          geo = 'City') %>% 
  select(cod_mun = `Município (Código)`,
                   year = Ano,
                   pib = Valor) %>% 
  write_rds(here('data/processed/pib_per_capita_municipal.rds'))