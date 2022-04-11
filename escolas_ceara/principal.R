# Script Artigo Convergencia IDEB ----
# Data: 01/10/19
# Autor: Renato Gomes Chaves
# Versão 2.7

{
  # options(repos='http://cran.rstudio.com/')
  require(pacman)
  p_load(char = c('here',
                   'tidyverse',
                   # 'pipeR',
                   # 'lmtest',
                   # 'readr',
                   # 'data.table',
                   # 'reshape2',
                   'plm',
                   # 'normtest',
                   # 'lmtest',
                   'stargazer',
                   'xtable',
                   'doParallel',
                   'foreach',
                  'tictoc'
                  )
         )
  # no_cores <- detectCores()*0.75
  # registerDoParallel(cores = no_cores)
}


# IDEB #########################################################################
# remover 2005 pq nao tem como cruzar os dados do censo escolar
# o tipo de identificador por escola mudou de 2005 pra frente
{
  tic()
  ideb <- read_rds(here('data/processed/ideb.rds'))
  # uma funcao para cada db, anos iniciais e anos finais
ideb[['iniciais_VAR_LONG']] <- ideb[['iniciais']] %>%
                                  select('cod_escola',
                                         'uf',
                                         contains(c('ideb', 'growth'))) %>%
                                  pivot_longer(-c('uf',
                                                  'cod_escola'),
                                               names_to = c('.value', 'year'),
                                               names_sep = '__'
                                  ) %>% 
    filter(year != '2019') %>%
    filter(year != '2005') # tirar 2005 pq so usei 2007->2017
  
  ideb[['finais_VAR_LONG']] <- ideb[['finais']] %>%
                                select('cod_escola',
                                       'uf',
                                       contains(c('ideb', 'growth'))) %>%
                                pivot_longer(-c('uf',
                                                'cod_escola'),
                                             names_to = c('.value', 'year'),
                                             names_sep = '__'
                                ) %>%
    filter(year != '2019') %>% 
    filter(year != '2005')
  toc()
}

# TURMAS #######################################################################
{
  tic()
  # usar o summarise pra ter a media de alunos por TURMA
  turmas <- read_rds(here('data/processed/turmas.rds'))
  # metodo: contar numero de turmas na escola e dividir pela
  # soma dos alunos de cada turma, por escola por ano.
  # numero de turmas != do numero de salas de aula
  # nome da var: media_alunos_turma
  foreach(turmas = turmas,
          year = c('2007','2009','2011','2013','2015','2017', '2019'),
          .packages = 'tidyverse') %dopar% {
            turmas %>%
              group_by(cod_escola) %>% # Agrupar por cada escola
              # criar a media de alunos igual a soma das matriculas dividido pelo numero de turmas
              # criar variavel de numero total de alunos
              summarise(!!paste0('media_alunos_turma__', year) := mean(get(!!paste0('nu_matriculas__', year))),
                        !!paste0('nu_alunos__', year) := sum(get(!!paste0('nu_matriculas__', year)))
              )
          } %>%
    reduce(inner_join, by = 'cod_escola') %>% # Juntar tudo
    {turmas[['VAR']] <<- .} %>% # Salvar o join para estat descritiva
    pivot_longer(-cod_escola,
                 names_sep = '__',
                 names_to = c('.value', 'year')
                 ) %>% # Transformar em formato long
    filter(year != '2019') %>%  # Tirar 2019
    {turmas[['VAR_LONG']] <<- .} # Salvar a variavel long para o plm
  toc()
}

# ESCOLAS ######################################################################
# Carregar dados e manipula-los
# inverter os 0's e 1's
# criar as vars para o plm
# requer dados das TURMAS para numero de alunos total necessario
# para calculo de #PCs/#Alunos. Criacao dessa var foi feita
# na secao ANALISE
{
  tic()
  # Carregar dados dos censos escolares
  escolas <- here('data/processed/escolas.rds') %>%
    read_rds()
  # Criar variavel wide
  escolas %>%
    reduce(inner_join, by = c('cod_escola', 'cod_mun')) %>%
    # {escolas[['VAR']] <<- .} %>%
    pivot_longer(-c('cod_escola',
                    'cod_mun'), # Variavel usada como ID
                 names_to = c('.value', 'year'), # .value primeiro pra os anos serem as linhas
                 names_sep = '__' # separador dos anos e nomes das vars
    ) %>%
    # filtrar 2019
    filter(year != 2019) %>%
    # salvar variavel em long
    {escolas[['VAR_LONG']] <<- .}
  toc()
}  


# DOCENTES #####################################################################
{
  docentes <- list(
    VAR = here('data/processed/docentes_br.rds') %>% 
    read_rds(),
    VAR_LONG = here('data/processed/docentes_br.rds') %>% 
      read_rds() %>% 
      pivot_longer(-cod_escola,
                   names_prefix = 'prop_docentes_superior__',
                   names_to = 'year',
                   values_to = 'prop_docentes_superior') %>% # Transformar em formato long
      filter(year != 2019)
  )
}

# PIB MUNICIPAL ################################################################
pib_municipal <- here('data/processed/pib_per_capita_municipal.rds') %>% 
  read_rds()

# ANALISE BRASIL ###############################################################
# CONV ABSOLUTA ANOS INICIAIS
# Teste para ef fixos, variaveis e pooling
iniciais_brasil_absoluta <- foreach(model = c('within', 'between', 'pooling'),
        .packages = c('plm', 'tidyverse')
) %do% {
  print(model)
list(ideb[['iniciais_VAR_LONG']],
       escolas[['VAR_LONG']],
       turmas[['VAR_LONG']],
       docentes[['VAR_LONG']]) %>% 
    reduce(inner_join, by = c('cod_escola', 'year')) %>% 
  # filter(uf == 'CE') %>%
    plm(log(growth) ~ log(ideb)
        + factor(year),
        data = .,
        index = c('cod_escola', 'year'),
        model = model) %>%
    summary()
} %>% 
  set_names(c('Conv Absoluta - anos iniciais - Ef Fixos',
              'Conv Absoluta - anos iniciais - Ef Variavel',
              'Conv Absoluta - anos iniciais - Pooling'
              )
            )

# CONV CONDICIONAL ANOS INICIAIS
# Teste para ef fixos, variaveis e pooling
iniciais_brasil_condicional <- foreach(model = c('within', 'between', 'pooling'),
        .packages = c('plm', 'tidyverse')
) %do% {
  print(model)
  list(ideb[['iniciais_VAR_LONG']],
       escolas[['VAR_LONG']],
       turmas[['VAR_LONG']],
       docentes[['VAR_LONG']]) %>% 
    reduce(inner_join, by = c('cod_escola', 'year')) %>%
    left_join(.,
              pib_municipal,
              by = c('cod_mun',
                     'year')
    ) %>%
    # filter(uf == 'CE') %>%
    plm(log(growth) ~ log(ideb)
        + factor(year)
        # vars dummy
        + factor(id_agua)
        + factor(id_energia)
        + factor(id_esgoto)
        + factor(id_internet)
        + factor(id_biblioteca)
        + factor(id_cozinha)
        + factor(id_alimentacao)
        + factor(id_lab_ciencias)
        + factor(id_lab_inform)
        # vars numericas em log
        # + log(nu_comp_alunos/nu_alunos) # comp_alu ta com problema de log(0)
        + log(nu_alunos)
        + log(nu_funcionarios)
        + log(nu_salas)
        + log(media_alunos_turma)
        + log(pib)
        # + log(prop_docentes_superior) # ta com problema de log(0)
        # vars de categoria ou factor
        + factor(fc_localizacao)
        + factor(fc_dependencia_adm),
        data = .,
        index = c('cod_escola', 'year'),
        model = model) %>%
    summary()
} %>% 
  set_names(c('Conv Condicional - anos iniciais - Ef Fixos',
              'Conv Condicional - anos iniciais - Ef Variavel',
              'Conv Condicional - anos iniciais - Pooling'
              )
  )

# CONV ABSOLUTA ANOS FINAIS
finais_brasil_absoluta <- foreach(model = c('within', 'between', 'pooling'),
        .packages = c('plm', 'tidyverse')
) %do% {
  print(model)
  list(ideb[['finais_VAR_LONG']],
       escolas[['VAR_LONG']],
       turmas[['VAR_LONG']],
       docentes[['VAR_LONG']]) %>% 
    reduce(inner_join, by = c('cod_escola', 'year')) %>%
    # filter(uf == 'CE') %>%
    plm(log(growth) ~ log(ideb)
        + factor(year),
        data = .,
        index = c('cod_escola', 'year'),
        model = model) %>%
    summary()
} %>% 
  set_names(c('Conv Absoluta - anos finais - Ef Fixos',
              'Conv Absoluta - anos finais - Ef Variavel',
              'Conv Absoluta - anos finais - Pooling'
              )
  )

# CONV CONDICIONAL ANOS FINAIS
finais_brasil_condicional <- foreach(model = c('within', 'between', 'pooling'),
        .packages = c('plm', 'tidyverse')
) %do% {
  print(model)
  list(ideb[['finais_VAR_LONG']],
       escolas[['VAR_LONG']],
       turmas[['VAR_LONG']],
       docentes[['VAR_LONG']]) %>% 
    reduce(inner_join, by = c('cod_escola', 'year')) %>%
    left_join(.,
              pib_municipal,
              by = c('cod_mun',
                     'year')
    ) %>%
    # filter(uf == 'CE') %>%
    plm(log(growth) ~ log(ideb)
        + factor(year)
        # vars dummy
        + factor(id_agua)
        + factor(id_energia)
        + factor(id_esgoto)
        + factor(id_internet)
        + factor(id_biblioteca)
        + factor(id_cozinha)
        + factor(id_alimentacao)
        + factor(id_lab_ciencias)
        + factor(id_lab_inform)
        # vars numericas em log
        # + log(nu_comp_alunos/nu_alunos) # comp_alu ta com problema de log(0)
        + log(nu_alunos)
        + log(nu_funcionarios)
        + log(nu_salas)
        + log(media_alunos_turma)
        + log(pib)
        # + log(prop_docentes_superior) # ta com problema de log(0)
        # vars de categoria ou factor
        + factor(fc_localizacao)
        + factor(fc_dependencia_adm),
        data = .,
        index = c('cod_escola', 'year'),
        model = model) %>%
    summary()
} %>% 
  set_names(c('Conv Condicional - anos finais - Ef Fixos',
              'Conv Condicional - anos finais - Ef Variavel',
              'Conv Condicional - anos finais - Pooling'
              )
  )

# ANALISE CEARA ################################################################
# CONV ABSOLUTA ANOS INICIAIS
# Teste para ef fixos, variaveis e pooling
iniciais_ceara_absoluta <- foreach(model = c('within', 'between', 'pooling'),
                                    .packages = c('plm', 'tidyverse')
) %do% {
  print(model)
  list(ideb[['iniciais_VAR_LONG']],
       escolas[['VAR_LONG']],
       turmas[['VAR_LONG']],
       docentes[['VAR_LONG']]) %>% 
    reduce(inner_join, by = c('cod_escola', 'year')) %>% 
    filter(uf == 'CE') %>%
    plm(log(growth) ~ log(ideb)
        + factor(year),
        data = .,
        index = c('cod_escola', 'year'),
        model = model) %>%
    summary()
} %>% 
  set_names(c('Conv Absoluta - anos iniciais - Ef Fixos',
              'Conv Absoluta - anos iniciais - Ef Variavel',
              'Conv Absoluta - anos iniciais - Pooling'
  )
  )

# CONV CONDICIONAL ANOS INICIAIS
# Teste para ef fixos, variaveis e pooling
iniciais_ceara_condicional <- foreach(model = c('within', 'between', 'pooling'),
                                       .packages = c('plm', 'tidyverse')
) %do% {
  # print(model)
  list(ideb[['iniciais_VAR_LONG']],
       escolas[['VAR_LONG']],
       turmas[['VAR_LONG']],
       docentes[['VAR_LONG']]) %>% 
    reduce(inner_join, by = c('cod_escola', 'year')) %>%
    left_join(.,
              pib_municipal,
              by = c('cod_mun',
                     'year')
    ) %>%
    filter(uf == 'CE') %>%
    plm(log(growth) ~ log(ideb)
        + factor(year)
        # vars dummy
        + factor(id_agua)
        + factor(id_energia)
        + factor(id_esgoto)
        + factor(id_internet)
        + factor(id_biblioteca)
        + factor(id_cozinha)
        + factor(id_alimentacao)
        + factor(id_lab_ciencias)
        + factor(id_lab_inform)
        # vars numericas em log
        # + log(nu_comp_alunos/nu_alunos) # comp_alu ta com problema de log(0)
        + log(nu_alunos)
        + log(nu_funcionarios)
        + log(nu_salas)
        + log(media_alunos_turma)
        + log(pib)
        # + log(prop_docentes_superior) # ta com problema de log(0)
        # vars de categoria ou factor
        + factor(fc_localizacao)
        + factor(fc_dependencia_adm),
        data = .,
        index = c('cod_escola', 'year'),
        model = model) %>%
    summary()
} %>% 
  set_names(c('Conv Condicional - anos iniciais - Ef Fixos',
              'Conv Condicional - anos iniciais - Ef Variavel',
              'Conv Condicional - anos iniciais - Pooling'
  )
  )

# CONV ABSOLUTA ANOS FINAIS
finais_ceara_absoluta <- foreach(model = c('within', 'between', 'pooling'),
                                  .packages = c('plm', 'tidyverse')
) %do% {
  print(model)
  list(ideb[['finais_VAR_LONG']],
       escolas[['VAR_LONG']],
       turmas[['VAR_LONG']],
       docentes[['VAR_LONG']]) %>% 
    reduce(inner_join, by = c('cod_escola', 'year')) %>%
    filter(uf == 'CE') %>%
    plm(log(growth) ~ log(ideb)
        + factor(year),
        data = .,
        index = c('cod_escola', 'year'),
        model = model) %>%
    summary()
} %>% 
  set_names(c('Conv Absoluta - anos finais - Ef Fixos',
              'Conv Absoluta - anos finais - Ef Variavel',
              'Conv Absoluta - anos finais - Pooling'
  )
  )

# CONV CONDICIONAL ANOS FINAIS
finais_ceara_condicional <- foreach(model = c('within', 'between', 'pooling'),
                                     .packages = c('plm', 'tidyverse')
) %do% {
  print(model)
  list(ideb[['finais_VAR_LONG']],
       escolas[['VAR_LONG']],
       turmas[['VAR_LONG']],
       docentes[['VAR_LONG']]) %>% 
    reduce(inner_join, by = c('cod_escola', 'year')) %>%
    left_join(.,
              pib_municipal,
              by = c('cod_mun',
                     'year')
    ) %>%
    filter(uf == 'CE') %>%
    plm(log(growth) ~ log(ideb)
        + factor(year)
        # vars dummy
        + factor(id_agua)
        # + factor(id_energia)
        + factor(id_esgoto)
        + factor(id_internet)
        + factor(id_biblioteca)
        + factor(id_cozinha)
        + factor(id_alimentacao)
        + factor(id_lab_ciencias)
        + factor(id_lab_inform)
        # vars numericas em log
        # + log(nu_comp_alunos/nu_alunos) # comp_alu ta com problema de log(0)
        + log(nu_alunos)
        + log(nu_funcionarios)
        + log(nu_salas)
        + log(media_alunos_turma)
        + log(pib)
        # + log(prop_docentes_superior) # ta com problema de log(0)
        # vars de categoria ou factor
        + factor(fc_localizacao)
        + factor(fc_dependencia_adm),
        data = .,
        index = c('cod_escola', 'year'),
        model = model) %>%
    summary()
} %>% 
  set_names(c('Conv Condicional - anos finais - Ef Fixos',
              'Conv Condicional - anos finais - Ef Variavel',
              'Conv Condicional - anos finais - Pooling'
  )
  )

# GRAVAR RESULTADOS ############################################################
# BRASIL
iniciais_brasil_absoluta %>% 
  write_rds(here('data/processed/iniciais_brasil_absoluta.rds'))
iniciais_brasil_condicional %>%
  write_rds(here('data/processed/iniciais_brasil_condicional.rds'))
finais_brasil_absoluta %>%
  write_rds(here('data/processed/finais_brasil_absoluta.rds'))
finais_brasil_condicional %>%
  write_rds(here('data/processed/finais_brasil_condicional.rds'))

# CEARA
iniciais_ceara_absoluta %>% 
  write_rds(here('data/processed/iniciais_ceara_absoluta.rds'))
iniciais_ceara_condicional %>%
  write_rds(here('data/processed/iniciais_ceara_condicional.rds'))
finais_ceara_absoluta %>%
  write_rds(here('data/processed/finais_ceara_absoluta.rds'))
finais_ceara_condicional %>%
  write_rds(here('data/processed/finais_ceara_condicional.rds'))
