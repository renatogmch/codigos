# Script Dissertacao ----
# Data: 01/11/2019
# Ultima Modificacao: 18/06/2020
# Autor: Renato Gomes Chaves
# Versao: 2.0

{
  require(pacman)
  p_load(char = c("tidyverse",
                  "here",
                  "foreach",
                  "doParallel",
                  'geosphere',
                  'REAT',
                  'plm'
  )
  )
  gc()
  no_cores <- detectCores() - 1
  registerDoParallel(cores=no_cores)
  # stopImplicitCluster()
  # aumentar max mem ram com pagefile
  utils::memory.limit(32000)
  print(paste0('Memoria RAM maxima: ', memory.limit()))
  print(paste0('Cores utilizados: ', no_cores))
  rm(no_cores)
}

# funcao pra colocar zeros a esquerda str_pad()


# teste do future
p_load(future)
p_load(doFuture)
plan('multisession')
registerDoFuture()


firmas %<-% foreach(files = here('data/processed/') %>%
                    list.files(pattern = '^firmas_'), #^ significa 'comeca com...'
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
           !!paste0('prop_superior__',year) := prop_superior,
           !!paste0('tam_estab__',year) := tam_estab)
} %>%
  set_names(c('2006',
              '2008',
              '2010',
              '2012',
              '2014',
              '2016'))




# Carregar Dados ----
firmas <- foreach(files = here('data/processed/') %>%
                    list.files(pattern = '^firmas_'), #^ significa 'comeca com...'
                  year = c('2006','2008','2010','2012','2014','2016'),
                  .packages = c('tidyverse','here'),
                  .verbose = T
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
                               !!paste0('prop_superior__',year) := prop_superior,
                               !!paste0('tam_estab__',year) := tam_estab)
                    } %>%
                      set_names(c('2006',
                                  '2008',
                                  '2010',
                                  '2012',
                                  '2014',
                                  '2016'))

# Variavel FGROWTH ----
# crescimento do emprego entre os anos
# crescimento_ano_base = log(ano_seguinte) - log(ano_base)
# ln(vinc)t+1 - ln(vinc)t
fgrowth <- firmas %>%
  reduce(inner_join, by = c('cnpj', 'cnae')) %>%
  select(cnpj,
         contains('vinculos')) %>%
  transmute(cnpj,
            fgrowth__2006 = log(vinculos__2008)-log(vinculos__2006),
            fgrowth__2008 = log(vinculos__2010)-log(vinculos__2008),
            fgrowth__2010 = log(vinculos__2012)-log(vinculos__2010),
            fgrowth__2012 = log(vinculos__2014)-log(vinculos__2012),
            fgrowth__2014 = log(vinculos__2016)-log(vinculos__2014))


# Variavel MKTPOT ----
# LatLon dos municipios
# abri o KML no excel e exportei a coluna 'laton' pra xlsx pra nao ter que ler KML no R
{
  mun_latlon <- readxl::read_excel(here('data/raw/extra/mun_georef_excel.xlsx')) %>%
    transmute(cod_mun = as.character(`ns1:value`),
              nome_mun = `ns1:address`,
              `ns1:coordinates`) %>%
    separate('ns1:coordinates',
             into = c('lon','lat'),
             sep = ',',
             convert = T)
  
  # PIB municipal
  pib_municipal <- here('/data/raw/extra/pib_per_capita_anual_mun_2017.csv') %>%
    readr::read_csv(skip = 3,
                    n_max = 5570,
                    col_types = cols(
                      `Cód.` = 'c',
                      Município = 'c',
                      `2002` = 'd',
                      `2003` = 'd',
                      `2004` = 'd',
                      `2005` = 'd',
                      `2006` = 'd',
                      `2007` = 'd',
                      `2008` = 'd',
                      `2009` = 'd',
                      `2010` = 'd',
                      `2011` = 'd',
                      `2012` = 'd',
                      `2013` = 'd',
                      `2014` = 'd',
                      `2015` = 'd',
                      `2016` = 'd',
                      `2017` = 'd'
                    )) %>%
    as_tibble() %>%
    # tirar os anos nao utilizados
    transmute(cod_mun = str_remove(.$`Cód.`, "[0-9]$"),
              `2006`,
              `2008`,
              `2010`,
              `2012`,
              `2014`,
              `2016`) %>%
    replace(is.na(.), 0)
  
  # Area dos municipios para a diagonal principal
  area_municipios <- here('/data/raw/extra/AR_BR_RG_UF_MES_MIC_MUN_2018.xls') %>%
    readxl::read_excel() %>%
    select(cod_mun = CD_GCMUN, 
           nome_mun = NM_MUN_2018,
           area_mun = AR_MUN_2018) %>%
    semi_join(., mun_latlon, by = 'cod_mun')
  
  # nome e codigo dos municipios
  regioes_geograficas <- here('/data/raw/extra/regioes_geograficas_composicao_por_municipios_2017.xlsx') %>%
    readxl::read_excel() %>%
    transmute(
      across(.cols = c('cod_mun_dv',
                       'cod_mun',
                       'nome_mun'),
             as.character)
    )
  
  # Matriz de distancias
  dist_mat <- mun_latlon %>%
    # escolher lon lat, nessa ordem
    select(lon, lat) %>%
    # distancia em metros
    distm(fun = distGeo)
  # diagonal da matriz de distancias de acordo com literatura MATA(2005)
  diag(dist_mat) <- ((2/3)*sqrt(area_municipios$area_mun/pi))
  dist_mat <- dist_mat^0.3
  
  # variavel mktpot por ano e municipio
  # foreach nao deu certo. usando do jeito antigo mesmo
  # um bloco de cbind pra cada ano
  mktpot <- bind_cols(
    pib_municipal %>%
      select(cod_mun),
    pib_municipal %>%
      select(`2006`) %>%
      "/"(dist_mat) %>%
      rowSums() %>% 
      as_tibble(),
    pib_municipal %>%
      select(`2008`) %>%
      "/"(dist_mat) %>%
      rowSums() %>% 
      as_tibble(),
    pib_municipal %>%
      select(`2010`) %>%
      "/"(dist_mat) %>%
      rowSums() %>% 
      as_tibble(),
    pib_municipal %>%
      select(`2012`) %>%
      "/"(dist_mat) %>%
      rowSums() %>% 
      as_tibble(),
    pib_municipal %>%
      select(`2014`) %>%
      "/"(dist_mat) %>%
      rowSums() %>% 
      as_tibble(),
    pib_municipal %>%
      select(`2016`) %>%
      "/"(dist_mat) %>%
      rowSums() %>% 
      as_tibble()
  ) %>%
    set_names(nm = c('cod_mun',
                     'mktpot__2006',
                     'mktpot__2008',
                     'mktpot__2010',
                     'mktpot__2012',
                     'mktpot__2014',
                     'mktpot__2016')
    )
  
  # %>%
  #   inner_join(regioes_geograficas,
  #              .,
  #              by = c('cod_mun_dv' = 'cod_mun')
  #   )
  
  rm(area_municipios,
     dist_mat,
     mun_latlon,
     pib_municipal,
     regioes_geograficas)
}


# Variavel LOCQ ----

# E= Emprego
# F= Firmas
# i= Industria
# j= Municipio
# t= Ano

# Metodologia (por MUN):
# Locq de emprego
# (e_ijt)Qt de Emprego/Firmas da industria i, no municipio j, no ano t;
# (e_jt)Qt de Emprego/Firmas total no municipio j no ano t;
# (e_it)Qt de Emprego/Firmas total na industria i no ano t;
# (e_t)Qt de Emprego/Firmas total no ano t;

# locationq <- foreach(year = firmas,
#                      .packages = 'tidyverse') %dopar% {
#                        year %>%
#                          filter(cnae >=10000 & cnae <11000) %>%
#                          group_by(cod_mun) %>%
#                          summarise(!!paste0('vinc_',year ,'_cnae_mun') := sum(!!paste0('vinculos__', year))) %>%
#                          full_join(year %>%
#                                      group_by(cod_mun) %>%
#                                      summarise(!!paste0('vinc_', year, '_mun') := sum(!!paste0('vinculos__', year))),
#                                    by = 'cod_mun'
#                          )
#                      }
# # parei aqui
# precisa reescrever o locq abaixo e depois paralelizar
# nao ta dando certo, fiz como abaixo
{
  locationq <- list()
  
  locationq[['locq2006']] <- firmas[['2006']] %>%
    filter(cnae >=10000 & cnae <11000) %>%
    group_by(cod_mun) %>%
    summarise(vinc_2006_cnae_mun = sum(vinculos__2006)) %>%
    full_join(firmas[['2006']] %>%
                group_by(cod_mun) %>%
                summarise(vinc_2006_mun = sum(vinculos__2006)),
              by = 'cod_mun'
    ) %>%
    mutate(firmas[['2006']] %>%
             filter(cnae >=10000 & cnae <11000) %>%
             summarise(vinc_2006_cnae = sum(vinculos__2006)),
           firmas[['2006']] %>%
             summarise(vinc_2006 = sum(vinculos__2006))
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2006 = locq(vinc_2006_cnae_mun,
                           vinc_2006_mun,
                           vinc_2006_cnae,
                           vinc_2006)
    ) %>%
    ungroup() %>%
    select(cod_mun,
           locq__2006)
  
  locationq[['locq2008']] <- firmas[['2008']] %>%
    filter(cnae >=10000 & cnae <11000) %>%
    group_by(cod_mun) %>%
    summarise(vinc_2008_cnae_mun = sum(vinculos__2008)) %>%
    full_join(firmas[['2008']] %>%
                group_by(cod_mun) %>%
                summarise(vinc_2008_mun = sum(vinculos__2008)),
              by = 'cod_mun'
    ) %>%
    mutate(firmas[['2008']] %>%
             filter(cnae >=10000 & cnae <11000) %>%
             summarise(vinc_2008_cnae = sum(vinculos__2008)),
           firmas[['2008']] %>%
             summarise(vinc_2008 = sum(vinculos__2008))
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2008 = locq(vinc_2008_cnae_mun,
                           vinc_2008_mun,
                           vinc_2008_cnae,
                           vinc_2008)
    ) %>%
    ungroup() %>%
    select(cod_mun,
           locq__2008)
  
  locationq[['locq2010']] <- firmas[['2010']] %>%
    filter(cnae >=10000 & cnae <11000) %>%
    group_by(cod_mun) %>%
    summarise(vinc_2010_cnae_mun = sum(vinculos__2010)) %>%
    full_join(firmas[['2010']] %>%
                group_by(cod_mun) %>%
                summarise(vinc_2010_mun = sum(vinculos__2010)),
              by = 'cod_mun'
    ) %>%
    mutate(firmas[['2010']] %>%
             filter(cnae >=10000 & cnae <11000) %>%
             summarise(vinc_2010_cnae = sum(vinculos__2010)),
           firmas[['2010']] %>%
             summarise(vinc_2010 = sum(vinculos__2010))
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2010 = locq(vinc_2010_cnae_mun,
                           vinc_2010_mun,
                           vinc_2010_cnae,
                           vinc_2010)
    ) %>%
    ungroup() %>%
    select(cod_mun,
           locq__2010)
  
  locationq[['locq2012']] <- firmas[['2012']] %>%
    filter(cnae >=10000 & cnae <11000) %>%
    group_by(cod_mun) %>%
    summarise(vinc_2012_cnae_mun = sum(vinculos__2012)) %>%
    full_join(firmas[['2012']] %>%
                group_by(cod_mun) %>%
                summarise(vinc_2012_mun = sum(vinculos__2012)),
              by = 'cod_mun'
    ) %>%
    mutate(firmas[['2012']] %>%
             filter(cnae >=10000 & cnae <11000) %>%
             summarise(vinc_2012_cnae = sum(vinculos__2012)),
           firmas[['2012']] %>%
             summarise(vinc_2012 = sum(vinculos__2012))
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2012 = locq(vinc_2012_cnae_mun,
                           vinc_2012_mun,
                           vinc_2012_cnae,
                           vinc_2012)
    ) %>%
    ungroup() %>%
    select(cod_mun,
           locq__2012)
  
  locationq[['locq2014']] <- firmas[['2014']] %>%
    filter(cnae >=10000 & cnae <11000) %>%
    group_by(cod_mun) %>%
    summarise(vinc_2014_cnae_mun = sum(vinculos__2014)) %>%
    full_join(firmas[['2014']] %>%
                group_by(cod_mun) %>%
                summarise(vinc_2014_mun = sum(vinculos__2014)),
              by = 'cod_mun'
    ) %>%
    mutate(firmas[['2014']] %>%
             filter(cnae >=10000 & cnae <11000) %>%
             summarise(vinc_2014_cnae = sum(vinculos__2014)),
           firmas[['2014']] %>%
             summarise(vinc_2014 = sum(vinculos__2014))
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2014 = locq(vinc_2014_cnae_mun,
                           vinc_2014_mun,
                           vinc_2014_cnae,
                           vinc_2014)
    ) %>%
    ungroup() %>%
    select(cod_mun,
           locq__2014)
  
  locationq[['locq2016']] <- firmas[['2016']] %>%
    filter(cnae >=10000 & cnae <11000) %>%
    group_by(cod_mun) %>%
    summarise(vinc_2016_cnae_mun = sum(vinculos__2016)) %>%
    full_join(firmas[['2016']] %>%
                group_by(cod_mun) %>%
                summarise(vinc_2016_mun = sum(vinculos__2016)),
              by = 'cod_mun'
    ) %>%
    mutate(firmas[['2016']] %>%
             filter(cnae >=10000 & cnae <11000) %>%
             summarise(vinc_2016_cnae = sum(vinculos__2016)),
           firmas[['2016']] %>%
             summarise(vinc_2016 = sum(vinculos__2016))
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2016 = locq(vinc_2016_cnae_mun,
                           vinc_2016_mun,
                           vinc_2016_cnae,
                           vinc_2016)
    ) %>%
    ungroup() %>%
    select(cod_mun,
           locq__2016)
  
  locationq <- locationq %>%
    reduce(left_join, by = 'cod_mun')
}

# DataBase Longo completo----
firmas_long <- firmas %>%
  # usei o inner_join pra cnae e cnpj, pra ter soh as sobreviventes dos anos. resultado 200k+ firmas
  # reduce pra juntar todos os dfs. Inner pra sobrar so o que tiver nos 2 com mesmo cnpj e cnae
  reduce(inner_join, by = c('cnpj', 'cnae')) %>%
  # adiciona o fgrowth
  left_join(fgrowth, by = 'cnpj') %>%
  # selecionar cada variavel de cada df
  select(cnpj,
         cod_mun = cod_mun.x,
         cnae,
         starts_with('vinculos'),
         starts_with('rem_med'),
         starts_with('prop_superior'),
         starts_with('tam_estab'),
         starts_with('fgrowth')) %>%
    # consertar algumas colunas que ficaram como character
  mutate(across(starts_with('tam_estab'), as.double))  %>%
  left_join(locationq, by = 'cod_mun') %>%
  left_join(mktpot, by = 'cod_mun') %>%
  select(-contains('__2016')) %>%
  pivot_longer(-c(cnpj,
                  cod_mun,
                  cnae),
               names_sep = '__',
               names_to = c('.value', 'year')) %>%
  # 2 PJs  estava am municipios que n tem dados, resultando NA
  # solucao mais rapida foi excluir
  drop_na()

# Testes PLM ----

plm_simples <- firmas_long %>%
  plm(fgrowth ~ log(vinculos),
      model = 'within',
      index = c('cnpj', 'year'),
      data = .)
  

plm_completo <- firmas_long %>%
  plm(fgrowth ~ log(vinculos)
      + log(rem_med + 0.000000000000000000000000000000000000000001)
      + log(prop_superior + 0.000000000000000000000000000000000000000001)
      + log(locq + 0.000000000000000000000000000000000000000001)
      + log(mktpot + 0.000000000000000000000000000000000000000001)
      + as.factor(year),
      model = 'within',
      index = c('cnpj', 'year'),
      data = .)
  