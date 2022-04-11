# Script Dissertacao ----
# Unificacao das bases e criacao das demais variaveis
# Data: 01/11/2019
# Autor: Renato Gomes Chaves
# Versao: 2.0

# cada secao depende de "Carregar dados"
# fora isso, cada seccao eh autonoma

{
  require(pacman)
  p_load(tidyverse,
         here,
         foreach,
         doParallel,
         geosphere,
         REAT,
         Rfast
         )
  no_cores <- detectCores()
  registerDoParallel(no_cores)
  # aumentar max mem ram com pagefile
  utils::memory.limit(32000)
  cat('Memoria RAM maxima:', memory.limit())
  # options(future.globals.maxSize = 2500 * 1024^2)
}

# Carregar Dados ---------------------------------------------------------------
# Arquivo firmas.rds criano no final do scrip 01_rais_trab
firmas <- read_rds(here('data/processed/firmas.rds'))


# Variavel FGROWTH ------------------------------------------------------------
# crescimento do emprego entre os anos
# aqui foi criado SEM O LOG, ou seja, (vinc)t/(vinc)t-1
# log sera adicionado na funcao do modelo no script 03_modelos
# procedimento sera feito da seguinte forma:
# log(crescimento) = log(ano_seguinte) - log(ano_base)
# ln(vinc)t+1 - ln(vinc)t

firmas %>%
  reduce(inner_join, by = c('cnpj', 'cnae')) %>%
  select(cnpj,
         contains('vinculos')) %>%
  transmute(cnpj,
            fgrowth__2006 = vinculos__2008/vinculos__2006,
            fgrowth__2008 = vinculos__2010/vinculos__2008,
            fgrowth__2010 = vinculos__2012/vinculos__2010,
            fgrowth__2012 = vinculos__2014/vinculos__2012,
            fgrowth__2014 = vinculos__2016/vinculos__2014
            ) %>%
  write_rds(., here('data/processed/fgrowth.rds'))



# Variavel MKTPOT -------------------------------------------------------------
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
              # abs tira o unico numero negativo
              `2012` = abs(`2012`),
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
  bind_cols(
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
    ) %>%
    write_rds(., here('data/processed/mktpot.rds'))
  
  rm(area_municipios,
     dist_mat,
     mun_latlon,
     pib_municipal,
     regioes_geograficas)
}


# Variavel LOCQ ---------------------------------------------------------------

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

{
  locationq <- list()
  
  locationq[['2006']] <- firmas[['2006']] %>%
    group_by(cod_mun) %>%
    summarise(vinc_2006_cnae_mun = sum(vinculos__2006[cnae >=10000 & cnae <11000]),
              vinc_2006_mun = sum(vinculos__2006)
    ) %>%
    mutate(vinc_2006_cnae = sum(vinc_2006_cnae_mun),
           vinc_2006 = sum(vinc_2006_mun)
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2006 = locq(vinc_2006_cnae_mun,
                                vinc_2006_mun,
                                vinc_2006_cnae,
                                vinc_2006)
    )  %>%
    select(cod_mun,
           locq__2006)

  locationq[['locq2008']] <- firmas[['2008']] %>%
    group_by(cod_mun) %>%
    summarise(vinc_2008_cnae_mun = sum(vinculos__2008[cnae >=10000 & cnae <11000]),
              vinc_2008_mun = sum(vinculos__2008)
    ) %>%
    mutate(vinc_2008_cnae = sum(vinc_2008_cnae_mun),
           vinc_2008 = sum(vinc_2008_mun)
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2008 = locq(vinc_2008_cnae_mun,
                             vinc_2008_mun,
                             vinc_2008_cnae,
                             vinc_2008)
    ) %>%
    select(cod_mun,
           locq__2008)
  
  locationq[['locq2010']] <- firmas[['2010']] %>%
    group_by(cod_mun) %>%
    summarise(vinc_2010_cnae_mun = sum(vinculos__2010[cnae >=10000 & cnae <11000]),
              vinc_2010_mun = sum(vinculos__2010)
    ) %>%
    mutate(vinc_2010_cnae = sum(vinc_2010_cnae_mun),
           vinc_2010 = sum(vinc_2010_mun)
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2010 = locq(vinc_2010_cnae_mun,
                             vinc_2010_mun,
                             vinc_2010_cnae,
                             vinc_2010)
    ) %>%
    select(cod_mun,
           locq__2010)
  
  locationq[['locq2012']] <- firmas[['2012']] %>%
    group_by(cod_mun) %>%
    summarise(vinc_2012_cnae_mun = sum(vinculos__2012[cnae >=10000 & cnae <11000]),
              vinc_2012_mun = sum(vinculos__2012)
    ) %>%
    mutate(vinc_2012_cnae = sum(vinc_2012_cnae_mun),
           vinc_2012 = sum(vinc_2012_mun)
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2012 = locq(vinc_2012_cnae_mun,
                             vinc_2012_mun,
                             vinc_2012_cnae,
                             vinc_2012)
    ) %>%
    select(cod_mun,
           locq__2012)
  
  locationq[['locq2014']] <- firmas[['2014']] %>%
    group_by(cod_mun) %>%
    summarise(vinc_2014_cnae_mun = sum(vinculos__2014[cnae >=10000 & cnae <11000]),
              vinc_2014_mun = sum(vinculos__2014)
    ) %>%
    mutate(vinc_2014_cnae = sum(vinc_2014_cnae_mun),
           vinc_2014 = sum(vinc_2014_mun)
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2014 = locq(vinc_2014_cnae_mun,
                             vinc_2014_mun,
                             vinc_2014_cnae,
                             vinc_2014)
    ) %>%
    select(cod_mun,
           locq__2014)
  
  locationq[['locq2016']] <- firmas[['2016']] %>%
    group_by(cod_mun) %>%
    summarise(vinc_2016_cnae_mun = sum(vinculos__2016[cnae >=10000 & cnae <11000]),
              vinc_2016_mun = sum(vinculos__2016)
    ) %>%
    mutate(vinc_2016_cnae = sum(vinc_2016_cnae_mun),
           vinc_2016 = sum(vinc_2016_mun)
    ) %>%
    replace(., is.na(.), 0) %>%
    rowwise() %>%
    mutate(locq__2016 = locq(vinc_2016_cnae_mun,
                             vinc_2016_mun,
                             vinc_2016_cnae,
                             vinc_2016)
    ) %>%
    select(cod_mun,
           locq__2016)
  
  locationq <- locationq %>%
    reduce(left_join, by = 'cod_mun')
} %>%
  write_rds(., here('data/processed/locationq.rds'))

# Variavel PORT_DIST ----------------------------------------------------------
{
  # carregar dados dos municipios
  mun_latlon <- readxl::read_excel(here('data/raw/extra/mun_georef_excel.xlsx')) %>%
    transmute(cod_mun = as.character(`ns1:value`),
              nome_mun = `ns1:address`,
              `ns1:coordinates`) %>%
    separate('ns1:coordinates',
             into = c('lon','lat'),
             sep = ',',
             convert = T)
  
  port_georef <- here('data/raw/extra/portos_georef.rds') %>%
    read_rds() %>%
    as_tibble()
  
  # criar tabela de distancia minima entre cidade e porto mais proximo
  mun_latlon %>%
    select(lon, lat) %>%
    distm(port_georef %>%
            select(lon, lat) %>%
            drop_na,
          fun = distGeo) %>%
    # value=T eh pra retornar o valor da celula
    # sem isso o resultado eh o numero da coluna onde tem o minimo
    rowMins(value = T) %>%
    as_tibble() %>%
    cbind(mun_latlon %>%
            select(cod_mun),
          .) %>%
    as_tibble() %>%
    transmute(cod_mun = str_remove(cod_mun, '[0-9]$'),
              port_dist = value) %>%
    write_rds(., here('data/processed/port_dist.rds')) 
  rm(mun_latlon,
     port_georef)
}
