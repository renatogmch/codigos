# Script Dissertacao ----
# modelos
# Data: 06/10/2020
# script precisa de MUITA RAM por causa do FUTURE
{
  require(pacman)
  p_load(tidyverse,
         here,
         tictoc,
         plm,
         future
  )
  # tamanho max dos dados pro future
  options(future.globals.maxSize=1e9)
}

# Carregar dos dados -----------------------------------------------------------
# Dados em formato Longo
firmas_long <- read_rds(here('data/processed/firmas.rds')) %>%
  # usei o inner_join pra cnae e cnpj, pra ter soh as sobreviventes dos anos.
  # resultado 200k+ firmas reduce pra juntar todos os dfs. Inner pra sobrar so
  # o que tiver nos 2 com mesmo cnpj e cnae.
  reduce(inner_join, by = c('cnpj', 'cnae', 'cod_mun')) %>%
  # adiciona o fgrowth
  left_join(read_rds(here('data/processed/fgrowth.rds')), by = 'cnpj') %>%
  # selecionar cada variavel de cada df
  select(cnpj,
         cod_mun,
         cnae,
         starts_with('vinculos'),
         starts_with('rem_med'),
         starts_with('prop_superior'),
         starts_with('tam_estab'),
         starts_with('fgrowth')) %>%
  # consertar algumas colunas que ficaram como character
  mutate(
    across(
      starts_with('tam_estab'),
      as.double)
    ) %>%
  left_join(read_rds(here('data/processed/locationq.rds')), by = 'cod_mun') %>%
  left_join(read_rds(here('data/processed/mktpot.rds')), by = 'cod_mun') %>%
  select(-contains('__2016')) %>%
  pivot_longer(-c(cnpj,
                  cod_mun,
                  cnae),
               names_sep = '__',
               names_to = c('.value', 'year')) %>%
  # 2 PJs  estava am municipios que n tem dados, resultando NA
  # solucao mais rapida foi excluir
  drop_na() 

# Dados para o OLS -----------------------------------------------------------
firmas_ols <- read_rds(here('data/processed/firmas.rds')) %>%
  # usei o inner_join pra cnae e cnpj, pra ter soh as sobreviventes dos anos.
  # resultado 200k+ firmas. reduce pra juntar todos os dfs. Inner pra sobrar
  # so o que tiver nos 2 com mesmo cnpj, cnae e cod do municipio
  reduce(inner_join, by = c('cnpj', 'cnae', 'cod_mun')) %>%
  # adicionar as outras variaveis
  left_join(read_rds(here('data/processed/locationq.rds')), by = 'cod_mun') %>%
  left_join(read_rds(here('data/processed/mktpot.rds')), by = 'cod_mun') %>%
  left_join(read_rds(here('data/processed/port_dist.rds')), by = 'cod_mun') %>%
  select(cnpj,
         cod_mun,
         cnae,
         port_dist,
         ends_with(c('2006',
                     '2010',
                     '2012',
                     '2016'))
  ) %>%
  mutate(fgrowth_2006_2010 = log(vinculos__2010) - log(vinculos__2006),
         fgrowth_2012_2016 = log(vinculos__2016) - log(vinculos__2012),
         fgrowth_2006_2016 = log(vinculos__2016) - log(vinculos__2006)
  ) %>%
  select(-contains('__2016'))


# Regressoes PLM Alimentos -----------------------------------------------------
modelos_plm_alimentos <- list()

# PLM Alimentos -  Efeito Fixo
{
  future:::ClusterRegistry("stop")
  plan(multisession, gc = TRUE)
  
  modelos_plm_alimentos[['fixo']] <- list()
  
  modelos_plm_alimentos[['fixo']][['simples']] <- future({
    firmas_long %>%
      filter(cnae >= 10000 & cnae < 11000) %>%
      plm(log(fgrowth) ~ log(vinculos),
          model = 'within',
          index = c('cnpj', 'year'),
          data = .)
  }, globals = c('modelos_plm_alimentos',
                 'firmas_long'),
  packages = c('tidyverse',
               'plm'))

  modelos_plm_alimentos[['fixo']][['simples_ano']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(year),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)
    }, globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['fixo']][['simples_tamanho']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(tam_estab),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)
    }, globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['fixo']][['completo']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)
    }, globals = c('modelos_plm_alimentos',
                                'firmas_long'),
  packages = c('tidyverse',
               'plm'))
  
  modelos_plm_alimentos[['fixo']][['completo_ano']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(year),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)
    }, globals = c('modelos_plm_alimentos',
                                'firmas_long'),
  packages = c('tidyverse',
               'plm'))
  
  modelos_plm_alimentos[['fixo']][['completo_tamanho']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(tam_estab),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)
    }, globals = c('modelos_plm_alimentos',
                                'firmas_long'),
  packages = c('tidyverse',
               'plm'))
  
  modelos_plm_alimentos[['fixo']] <- value(modelos_plm_alimentos[['fixo']])
  
  future:::ClusterRegistry("stop")
}

# PLM Alimentos - Efeito Variável
{
  future:::ClusterRegistry("stop")
  plan(multisession, gc = TRUE)
  
  modelos_plm_alimentos[['variavel']] <- list()
  
  modelos_plm_alimentos[['variavel']][['simples']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['variavel']][['simples_ano']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(year),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['variavel']][['simples_tamanho']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(tam_estab),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['variavel']][['completo']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['variavel']][['completo_ano']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(year),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['variavel']][['completo_tamanho']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(tam_estab),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['variavel']] <- value(modelos_plm_alimentos[['variavel']])
  
  future:::ClusterRegistry("stop")
}

# PLM Alimentos - Pooling
{
  future:::ClusterRegistry("stop")
  plan(multisession, gc = TRUE)
  
  modelos_plm_alimentos[['pooling']] <- list()
  
  modelos_plm_alimentos[['pooling']][['simples']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
        packages = c('tidyverse',
                     'plm'))
  
  modelos_plm_alimentos[['pooling']][['simples_ano']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(year),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
        packages = c('tidyverse',
                     'plm'))
  
  modelos_plm_alimentos[['pooling']][['simples_tamanho']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(tam_estab),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
        packages = c('tidyverse',
                     'plm'))
  
  modelos_plm_alimentos[['pooling']][['completo']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
        packages = c('tidyverse',
                     'plm'))
  
  modelos_plm_alimentos[['pooling']][['completo_ano']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(year),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['pooling']][['completo_tamanho']] <- future({firmas_long %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(tam_estab),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_alimentos',
                                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_alimentos[['pooling']] <- value(modelos_plm_alimentos[['pooling']])
  
  future:::ClusterRegistry("stop")
}
# modelos_plm_alimentos %>% write_rds(here('data/processed/models/modelos_plm_alimentos.rds'))

# Regressoes PLM Nacional-------------------------------------------------------
modelos_plm_nacional <- list()

# PLM Nacional - Efeito Fixo
{
  future:::ClusterRegistry("stop")
  plan(multisession, gc = TRUE)
  
  modelos_plm_nacional[['fixo']] <- list()
  
  modelos_plm_nacional[['fixo']][['simples']] <- future({
    firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('tidyverse',
                 'plm'))
  
  modelos_plm_nacional[['fixo']][['simples_ano']] <- future({
    firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(year),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['fixo']][['simples_tamanho']] <- future({
    firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(tam_estab),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['fixo']][['completo']] <- future({
    firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['fixo']][['completo_ano']] <- future({
    firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(year),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['fixo']][['completo_tamanho']] <- future({
    firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(tam_estab),
        model = 'within',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['fixo']] <- value(modelos_plm_nacional[['fixo']])
  future:::ClusterRegistry("stop")
}

# PLM Nacional -  Efeito Variável
{
  future:::ClusterRegistry("stop")
  plan(multisession)
  
  modelos_plm_nacional[['variavel']] <- list()
  
  modelos_plm_nacional[['variavel']][['simples']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['variavel']][['simples_ano']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(year),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['variavel']][['simples_tamanho']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(tam_estab),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  # pegar logo os resultados
  modelos_plm_nacional[['variavel']] <- value(modelos_plm_nacional[['variavel']])
  
  # refresh na RAM pra conseguir terminar
  future:::ClusterRegistry("stop")
  plan(multisession)
  
  modelos_plm_nacional[['variavel']][['completo']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['variavel']][['completo_ano']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(year),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['variavel']][['completo_tamanho']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(tam_estab),
        model = 'between',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['variavel']] <- value(modelos_plm_nacional[['variavel']])
  future:::ClusterRegistry("stop")
}

# PLM Nacional - Pooling
{
  future:::ClusterRegistry("stop")
  plan(multisession, gc = TRUE)
  
  modelos_plm_nacional[['pooling']] <- list()
  
  modelos_plm_nacional[['pooling']][['simples']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['pooling']][['simples_ano']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(year),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['pooling']][['simples_tamanho']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + factor(tam_estab),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  # pegar os values
  modelos_plm_nacional[['pooling']] <- value(modelos_plm_nacional[['pooling']])
  # refresh nos workers
  future:::ClusterRegistry("stop")
  plan(multisession, gc = TRUE)
  
  modelos_plm_nacional[['pooling']][['completo']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['pooling']][['completo_ano']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(year),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['pooling']][['completo_tamanho']] <- future({firmas_long %>%
    plm(log(fgrowth) ~ log(vinculos)
        + log(rem_med + 0.000000000000000000000000000000000000000001)
        + log(prop_superior + 0.000000000000000000000000000000000000000001)
        + log(locq + 0.000000000000000000000000000000000000000001)
        + log(mktpot + 0.000000000000000000000000000000000000000001)
        + factor(tam_estab),
        model = 'pooling',
        index = c('cnpj', 'year'),
        data = .)},
    globals = c('modelos_plm_nacional',
                'firmas_long'),
    packages = c('dplyr',
                 'forcats',
                 'plm'))
  
  modelos_plm_nacional[['pooling']] <- value(modelos_plm_nacional[['pooling']])
  future:::ClusterRegistry("stop")
}
# modelos_plm_nacional %>% write_rds(here('data/processed/models/modelos_plm_nacional.rds'))

# LM Alimentos -----------------------------------------------------------------
{
  future:::ClusterRegistry("stop")
  plan(multisession, gc = TRUE)
  modelos_ols_alimentos <- list()
}

# OLS 2006 -> 2010
{
  modelos_ols_alimentos[['2006a2010']] <- list()
  
  modelos_ols_alimentos[['2006a2010']][['simples']] <- future({firmas_ols %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2006_2010 ~ log(vinculos__2006),
        data = .)}, globals = c('modelos_ols_alimentos',
                                'firmas_ols'),
    packages = c('dplyr'))

  modelos_ols_alimentos[['2006a2010']][['completo']] <- future({firmas_ols %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2006_2010 ~ log(vinculos__2006)
       + log(port_dist + 0.000000000000000000000000000000000000000001)
        + log(rem_med__2006 + 0.000000000000000000000000000000000000000001)
        + log(prop_superior__2006 + 0.000000000000000000000000000000000000000001)
        + log(locq__2006 + 0.000000000000000000000000000000000000000001)
        + log(mktpot__2006 + 0.000000000000000000000000000000000000000001),
        data = .)}, globals = c('modelos_ols_alimentos',
                                'firmas_ols'),
    packages = c('dplyr',
                 'forcats'))
}

# OLS 2012 -> 2016
{
  modelos_ols_alimentos[['2012a2016']] <- list()
  
  modelos_ols_alimentos[['2012a2016']][['simples']] <- future({firmas_ols %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2012_2016 ~ log(vinculos__2012),
       data = .)}, globals = c('modelos_ols_alimentos',
                               'firmas_ols'),
    packages = c('dplyr',
                 'forcats'))
  
  modelos_ols_alimentos[['2012a2016']][['completo']] <- future({firmas_ols %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2012_2016 ~ log(vinculos__2012)
       + log(port_dist + 0.000000000000000000000000000000000000000001)
       + log(rem_med__2012 + 0.000000000000000000000000000000000000000001)
       + log(prop_superior__2012 + 0.000000000000000000000000000000000000000001)
       + log(locq__2012 + 0.000000000000000000000000000000000000000001)
       + log(mktpot__2012 + 0.000000000000000000000000000000000000000001),
       data = .)}, globals = c('modelos_ols_alimentos',
                               'firmas_ols'),
    packages = c('dplyr',
                 'forcats'))
}

# OLS 2006 -> 2016
{
  modelos_ols_alimentos[['2006a2016']] <- list()
  
  modelos_ols_alimentos[['2006a2016']][['simples']] <- future({firmas_ols %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2006_2016 ~ log(vinculos__2006),
       data = .)}, globals = c('modelos_ols_alimentos',
                               'firmas_ols'),
    packages = c('dplyr',
                 'forcats'))
  
  modelos_ols_alimentos[['2006a2016']][['completo']] <- future({firmas_ols %>%
    filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2006_2016 ~ log(vinculos__2006)
       + log(port_dist + 0.000000000000000000000000000000000000000001)
       + log(rem_med__2006 + 0.000000000000000000000000000000000000000001)
       + log(prop_superior__2006 + 0.000000000000000000000000000000000000000001)
       + log(locq__2006 + 0.000000000000000000000000000000000000000001)
       + log(mktpot__2006 + 0.000000000000000000000000000000000000000001),
       data = .)}, globals = c('modelos_ols_alimentos',
                               'firmas_ols'),
    packages = c('dplyr',
                 'forcats'))
}

# Resgatar os values dos LMs
{
  modelos_ols_alimentos[['2006a2010']] <- value(modelos_ols_alimentos[['2006a2010']])
  modelos_ols_alimentos[['2012a2016']] <- value(modelos_ols_alimentos[['2012a2016']])
  modelos_ols_alimentos[['2006a2016']] <- value(modelos_ols_alimentos[['2006a2016']])
}
# modelos_ols_alimentos %>% write_rds(here('data/processed/models/modelos_ols_alimentos.rds'))

# LM Nacional ------------------------------------------------------------------
{
  future:::ClusterRegistry("stop")
  plan(multisession, gc = TRUE)
  modelos_ols_nacional <- list()
}

# OLS 2006 -> 2010
{
  modelos_ols_nacional[['2006a2010']] <- list()
  
  modelos_ols_nacional[['2006a2010']][['simples']] <- future({firmas_ols %>%
    # filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2006_2010 ~ log(vinculos__2006),
       data = .)}, globals = c('modelos_ols_nacional',
                              'firmas_ols'),
       packages = c('dplyr',
                    'forcats'))
  
  modelos_ols_nacional[['2006a2010']][['completo']] <- future({firmas_ols %>%
    # filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2006_2010 ~ log(vinculos__2006)
       + log(port_dist + 0.000000000000000000000000000000000000000001)
       + log(rem_med__2006 + 0.000000000000000000000000000000000000000001)
       + log(prop_superior__2006 + 0.000000000000000000000000000000000000000001)
       + log(locq__2006 + 0.000000000000000000000000000000000000000001)
       + log(mktpot__2006 + 0.000000000000000000000000000000000000000001),
       data = .)}, globals = c('modelos_ols_nacional',
                               'firmas_ols'),
    packages = c('dplyr',
                 'forcats'))
}

# OLS 2012 -> 2016
{
  modelos_ols_nacional[['2012a2016']] <- list()
  
  modelos_ols_nacional[['2012a2016']][['simples']] <- future({firmas_ols %>%
    # filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2012_2016 ~ log(vinculos__2012),
       data = .)}, globals = c('modelos_ols_nacional',
                              'firmas_ols'),
       packages = c('dplyr',
                    'forcats'))
  
  modelos_ols_nacional[['2012a2016']][['completo']] <- future({firmas_ols %>%
    # filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2012_2016 ~ log(vinculos__2012)
       + log(port_dist + 0.000000000000000000000000000000000000000001)
       + log(rem_med__2012 + 0.000000000000000000000000000000000000000001)
       + log(prop_superior__2012 + 0.000000000000000000000000000000000000000001)
       + log(locq__2012 + 0.000000000000000000000000000000000000000001)
       + log(mktpot__2012 + 0.000000000000000000000000000000000000000001),
       data = .)}, globals = c('modelos_ols_nacional',
                              'firmas_ols'),
       packages = c('dplyr',
                    'forcats'))
}

# OLS 2006 -> 2016
{
  modelos_ols_nacional[['2006a2016']] <- list()
  
  modelos_ols_nacional[['2006a2016']][['simples']] <- future({firmas_ols %>%
    # filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2006_2016 ~ log(vinculos__2006),
       data = .)}, globals = c('modelos_ols_nacional',
                               'firmas_ols'),
    packages = c('dplyr',
                 'forcats'))
  
  modelos_ols_nacional[['2006a2016']][['completo']] <- future({firmas_ols %>%
    # filter(cnae >= 10000 & cnae < 11000) %>%
    lm(fgrowth_2006_2016 ~ log(vinculos__2006)
       + log(port_dist + 0.000000000000000000000000000000000000000001)
       + log(rem_med__2006 + 0.000000000000000000000000000000000000000001)
       + log(prop_superior__2006 + 0.000000000000000000000000000000000000000001)
       + log(locq__2006 + 0.000000000000000000000000000000000000000001)
       + log(mktpot__2006 + 0.000000000000000000000000000000000000000001),
       data = .)}, globals = c('modelos_ols_nacional',
                               'firmas_ols'),
    packages = c('dplyr',
                 'forcats'))
}

# Resgatar os values dos LMs
{
  modelos_ols_nacional[['2006a2010']] <- value(modelos_ols_nacional[['2006a2010']])
  modelos_ols_nacional[['2012a2016']] <- value(modelos_ols_nacional[['2012a2016']])
  modelos_ols_nacional[['2006a2016']] <- value(modelos_ols_nacional[['2006a2016']])
  future:::ClusterRegistry("stop")
}
# modelos_ols_nacional %>% write_rds(here('data/processed/models/modelos_ols_nacional.rds'))
gc()