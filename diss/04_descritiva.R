# Script Dissertacao ----
# estatistica descritiva
# Data: 28/07/2020

# Pacotes ----------------------------------------------------------------------
{
  require(pacman)
  p_load(here,
         tidyverse,
         # pacote para os mapas
         geobr,
         # pacote para ajeitar as legendas dos mapas
         scales)
  # utils::memory.limit(32000)
  # cat('Memoria RAM maxima:', memory.limit())
}
# to-do
# criar variavel de quantidade de trab com ensino superior OK
# rodar novamente o final do script rais trab  OK
# tirar o log do fgrowth pra o summary da descritiva OK
# adicionar log() no fgrowth das funcoes plm OK
# ajeitar fgrowth no LM (tirar o log)

# Tabelas Descritivas ----------------------------------------------------------
# Primeiro criar a base de dados apenas com as firmas sobreviventes, as mesmas
# utilizadas para os modelos
read_rds(here('data/processed/firmas.rds')) %>%
  # usei o inner_join pra cnae e cnpj, pra ter soh as sobreviventes dos anos.
  # resultado 200k+ firmas. reduce pra juntar todos os dfs. Inner pra sobrar
  # so o que tiver nos 2 com mesmo cnpj, cnae e cod do municipio.
  reduce(inner_join, by = c('cnpj', 'cnae', 'cod_mun')) %>%
  # adiciona o fgrowth
  left_join(read_rds(here('data/processed/fgrowth.rds')), by = 'cnpj') %>%
  # selecionar cada variavel de cada df
  select(cnpj,
         cod_mun,
         cnae,
         starts_with('vinculos'),
         starts_with('rem_med'),
         starts_with('total_superior'),
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
  # salvar base inteira
  {. ->> firmas_BR } %>% 
  # criar base separada apenas com as firmas sobreviventes na industria de
  # interesse, a industria alimenticia.
  filter(cnae >= 10000 & cnae < 11000) %>% 
  {. ->> firmas_BR_cnae }

firmas_BR_cnae_long <- firmas_BR_cnae %>%
  select(-contains('__2016'),
         -cnae,
         -cod_mun) %>%
  pivot_longer(-c(cnpj),
               names_sep = '__',
               names_to = c('.value', 'year')) %>%
  # 2 PJs  estava am municipios que n tem dados, resultando NA
  # solucao mais rapida foi excluir
  drop_na()

firmas_BR_cnae_long %>% 
  filter(total_superior > 100) %>% 
  ggplot(data = .,
         aes(year, total_superior)) +
  geom_point() +
  geom_smooth()


stat_summary(aes(group = total_superior),fun=mean,geom="point") + 
  stat_summary(aes(group = total_superior),fun=mean,geom="line",alpha=0.7,linetype="dashed")

firmas_BR_cnae %>% 
  select(contains('2006'),
         -cnpj,
         -cod_mun,
         -cnae,
         -contains('tam_estab')) %>% 
  summary()


# Mapas ########################################################################
# Carregar codigos das regioes brasileiras
cod_regioes <- readxl::read_xlsx(here('data/raw/extra/regioes_geograficas_composicao_por_municipios_2017.xlsx')) %>% 
  select(cod_mun,
         cod_rgint,
         cod_uf) %>% 
  mutate(cod_mun = as.character(cod_mun),
         cod_rgint = as.character(cod_rgint),
         cod_uf = as.character(cod_uf))

# Carregar dados inteiros para 2006
firmas_2006 <- read_rds(here('data/processed/firmas.rds')) %>%
  pluck('2006')

# Sumarizar numero de cnpj's por regioes int
firmas_rgint_2006 <- firmas_2006 %>% 
  left_join(.,
            cod_regioes,
            by = 'cod_mun') %>%
  group_by(cod_rgint) %>%
  summarise(vinculos_total = sum(vinculos__2006)) %>% 
  drop_na()

# Sumarizar numero de cnpj's por Estado
firmas_estados_2006 <- firmas_2006 %>% 
  left_join(.,
            cod_regioes,
            by = 'cod_mun') %>%
  group_by(cod_uf) %>%
  summarise(vinculos_total = sum(vinculos__2006)) %>% 
  drop_na()

# Carregar dados inteiros para 2016
firmas_2016 <- read_rds(here('data/processed/firmas.rds')) %>%
  pluck('2016')

# Sumarizar numero de cnpj's por regioes int
firmas_rgint_2016 <- firmas_2016 %>% 
  left_join(.,
            cod_regioes,
            by = 'cod_mun') %>%
  group_by(cod_rgint) %>%
  summarise(vinculos_total = sum(vinculos__2016)) %>% 
  drop_na()

# Sumarizar numero de cnpj's por Estado
firmas_estados_2016 <- firmas_2016 %>% 
  left_join(.,
            cod_regioes,
            by = 'cod_mun') %>%
  group_by(cod_uf) %>%
  summarise(vinculos_total = sum(vinculos__2016)) %>% 
  drop_na()

# Mapas poligonais dos esdados
poly_estados <- read_state(code_state = 'all',
                           year = 2019,
                           simplified = T,
                           showProgress = T) %>%
  mutate(code_state = as.character(code_state))

# Mapas poligonais das regioes int
poly_reg_inter <- read_intermediate_region(code_intermediate = 'all',
                                           year = 2019,
                                           simplified = T,
                                           showProgress = T) %>%
  mutate(code_intermediate = as.character(code_intermediate))

# Gerar os mapas ===============================================================
# Mapas para 2016
# Mapa por estados 2016
poly_estados %>% 
  left_join(firmas_estados_2016,
            by = c('code_state' = 'cod_uf')) %>% 
  ggplot(data = .) +
  geom_sf(aes(fill = vinculos_total),
          color = NA,
          size = .15,
          show.legend = T) +
  scale_fill_distiller(palette = "Blues",
                       name="Number of Jobs",
                       trans = 'reverse',
                       labels = comma_format(big.mark = ".",
                                             decimal.mark = ",")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        legend.position="right",
        legend.justification="right",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-50,0,-50,-50)
        )

# Mapa por regiao intermediaria 2016
poly_reg_inter %>% 
  left_join(firmas_rgint_2016,
            by = c('code_intermediate' = 'cod_rgint')) %>% 
  ggplot(data = .) +
  geom_sf(aes(fill = vinculos_total),
          color = NA,
          size = .15,
          show.legend = T) +
  labs(x = '',
       y = '')+
  scale_fill_distiller(palette = "Blues",
                       name="Number of Jobs",
                       trans = 'reverse',
                       labels = comma_format(big.mark = ".",
                                             decimal.mark = ",")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        legend.position="right",
        legend.justification="right",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-50,0,-50,-50)
  )

# Mapas para 2006
# Mapa por estados 2006
poly_estados %>% 
  left_join(firmas_estados_2006,
            by = c('code_state' = 'cod_uf')) %>% 
  ggplot(data = .) +
  geom_sf(aes(fill = vinculos_total),
          color = NA,
          size = .15,
          show.legend = T) +
  labs(x = '',
       y = '')+
  scale_fill_distiller(palette = "Reds",
                       name="Number of Jobs",
                       trans = 'reverse',
                       labels = comma_format(big.mark = ".",
                                             decimal.mark = ",")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        legend.position="right",
        legend.justification="right",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-50,0,-50,-50)
  )

# Mapa por regiao intermediaria 2006
poly_reg_inter %>% 
  left_join(firmas_rgint_2006,
            by = c('code_intermediate' = 'cod_rgint')) %>% 
  ggplot(data = .) +
  geom_sf(aes(fill = vinculos_total),
          color = NA,
          size = .15,
          show.legend = T) +
  labs(x = '',
       y = '')+
  scale_fill_distiller(palette = "Reds",
                       name = "Number of Jobs",
                       trans = 'reverse',
                       labels = comma_format(big.mark = ".",
                                             decimal.mark = ",")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        legend.position="right",
        legend.justification="right",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-50,0,-50,-50)
  )


# Salvar individualmente em 450x450
# nomes: estados_20XXX e rgint_20XX



# grafico dados em painel simultaneo

# tabelas dos resultados das regressoes

# regressoes PLM


# regressoes do corpo do texto
# regresoes de apendice

# regressoes OLS
# regressoes do corpo do texto
# regresoes de apendice