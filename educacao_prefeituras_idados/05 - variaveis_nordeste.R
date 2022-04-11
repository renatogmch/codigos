# Criacação das variaveis sobre o censo escolar 2020
# modelo da consultoria iDados
# Renato Gomes Chaves
# Criacao 08/10/2021
# Ultima modificacao: 22/11/2021

# Carregar Pacotes #############################################################
{
  require(pacman)
  p_load(tidyverse,
         here
  )
  # no_cores <- detectCores()
  # registerDoParallel(no_cores)
  # aumentar max mem ram com pagefile
  # utils::memory.limit(32000)
  # cat('Memoria RAM maxima:', memory.limit())
  # options(future.globals.maxSize = 2500 * 1024^2)
}
# IMPORTANTE
# group_by(var1, var2) cria os grupos para summarise usanso primeiro var2

# Importar dados limpos ########################################################
{
  matriculas_2020_nordeste <- read_rds(here('data/processed/matriculas_2020_nordeste.rds'))
  turmas_2020_nordeste <- read_rds(here('data/processed/turmas_2020_nordeste.rds'))
  escolas_2020_nordeste <- read_rds(here('data/processed/escolas_2020_nordeste.rds'))
  docentes_2020_nordeste <- read_rds(here('data/processed/docentes_2020_nordeste.rds'))
}

# Dados sumarizados ############################################################
{
  # Selecionar estado ----------------------------------------------------------
  estado <- matriculas_2020_nordeste %>% 
    pull(nome_uf) %>% 
    droplevels() %>% 
    levels() %>% 
    select.list(., title = "Selecione Estado")
  
  # Selecionar municipio
  municipio <- matriculas_2020_nordeste %>% 
    filter(nome_uf == as.character(estado)) %>% 
    pull(nome_mun) %>% 
    droplevels() %>% 
    levels() %>% 
    select.list(., title = "Selecione Municipio")
}

# Matriculas X Rede de Ensino --------------------------------------------------
matriculas_2020_nordeste %>% 
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>% 
  group_by('Rede de Ensino' = TP_DEPENDENCIA) %>% 
  summarise('Matriculas' = n()) %>% 
  mutate(prop = Matriculas/sum(Matriculas)) %>% 
  arrange(desc(Matriculas)) %>% 
  {. ->> matriculas_rede} %>% 
  print()
# Grafico Pizza
matriculas_rede %>% 
  ggplot(aes(2,
             prop,
             fill = `Rede de Ensino`)) +
  geom_bar(stat = 'identity',
           color = 'white')+
  coord_polar(theta = 'y', 
              start = 0) +
  labs(title = 'Matriculas por Rede de Ensino') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(values = c('#0073C2FF',
                               '#EFC000FF',
                               '#868686FF',
                               '#CD534CFF')) +
  # theme_void() +
  xlim(0.5, 2.5)





matriculas_2020_nordeste %>% 
  filter(nome_uf == estado) %>% 
  group_by('Rede de Ensino' = TP_DEPENDENCIA) %>% 
  summarise('Matriculas' = n()) %>% 
  arrange(desc(Matriculas)) %>% 
  # piechart
  ggplot(aes(x = 2, y = Matriculas, fill = `Rede de Ensino`)) +
  geom_bar(stat = "identity", color = "white") +
  # coord_polar faz a barra virar uma pizza
  coord_polar(theta = "y", start = 0)+
  # texto no meio do grafico
  # geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  # valores das cores manualmente
  scale_fill_manual(values = c("#0073C2FF",
                               "#EFC000FF",
                               "#868686FF",
                               "#CD534CFF")) +
  # tema vazio
  theme_void() +
  # xlim necessario para virar rosquinha
  xlim(0.5, 2.5)


# Urbanas X Rurais -------------------------------------------------------------
matriculas_2020_nordeste %>% 
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>%
  group_by('Zona da Escola' = TP_LOCALIZACAO) %>% 
  summarise('Matriculas' = n()) %>% 
  mutate(prop = Matriculas/sum(Matriculas)) %>% 
  {. ->> urbanas_rurais} %>% 
  arrange(desc(Matriculas))
  
# % de turmas em tempo integral ------------------------------------------------
# https://educacaointegral.org.br/glossario/educacao-em-tempo-integral/
# O artigo 36 do parecer numero sete da Resolucao do Conselho Nacional de
# Educacao (2010) aponta que e considerado periodo integral toda jornada
# escolar organizada em SETE HORAS DIA?RIAS (420 min), resultando em carga
# horaria anual de  1.400 horas.
matriculas_2020_nordeste %>%
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>% 
  group_by(duracao_turma = NU_DURACAO_TURMA,
           ID_TURMA) %>%
  summarise() %>% 
  summarise(numero_turmas = n()) %>% 
  mutate(prop = numero_turmas/sum(numero_turmas)) %>% 
  filter(duracao_turma >= 420) %>% 
  {. ->> integral_turmas_municipal} %>% 
  pull(prop) %>% 
  sum()

# % de escolas municipais que oferecem tempo integral --------------------------
matriculas_2020_nordeste %>%
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>%
  group_by(CO_ENTIDADE,
           TP_DEPENDENCIA) %>% 
  summarise(NU_DURACAO_TURMA = max(NU_DURACAO_TURMA)) %>% 
  ungroup() %>%
  group_by(rede_ensino = TP_DEPENDENCIA) %>% 
  summarise(integral = length(CO_ENTIDADE[NU_DURACAO_TURMA>=420]),
            regulares = length(CO_ENTIDADE[NU_DURACAO_TURMA<420]),
            total = length(CO_ENTIDADE),
            prop = integral/total) %>% 
  {. ->> integral_escolas_municipais} %>%
  filter(rede_ensino == 'Municipal') %>% 
  pull(prop)

# Matricula X demanda por vagas ------------------------------------------------
# falta dados IBGE

# Escolas X Rede ---------------------------------------------------------------
matriculas_2020_nordeste %>%
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>% 
  group_by('Rede da Escola' = TP_DEPENDENCIA, CO_ENTIDADE) %>% 
  summarise() %>% 
  summarise(Total = n()) %>% 
  {. ->> escolas_por_rede} %>% 
  print()

# Indice de Ocupacao das Escolas -----------------------------------------------
# Turmas X Turno
turmas_2020_nordeste %>% 
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>% 
  # turmas que durem mais de 420min sao consideradas de tempo integral
  mutate(turno_turma = case_when(NU_DURACAO_TURMA < 420 & TX_HR_INICIAL < 12 ~ 'manha',
                                 NU_DURACAO_TURMA < 420 & TX_HR_INICIAL >= 12 ~ 'tarde',
                                 NU_DURACAO_TURMA >= 420 ~ 'integral')) %>% 
  group_by(turno_turma) %>% 
  summarise(total = n()) %>% 
  {. ->> turmas_por_turno} %>% 
  print()

# Ociosidade X Escola ----------------------------------------------------------
left_join(
  # Turmas X Escola
  turmas_2020_nordeste %>% 
    # filter(nome_uf == estado) %>% 
    filter(nome_mun == municipio) %>% 
    mutate(turno_turma = case_when(NU_DURACAO_TURMA < 420 & TX_HR_INICIAL < 12 ~ 'manha',
                                   NU_DURACAO_TURMA < 420 & TX_HR_INICIAL >= 12 ~ 'tarde',
                                   NU_DURACAO_TURMA >= 420 ~ 'integral')
    ) %>% 
    # tem turma sem duracao, resultando em NAs
    drop_na() %>% 
    group_by(escola = CO_ENTIDADE) %>% 
    summarise(turmas_integral = sum(turno_turma %in% 'integral'),
              turmas_manha = sum(turno_turma %in% 'manha'),
              turmas_tarde = sum(turno_turma %in% 'tarde')
    ),
  # Salas x Escola
  escolas_2020_nordeste %>% 
    # filter(nome_uf == estado) %>% 
    filter(nome_mun == municipio) %>%
    select(escola = CO_ENTIDADE,
           salas_disponiveis = QT_SALAS_UTILIZADAS)
) %>% 
  mutate(across(c(turmas_manha:salas_disponiveis), as.double)) %>% 
  # calcular salas sobrando por turno levando em conta que turmas de periodo
  # integral precisam ser contadas nos dois periodos
  mutate(ociosidade_manha = salas_disponiveis - turmas_manha - turmas_integral,
         ociosidade_tarde = salas_disponiveis - turmas_tarde - turmas_integral
         ) %>% 
  {. ->> ociosidade_escola} %>% 
  print()

# Percentual de Ociosidade X Turno ---------------------------------------------
ociosidade_escola %>% 
  summarise(ociosidade_manha = sum(ociosidade_manha)/sum(salas_disponiveis),
            ociosidade_tarde_total = sum(ociosidade_tarde)/sum(salas_disponiveis)) %>% 
  {. ->> ociosidade_percentual} %>% 
  print()

# Observacoes
# A ociosidade muda dependendo de como for feito o calculo. Se uma escola nao
# funciona em um dos turnos, suas salas podem ou nao serem consideradas ociosas.
# Aqui o calculo foi feito independentemente do horario de funcionamento da 
# escola, portanto se a sala existe e nao ha turmas no turno ela sera
# considerada ociosa.

# Ocupacao X Nivel Escolar------------------------------------------------------
# quantidade de salas utilizadas sem superlotação e evitando a ociosidade
# anos inicias: 1 ao 6 ano
# anos finais: 7 ao 9 ano
# A Comissão de Educação da Câmara dos Deputados aprovou o Projeto de 
# Lei 4731/12, do Senado, que fixa em 25 o máximo de alunos na pré-escola e 
# nos dois primeiros anos do ensino fundamental e em 35 nos demais anos do 
# ensino fundamental e no ensino médio. (2021)
# https://www.camara.leg.br/noticias/818991-comissao-aprova-projeto-que-limita-o-numero-de-alunos-em-sala-de-aula
matriculas_2020_nordeste %>% 
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>%
  filter(str_detect(TP_ETAPA_ENSINO, 'Educacao Infantil') |
           str_detect(TP_ETAPA_ENSINO, 'Ensino Fundamental de 9 anos')) %>%
  mutate(alunos_turma_max = case_when(TP_ETAPA_ENSINO %in%
                                        c('Ensino Fundamental de 9 anos=3 Ano',
                                          'Ensino Fundamental de 9 anos=4 Ano',
                                          'Ensino Fundamental de 9 anos=5 Ano',
                                          'Ensino Fundamental de 9 anos=6 Ano',
                                          'Ensino Fundamental de 9 anos=7 Ano',
                                          'Ensino Fundamental de 9 anos=8 Ano',
                                          'Ensino Fundamental de 9 anos=9 Ano') ~ 35,
                                      TP_ETAPA_ENSINO %in%
                                        c('Educacao Infantil=Pre-escola',
                                          'Educacao Infantil=Creche',
                                          'Ensino Fundamental de 9 anos=1 Ano',
                                          'Ensino Fundamental de 9 anos=2 Ano') ~ 25),
         tipo_turma = if_else(NU_DURACAO_TURMA < 420, 'parcial', 'integral')
         ) %>% 
  group_by(ano_escolar = TP_ETAPA_ENSINO) %>% 
  summarise(matriculas_integral = sum(tipo_turma %in% 'integral'),
            matriculas_parcial = sum(tipo_turma %in% 'parcial'),
            total_turmas = n_distinct(ID_TURMA),
            mat_por_turma = (matriculas_integral+matriculas_parcial)/total_turmas,
            alunos_turma_max = unique(alunos_turma_max)
  ) %>% 
  # de acordo com o PL 4731/12 citado acima
  mutate(turmas_otimo_integral = ceiling(matriculas_integral/alunos_turma_max),
         turmas_otimo_parcial = ceiling(matriculas_parcial/alunos_turma_max),
         total_turmas_otimo = turmas_otimo_integral + turmas_otimo_parcial,
         nivel = case_when(ano_escolar %in%
                             c('Educacao Infantil=Pre-escola',
                               'Educacao Infantil=Creche') ~ 'ensino_infantil',
                           ano_escolar %in%
                             c('Ensino Fundamental de 9 anos=1 Ano',
                               'Ensino Fundamental de 9 anos=2 Ano',
                               'Ensino Fundamental de 9 anos=3 Ano',
                               'Ensino Fundamental de 9 anos=4 Ano',
                               'Ensino Fundamental de 9 anos=5 Ano') ~ 'anos_iniciais',
                           ano_escolar %in%
                             c('Ensino Fundamental de 9 anos=6 Ano',
                               'Ensino Fundamental de 9 anos=7 Ano',
                               'Ensino Fundamental de 9 anos=8 Ano',
                               'Ensino Fundamental de 9 anos=9 Ano') ~ 'anos_finais'
                           )
         ) %>% 
  {. ->> ocupacao_por_ano} %>% 
  group_by(nivel) %>% 
  summarise(alunos_integral = sum(matriculas_integral),
            alunos_parcial = sum(matriculas_parcial),
            turmas_necessarias = sum(total_turmas_otimo),
            salas_existentes = sum(total_turmas)
  ) %>% 
  arrange(desc(nivel)) %>%  
  {. ->> ocupacao_por_nivel} %>% 
  print()

# Indice de Superlotacao--------------------------------------------------------
# percentual de turmas que contam com alunos em excesso
# usei base matriculas mas teria sido melhor usar base turmas
# ha disparate entre qt de turmas entre as duas bases
matriculas_2020_nordeste %>%
  filter(nome_mun == municipio) %>%
  filter(TP_ETAPA_ENSINO %in%
           c('Educacao Infantil=Pre-escola',
             'Educacao Infantil=Creche',
             'Ensino Fundamental de 9 anos=1 Ano',
             'Ensino Fundamental de 9 anos=2 Ano',
             'Ensino Fundamental de 9 anos=3 Ano',
             'Ensino Fundamental de 9 anos=4 Ano',
             'Ensino Fundamental de 9 anos=5 Ano',
             'Ensino Fundamental de 9 anos=6 Ano',
             'Ensino Fundamental de 9 anos=7 Ano',
             'Ensino Fundamental de 9 anos=8 Ano',
             'Ensino Fundamental de 9 anos=9 Ano')
         ) %>%
  group_by(CO_ENTIDADE, TP_ETAPA_ENSINO, ID_TURMA) %>% 
  summarise(total_alunos = n()) %>% 
  # existem turmas repetidas, onde o mesmo ID_TURMA se refere a multiplos anos
  # de ensino. Estes serao desconsiderados.
  group_by(ID_TURMA) %>% 
  # tirar as obs duplicadas
  filter(n() == 1) %>% 
  mutate(alunos_turma_max = case_when(TP_ETAPA_ENSINO %in%
                                        c('Ensino Fundamental de 9 anos=3 Ano',
                                          'Ensino Fundamental de 9 anos=4 Ano',
                                          'Ensino Fundamental de 9 anos=5 Ano',
                                          'Ensino Fundamental de 9 anos=6 Ano',
                                          'Ensino Fundamental de 9 anos=7 Ano',
                                          'Ensino Fundamental de 9 anos=8 Ano',
                                          'Ensino Fundamental de 9 anos=9 Ano') ~ 35,
                                      TP_ETAPA_ENSINO %in%
                                        c('Educacao Infantil=Pre-escola',
                                          'Educacao Infantil=Creche',
                                          'Ensino Fundamental de 9 anos=1 Ano',
                                          'Ensino Fundamental de 9 anos=2 Ano') ~ 25),
         condicao = if_else((alunos_turma_max - total_alunos) > 0,
                            'regular',
                            'superlotada')
  ) %>% 
  {. ->> turmas_superlotadas} %>% 
  ungroup() %>% 
  summarise(superlotada = sum(condicao %in% 'superlotada'),
            regular = sum(condicao %in% 'regular'),
            percentual_superlotado = superlotada/(superlotada + regular)) %>% 
  {. ->> superlotacao_turmas} %>% 
  print()
  
# Escolas e turmas com superlotacao
turmas_superlotadas %>% 
  ungroup() %>%
  filter(condicao == 'superlotada') %>% 
  select(CO_ENTIDADE, TP_ETAPA_ENSINO) %>% 
  mutate(TP_ETAPA_ENSINO = str_remove(TP_ETAPA_ENSINO, "Ensino Fundamental de 9 anos=|Educacao Infantil=")
  ) %>% 
  group_by(escola = CO_ENTIDADE,
           TP_ETAPA_ENSINO) %>% 
  summarise(total = n()) %>%
  pivot_wider(id_cols = escola,
              names_from = TP_ETAPA_ENSINO,
              values_from = total,
              values_fill = 0) %>%
  relocate(`Pre-escola`, .after = Creche) %>%
  relocate(`3 Ano`, .after = `2 Ano`) %>%
  rowwise() %>%
  # c_Acorss pra somar por linha do rowwise
  mutate(Total = sum(c_across(Creche:`9 Ano`))) %>% 
  {. ->> escolas_superlotadas} %>% 
  print()

# Indice de Descontinuidade ----------------------------------------------------
# Painel wide com quantidade de turmas por ano por escola
turmas_2020_nordeste %>%
  filter(nome_mun == municipio) %>%
  filter(TP_ETAPA_ENSINO %in%
           c('Ensino Fundamental de 9 anos - 1 Ano',
             'Ensino Fundamental de 9 anos - 2 Ano',
             'Ensino Fundamental de 9 anos - 3 Ano',
             'Ensino Fundamental de 9 anos - 4 Ano',
             'Ensino Fundamental de 9 anos - 5 Ano',
             'Ensino Fundamental de 9 anos - 6 Ano',
             'Ensino Fundamental de 9 anos - 7 Ano',
             'Ensino Fundamental de 9 anos - 8 Ano',
             'Ensino Fundamental de 9 anos - 9 Ano')
  ) %>%
  group_by(escola = CO_ENTIDADE,
           TP_ETAPA_ENSINO) %>% 
  summarise(total = n()) %>% 
  mutate(TP_ETAPA_ENSINO = str_remove(TP_ETAPA_ENSINO,
                                      "Ensino Fundamental de 9 anos - |Educacao Infantil=")
  ) %>% 
  pivot_wider(id_cols = escola,
              names_from = TP_ETAPA_ENSINO,
              values_from = total,
              names_sort = T
  ) %>% 
  {. ->> escolas_descontinuidade} %>% 
  print()

# Percentual de escolas com descontinuidade 
# Total de escolas com descontinuidade sobre total de escolas

# Anos iniciais
{escolas_descontinuidade %>% 
  select(escola,
        `1 Ano`:`5 Ano`) %>%
  # filtrar escolas que nao tenhao pelo menos um ano
  # ou seja: filtrar linhas que tem NA
  filter(if_any(everything(), ~ is.na(.))) %>%  
  # filtrar escolas que oferecam anos iniciais
  # ou seja: excluir linhas que sejam toda NA
  filter(if_any(everything(), ~ !is.na(.))) %>%
  nrow()/
escolas_descontinuidade %>%  
  nrow()} %>% 
  as_tibble() %>% 
  rename(percentual_desc_iniciais = value) %>% 
  {. ->> percentual_desc_iniciais} %>% 
  print()

# Anos finais
{escolas_descontinuidade %>% 
  select(escola,
         `6 Ano`:`9 Ano`) %>%
  # filtrar escolas que nao tenhao pelo menos um ano
  # ou seja: filtrar linhas que tem NA
  filter(if_any(everything(), ~ is.na(.))) %>%  
  # filtrar escolas que oferecam anos iniciais
  # ou seja: excluir linhas que sejam toda NA
  filter(if_any(everything(), ~ !is.na(.))) %>%
  nrow()/
  escolas_descontinuidade %>%  
  nrow()} %>% 
  as_tibble() %>% 
  rename(percentual_desc_finais = value) %>% 
  {. ->> percentual_desc_finais} %>% 
  print()

# Descontinuidade de Séries por Escola -----------------------------------------
# Escolas com descontinuidade nos anos iniciais
escolas_descontinuidade %>% 
  select(escola,
         `1 Ano`:`5 Ano`) %>%
  # filtrar escolas que nao tenhao pelo menos um ano
  # ou seja: filtrar linhas que tem NA
  filter(if_any(everything(), ~ is.na(.))) %>%  
  # filtrar escolas que oferecam anos iniciais
  # ou seja: excluir linhas que sejam toda NA
  filter(if_any(everything(), ~ !is.na(.))) %>% 
  # trocar todos os NA por 0
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  # criar colunas com nomes dos anos que estao faltando na escola
  mutate(across(everything(), ~ case_when(. == 0 ~ cur_column()), .names = 'new_{col}')
  ) %>% 
  # unite = concatenar
  unite(anos_faltando,
        starts_with('new'),
        na.rm = TRUE,
        sep = ', ',
        remove = T) %>% 
  select(escola,
         anos_faltando) %>%
  {. ->> escolas_desc_iniciais} %>% 
  print()

# Escolas com descontinuidade nos anos finais
escolas_descontinuidade %>% 
  select(escola,
         `6 Ano`:`9 Ano`) %>%
  # filtrar escolas que nao tenhao pelo menos um ano
  # ou seja: filtrar linhas que tem NA
  filter(if_any(everything(), ~ is.na(.))) %>%  
  # filtrar escolas que oferecam anos iniciais
  # ou seja: excluir linhas que sejam toda NA
  filter(if_any(everything(), ~ !is.na(.))) %>% 
  # trocar todos os NA por 0
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  # criar colunas com nomes dos anos que estao faltando na escola
  mutate(across(everything(), ~ case_when(. == 0 ~ cur_column()), .names = 'new_{col}')
  ) %>% 
  # unite = concatenar
  unite(anos_faltando,
        starts_with('new'),
        na.rm = TRUE,
        sep = ', ',
        remove = T) %>% 
  select(escola,
         anos_faltando) %>%
  {. ->> escolas_desc_finais} %>% 
  print()

# Indice de Adequacao do Mobiliario --------------------------------------------
# percentual de escolas que utilizam carteiras inadequadas ao tamanho dos alunos
# Nao sei como identificar as salas que sao usadas por mais de uma turma
# Nao ha variavel que etiquete cada sala para cruzar as turmas
# jeito seria uma escola com qt de salas total menor que qt de turmas de
# forma que as turmas sao obrigadas a compartilhar salas
# left_join(
#   # Salas x Escola
#   escolas_2020_nordeste %>% 
#             # filter(nome_uf == estado) %>% 
#             filter(nome_mun == municipio) %>%
#             select(escola = CO_ENTIDADE,
#                    salas_disponiveis = QT_SALAS_UTILIZADAS),
#   # Turmas X Escola X Ano
#   turmas_2020_nordeste %>%
#     # filter(nome_uf == estado) %>% 
#     filter(nome_mun == municipio) %>%
#     filter(TP_ETAPA_ENSINO %in%
#              c('Ensino Fundamental de 9 anos - 1 Ano',
#                'Ensino Fundamental de 9 anos - 2 Ano',
#                'Ensino Fundamental de 9 anos - 3 Ano',
#                'Ensino Fundamental de 9 anos - 4 Ano',
#                'Ensino Fundamental de 9 anos - 5 Ano',
#                'Ensino Fundamental de 9 anos - 6 Ano',
#                'Ensino Fundamental de 9 anos - 7 Ano',
#                'Ensino Fundamental de 9 anos - 8 Ano',
#                'Ensino Fundamental de 9 anos - 9 Ano',
#                'Educacao Infantil - Creche',
#                'Educacao Infantil - Pre-escola')
#     ) %>%
#     mutate(TP_ETAPA_ENSINO = str_remove(TP_ETAPA_ENSINO,
#                                         "Ensino Fundamental de 9 anos - |Educacao Infantil - ")
#     ) %>% 
#     group_by(escola = CO_ENTIDADE,
#              TP_ETAPA_ENSINO) %>% 
#     summarise(total = n()) %>% 
#     pivot_wider(id_cols = escola,
#                 names_from = TP_ETAPA_ENSINO,
#                 values_from = total,
#                 names_sort = T
#     )
# ) %>% 
#   group_by(escola) %>% 
#   filter(!if_all(everything(), ~ is.na(.))) %>% 
#   filter(if_all(`1 Ano`:`Pre-escola`, ~ is.na(.)))


# Professores ------------------------------------------------------------------
# tabela com total de docentes por categoria de contratacao
docentes_2020_nordeste %>% 
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>%
  group_by(TP_TIPO_CONTRATACAO) %>% 
  summarise(total = n()) %>% 
  {. ->> docentes_categoria} %>% 
  print()

# professores efetivos com mais de 40 anos que poderao se aposentar nos prox 15
docentes_2020_nordeste %>% 
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>%
  filter(TP_TIPO_CONTRATACAO == 'Concursado/efetivo/estavel') %>% 
  mutate(idade = as.numeric(format(Sys.Date(), '%Y')) - NU_ANO) %>%
  group_by(!!paste0('faixa_etaria_docentes_', format(Sys.Date(), '%Y')) := cut(idade,
                                                breaks = c(19,
                                                           26,
                                                           31,
                                                           36,
                                                           41,
                                                           46,
                                                           51,
                                                           56,
                                                           61,
                                                           66,
                                                           Inf),
                                                labels = c('20 a 25 anos',
                                                           '25 a 30 anos',	
                                                           '30 a 35 anos',
                                                           '35 a 40 anos',	
                                                           '40 a 45 anos',	
                                                           '45 a 50 anos',
                                                           '50 a 55 anos',
                                                           '55 a 60 anos',
                                                           '60 a 65 anos',
                                                           'Acima de 65 anos'
                                                           )
                            )
           ) %>% 
  summarise(quantidade = n()) %>% 
  {. ->> docentes_idade} %>% 
  print()

# docentes_idade %>% 
#   ggplot(aes(x = quantidade, y = faixa_etaria_docentes_2021)) +
#   geom_bar(stat = "identity")

# Expectativa populacional por faixa etária por ano
# falta pegar dados do IBGE

# % de alunos da rede municipal que usam transporte publico
matriculas_2020_nordeste %>% 
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>%
  filter(TP_RESPONSAVEL_TRANSPORTE  == 'Municipal' |
           IN_TRANSPORTE_PUBLICO == T) %>%
  count()/
  matriculas_2020_nordeste %>% 
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>%
  count() %>% 
  {. ->> percentual_alunos_transporte} %>% 
  print()

# Alunos transportados por rede e escola
matriculas_2020_nordeste %>% 
  # filter(nome_uf == estado) %>% 
  filter(nome_mun == municipio) %>% 
  filter(TP_DEPENDENCIA != 'Privada') %>% 
  filter(TP_RESPONSAVEL_TRANSPORTE  == 'Municipal' |
         TP_RESPONSAVEL_TRANSPORTE  == 'Estadual') %>% 
  filter(IN_TRANSPORTE_PUBLICO == T) %>% 
  group_by('rede' = TP_DEPENDENCIA,
           'loc_escola' = TP_LOCALIZACAO) %>% 
  summarise(transp_municipal = sum(TP_RESPONSAVEL_TRANSPORTE %in% 'Municipal'),
            transp_estadual = sum(TP_RESPONSAVEL_TRANSPORTE %in% 'Estadual'),
            total = n()) %>% 
  arrange(desc(rede), desc(loc_escola)) %>% 
  {. ->> transporte_alunos} %>% 
  print()


# Graficos #####################################################################
# Matriculas por rede de ensino
matriculas_2020_nordeste %>% 
  filter(nome_uf == estado) %>% 
  group_by('Rede de Ensino' = TP_DEPENDENCIA) %>% 
  summarise('Matriculas' = n()) %>% 
  arrange(desc(Matriculas)) %>% 
  # piechart
  ggplot(aes(x = 2, y = Matriculas, fill = `Rede de Ensino`)) +
  geom_bar(stat = "identity", color = "white") +
  # coord_polar faz a barra virar uma pizza
  coord_polar(theta = "y", start = 0)+
  # texto no meio do grafico
  # geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  # valores das cores manualmente
  scale_fill_manual(values = c("#0073C2FF",
                               "#EFC000FF",
                               "#868686FF",
                               "#CD534CFF")) +
  # tema vazio
  theme_void() +
  # xlim necessario para virar rosquinha
  xlim(0.5, 2.5)

