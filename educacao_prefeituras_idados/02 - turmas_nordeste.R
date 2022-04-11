# Dados de educacao 2020 - Base TURMAS
# modelo da consultoria iDados
# Renato Gomes Chaves
# Criacao 08/10/2021
# Ultima modificacao: 14/11/2021

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

# Carregar dados ###############################################################
# Carregar dados das regioes brasileiras
regioes_br <- readxl::read_xlsx(here('data/extra/regioes_geograficas_composicao_por_municipios_2017.xlsx'),
                                col_types = 'text')

# Subdividir regioes
# por Estados
estados_br <- regioes_br %>%
  group_by(nome_uf,
           co_uf = cod_uf) %>%
  summarise() %>% 
  mutate(nome_uf = as_factor(nome_uf))

# por Municipios
municipios_br <- regioes_br %>%
  group_by(nome_mun,
           co_municipio_dv = cod_mun_dv) %>%
  summarise() %>% 
  mutate(nome_mun = as_factor(nome_mun))

# Carregar apenas as colunas selecionadas
turmas_2020_nordeste <- read_delim(here('data/raw/microdados_educacao_basica_2020/DADOS/turmas.CSV'),
                                       '|',
                                       # n_max = 10000,
                                       # escape_double = FALSE,
                                       trim_ws = TRUE,
                                       locale = locale(encoding = "ISO-8859-1",
                                                       decimal_mark = ','),
                                       col_types = cols_only(# DADOS DA MATRICULA
                                         # NU_ANO_CENSO = col_double(),
                                         ID_TURMA = 'c',
                                         # NO_TURMA = col_character(),
                                         # TP_MEDIACAO_DIDATICO_PEDAGO = col_double(),
                                         TX_HR_INICIAL = 'd',
                                         # TX_MI_INICIAL = col_character(),
                                         # IN_DIA_SEMANA_DOMINGO = col_double(),
                                         # IN_DIA_SEMANA_SEGUNDA = col_double(),
                                         # IN_DIA_SEMANA_TERCA = col_double(),
                                         # IN_DIA_SEMANA_QUARTA = col_double(),
                                         # IN_DIA_SEMANA_QUINTA = col_double(),
                                         # IN_DIA_SEMANA_SEXTA = col_double(),
                                         # IN_DIA_SEMANA_SABADO = col_double(),
                                         # NU_DIAS_ATIVIDADE = col_double(),
                                         NU_DURACAO_TURMA = 'd',
                                         # TP_TIPO_ATENDIMENTO_TURMA = col_double(),
                                         # TP_TIPO_LOCAL_TURMA = col_double(),
                                         # CO_TIPO_ATIVIDADE_1 = col_double(),
                                         # CO_TIPO_ATIVIDADE_2 = col_double(),
                                         # CO_TIPO_ATIVIDADE_3 = col_double(),
                                         # CO_TIPO_ATIVIDADE_4 = col_double(),
                                         # CO_TIPO_ATIVIDADE_5 = col_logical(),
                                         # CO_TIPO_ATIVIDADE_6 = col_logical(),
                                         TP_ETAPA_ENSINO = 'f',
                                         # CO_CURSO_EDUC_PROFISSIONAL = col_double(),
                                         # IN_ESPECIAL_EXCLUSIVA = col_double(),
                                         # IN_REGULAR = col_double(),
                                         # IN_EJA = col_double(),
                                         # IN_PROFISSIONALIZANTE = col_double(),
                                         QT_MATRICULAS = 'd',
                                         # IN_DISC_LINGUA_PORTUGUESA = col_double(),
                                         # IN_DISC_EDUCACAO_FISICA = col_double(),
                                         # IN_DISC_ARTES = col_double(),
                                         # IN_DISC_LINGUA_INGLES = col_double(),
                                         # IN_DISC_LINGUA_ESPANHOL = col_double(),
                                         # IN_DISC_LINGUA_FRANCES = col_double(),
                                         # IN_DISC_LINGUA_OUTRA = col_double(),
                                         # IN_DISC_LIBRAS = col_double(),
                                         # IN_DISC_LINGUA_INDIGENA = col_double(),
                                         # IN_DISC_PORT_SEGUNDA_LINGUA = col_double(),
                                         # IN_DISC_MATEMATICA = col_double(),
                                         # IN_DISC_CIENCIAS = col_double(),
                                         # IN_DISC_FISICA = col_double(),
                                         # IN_DISC_QUIMICA = col_double(),
                                         # IN_DISC_BIOLOGIA = col_double(),
                                         # IN_DISC_HISTORIA = col_double(),
                                         # IN_DISC_GEOGRAFIA = col_double(),
                                         # IN_DISC_SOCIOLOGIA = col_double(),
                                         # IN_DISC_FILOSOFIA = col_double(),
                                         # IN_DISC_ESTUDOS_SOCIAIS = col_double(),
                                         # IN_DISC_EST_SOCIAIS_SOCIOLOGIA = col_double(),
                                         # IN_DISC_INFORMATICA_COMPUTACAO = col_double(),
                                         # IN_DISC_ENSINO_RELIGIOSO = col_double(),
                                         # IN_DISC_PROFISSIONALIZANTE = col_double(),
                                         # IN_DISC_ESTAGIO_SUPERVISIONADO = col_double(),
                                         # IN_DISC_PEDAGOGICAS = col_logical(),
                                         # IN_DISC_OUTRAS = col_double(),
########################################## DADOS DA ESCOLA
                                         CO_ENTIDADE = 'c',
                                         # CO_REGIAO = col_double(),
                                         # CO_MESORREGIAO = col_double(),
                                         # CO_MICRORREGIAO = col_double(),
                                         CO_UF = 'c',
                                         CO_MUNICIPIO = 'c',
                                         # CO_DISTRITO = col_double(),
                                         # TP_DEPENDENCIA = col_double(),
                                         # TP_LOCALIZACAO = col_double(),
                                         # TP_CATEGORIA_ESCOLA_PRIVADA = col_double(),
                                         # IN_CONVENIADA_PP = col_double(),
                                         # TP_CONVENIO_PODER_PUBLICO = col_double(),
                                         # IN_MANT_ESCOLA_PRIVADA_EMP = col_double(),
                                         # IN_MANT_ESCOLA_PRIVADA_ONG = col_double(),
                                         # IN_MANT_ESCOLA_PRIVADA_OSCIP = col_double(),
                                         # IN_MANT_ESCOLA_PRIV_ONG_OSCIP = col_double(),
                                         # IN_MANT_ESCOLA_PRIVADA_SIND = col_double(),
                                         # IN_MANT_ESCOLA_PRIVADA_SIST_S = col_double(),
                                         # IN_MANT_ESCOLA_PRIVADA_S_FINS = col_double(),
                                         # TP_REGULAMENTACAO = col_double(),
                                         # TP_LOCALIZACAO_DIFERENCIADA = col_double(),
                                         # IN_EDUCACAO_INDIGENA = col_double()
                                       )
) %>% 
  left_join(estados_br, by = c('CO_UF' = 'co_uf')) %>% 
  left_join(municipios_br, by = c('CO_MUNICIPIO' = 'co_municipio_dv'))

# Renomear niveis dos fatores ##################################################
turmas_2020_nordeste <- turmas_2020_nordeste %>%
  mutate(TP_ETAPA_ENSINO = recode(TP_ETAPA_ENSINO,
                                  `1` = 'Educacao Infantil - Creche',
                                  `2` = 'Educacao Infantil - Pre-escola',
                                  `3` = 'Educacao Infantil - Unificada',
                                  `56` = 'Educacao Infantil e Ensino Fundamental (9 anos) Multietapa',
                                  `4` = 'Ensino Fundamental de 8 anos - 1 Serie',
                                  `5` = 'Ensino Fundamental de 8 anos - 2 Serie',
                                  `6` = 'Ensino Fundamental de 8 anos - 3 Serie',
                                  `7` = 'Ensino Fundamental de 8 anos - 4 Serie',
                                  `8` = 'Ensino Fundamental de 8 anos - 5 Serie',
                                  `9` = 'Ensino Fundamental de 8 anos - 6 Serie',
                                  `10` = 'Ensino Fundamental de 8 anos - 7 Serie',
                                  `11` = 'Ensino Fundamental de 8 anos - 8 Serie',
                                  `12` = 'Ensino Fundamental de 8 anos - Multi',
                                  `13` = 'Ensino Fundamental de 8 anos - Correcao de Fluxo',
                                  `14` = 'Ensino Fundamental de 9 anos - 1 Ano',
                                  `15` = 'Ensino Fundamental de 9 anos - 2 Ano',
                                  `16` = 'Ensino Fundamental de 9 anos - 3 Ano',
                                  `17` = 'Ensino Fundamental de 9 anos - 4 Ano',
                                  `18` = 'Ensino Fundamental de 9 anos - 5 Ano',
                                  `19` = 'Ensino Fundamental de 9 anos - 6 Ano',
                                  `20` = 'Ensino Fundamental de 9 anos - 7 Ano',
                                  `21` = 'Ensino Fundamental de 9 anos - 8 Ano',
                                  `41` = 'Ensino Fundamental de 9 anos - 9 Ano',
                                  `22` = 'Ensino Fundamental de 9 anos - Multi',
                                  `23` = 'Ensino Fundamental de 9 anos - Correcao de Fluxo',
                                  `24` = 'Ensino Fundamental de 8 e 9 anos - Multi 8 e 9 anos',
                                  `25` = 'Ensino Medio - 1 ano/1 Serie',
                                  `26` = 'Ensino Medio - 2 ano/2 Serie',
                                  `27` = 'Ensino Medio - 3 ano/3 Serie',
                                  `28` = 'Ensino Medio - 4 ano/4 Serie',
                                  `29` = 'Ensino Medio - Nao Seriada',
                                  `30` = 'Curso Tecnico Integrado (Ensino Medio Integrado) 1 Serie',
                                  `31` = 'Curso Tecnico Integrado (Ensino Medio Integrado) 2 Serie',
                                  `32` = 'Curso Tecnico Integrado (Ensino Medio Integrado) 3 Serie',
                                  `33` = 'Curso Tecnico Integrado (Ensino Medio Integrado) 4 Serie',
                                  `34` = 'Curso Tecnico Integrado (Ensino Medio Integrado) Nao Seriada',
                                  `35` = 'Ensino Medio - Modalidade Normal/Magisterio 1 Serie',
                                  `36` = 'Ensino Medio - Modalidade Normal/Magisterio 2 Serie',
                                  `37` = 'Ensino Medio - Modalidade Normal/Magisterio 3 Serie',
                                  `38` = 'Ensino Medio - Modalidade Normal/Magisterio 4 Serie',
                                  `39` = 'Curso Tecnico - Concomitante',
                                  `40` = 'Curso Tecnico - Subsequente',
                                  `64` = 'Curso Tecnico Misto (Concomitante e Subsequente)',
                                  `65` = 'EJA - Ensino Fundamental - Projovem Urbano',
                                  `67` = 'Curso FIC integrado na modalidade EJA  - Nivel Medio',
                                  `68` = 'Curso FIC Concomitante',
                                  `69` = 'EJA - Ensino Fundamental - Anos Iniciais',
                                  `70` = 'EJA - Ensino Fundamental - Anos Finais',
                                  `71` = 'EJA - Ensino Medio',
                                  `72` = 'EJA - Ensino Fundamental - Anos Iniciais e Anos Finais',
                                  `73` = 'Curso FIC integrado na modalidade EJA - Nivel Fundamental (EJA integrada a Educacao Profissional de Nivel Fundamental)',
                                  `74` = 'Curso Tecnico Integrado na Modalidade EJA (EJA integrada a Educacao Profissional de Nivel Medio)',
                                  ` ` = 'Nao aplicavel para turmas exclusivas de atendimento educacional especializado (AEE) e exclusivas de atividade complementar'
                                  )
  )

# Salvar DB resultante #########################################################
write_rds(turmas_2020_nordeste, here('data/processed/turmas_2020_nordeste.rds'))

