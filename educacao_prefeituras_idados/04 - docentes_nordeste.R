# Dados de educacao 2020 - Base ESCOLAS
# modelo da consultoria iDados
# Renato Gomes Chaves
# Criacao 19/11/2021
# Ultima modificacao: 23/11/2021

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

# Carregar apenas as colunas selecionadas ######################################
docentes_2020_nordeste <- read_delim(here('data/raw/microdados_educacao_basica_2020/DADOS/docentes_nordeste.CSV'),
                                     '|',
                                     # n_max = 1000,
                                     # escape_double = FALSE,
                                     trim_ws = TRUE,
                                     locale = locale(encoding = "ISO-8859-1",
                                                     decimal_mark = ','),
                                     col_types = cols_only(# DADOS DO PROFISSIONAL ESCOLAR EM SALA DE AULA
                                                           # NU_ANO_CENSO = col_double(),
                                                           ID_DOCENTE = 'c',
                                                           # NU_MES = col_double(),
                                                           NU_ANO = 'i',
                                                           # NU_IDADE_REFERENCIA = col_double(),
                                                           # NU_IDADE = col_double(),
                                                           # TP_SEXO = col_double(),
                                                           # TP_COR_RACA = col_double(),
                                                           # TP_NACIONALIDADE = col_double(),
                                                           # CO_PAIS_ORIGEM = col_double(),
                                                           # CO_UF_NASC = col_double(),
                                                           # CO_MUNICIPIO_NASC = col_double(),
                                                           # CO_UF_END = col_double(),
                                                           # CO_MUNICIPIO_END = col_double(),
                                                           # TP_ZONA_RESIDENCIAL = col_double(),
                                                           # TP_LOCAL_RESID_DIFERENCIADA = col_double(),
                                                           # IN_NECESSIDADE_ESPECIAL = col_double(),
                                                           # IN_BAIXA_VISAO = col_logical(),
                                                           # IN_CEGUEIRA = col_logical(),
                                                           # IN_DEF_AUDITIVA = col_logical(),
                                                           # IN_DEF_FISICA = col_logical(),
                                                           # IN_DEF_INTELECTUAL = col_logical(),
                                                           # IN_SURDEZ = col_logical(),
                                                           # IN_SURDOCEGUEIRA = col_logical(),
                                                           # IN_DEF_MULTIPLA = col_logical(),
                                                           # IN_AUTISMO = col_logical(),
                                                           # IN_SUPERDOTACAO = col_logical(),
                                                           # TP_ESCOLARIDADE = col_double(),
                                                           # TP_ENSINO_MEDIO = col_double(),
                                                           # TP_SITUACAO_CURSO_1 = col_double(),
                                                           # CO_AREA_CURSO_1 = col_double(),
                                                           # CO_CURSO_1 = col_character(),
                                                           # IN_LICENCIATURA_1 = col_double(),
                                                           # NU_ANO_CONCLUSAO_1 = col_double(),
                                                           # TP_TIPO_IES_1 = col_double(),
                                                           # CO_IES_1 = col_double(),
                                                           # TP_SITUACAO_CURSO_2 = col_double(),
                                                           # CO_AREA_CURSO_2 = col_double(),
                                                           # CO_CURSO_2 = col_character(),
                                                           # IN_LICENCIATURA_2 = col_double(),
                                                           # NU_ANO_CONCLUSAO_2 = col_double(),
                                                           # TP_TIPO_IES_2 = col_double(),
                                                           # CO_IES_2 = col_double(),
                                                           # TP_SITUACAO_CURSO_3 = col_logical(),
                                                           # CO_AREA_CURSO_3 = col_logical(),
                                                           # CO_CURSO_3 = col_logical(),
                                                           # IN_LICENCIATURA_3 = col_double(),
                                                           # NU_ANO_CONCLUSAO_3 = col_logical(),
                                                           # TP_TIPO_IES_3 = col_logical(),
                                                           # CO_IES_3 = col_logical(),
                                                           # IN_COMPLEMENTACAO_PEDAGOGICA = col_double(),
                                                           # CO_AREA_COMPL_PEDAGOGICA_1 = col_double(),
                                                           # CO_AREA_COMPL_PEDAGOGICA_2 = col_double(),
                                                           # CO_AREA_COMPL_PEDAGOGICA_3 = col_logical(),
                                                           # IN_ESPECIALIZACAO = col_double(),
                                                           # IN_MESTRADO = col_double(),
                                                           # IN_DOUTORADO = col_double(),
                                                           # IN_POS_NENHUM = col_double(),
                                                           # IN_ESPECIFICO_CRECHE = col_double(),
                                                           # IN_ESPECIFICO_PRE_ESCOLA = col_double(),
                                                           # IN_ESPECIFICO_ANOS_INICIAIS = col_double(),
                                                           # IN_ESPECIFICO_ANOS_FINAIS = col_double(),
                                                           # IN_ESPECIFICO_ENS_MEDIO = col_double(),
                                                           # IN_ESPECIFICO_EJA = col_double(),
                                                           # IN_ESPECIFICO_ED_ESPECIAL = col_double(),
                                                           # IN_ESPECIFICO_ED_INDIGENA = col_double(),
                                                           # IN_ESPECIFICO_CAMPO = col_double(),
                                                           # IN_ESPECIFICO_AMBIENTAL = col_double(),
                                                           # IN_ESPECIFICO_DIR_HUMANOS = col_double(),
                                                           # IN_ESPECIFICO_DIV_SEXUAL = col_double(),
                                                           # IN_ESPECIFICO_DIR_ADOLESC = col_double(),
                                                           # IN_ESPECIFICO_AFRO = col_double(),
                                                           # IN_ESPECIFICO_GESTAO = col_double(),
                                                           # IN_ESPECIFICO_OUTROS = col_double(),
                                                           # IN_ESPECIFICO_NENHUM = col_double(),
############################################################ DADOS DA TURMA
                                                           ID_TURMA = 'c',
                                                           TP_TIPO_DOCENTE = 'f',
                                                           TP_TIPO_CONTRATACAO = 'f',
############################################################ Disciplinas - Linguagens
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
                                                           # Disciplinas - Matematica
                                                           # IN_DISC_MATEMATICA = col_double(),
############################################################ Disciplinas - Ciencias da Natureza
                                                           # IN_DISC_CIENCIAS = col_double(),
                                                           # IN_DISC_FISICA = col_double(),
                                                           # IN_DISC_QUIMICA = col_double(),
                                                           # IN_DISC_BIOLOGIA = col_double(),
############################################################ Disciplinas - Ciencias Huamanas e Sociais
                                                           # IN_DISC_HISTORIA = col_double(),
                                                           # IN_DISC_GEOGRAFIA = col_double(),
                                                           # IN_DISC_SOCIOLOGIA = col_double(),
                                                           # IN_DISC_FILOSOFIA = col_double(),
                                                           # IN_DISC_ESTUDOS_SOCIAIS = col_double(),
                                                           # IN_DISC_EST_SOCIAIS_SOCIOLOGIA = col_double(),
############################################################ Disciplinas - Outras Areas
                                                           # IN_DISC_INFORMATICA_COMPUTACAO = col_double(),
                                                           # IN_DISC_ENSINO_RELIGIOSO = col_double(),
                                                           # IN_DISC_PROFISSIONALIZANTE = col_double(),
                                                           # IN_DISC_ESTAGIO_SUPERVISIONADO = col_double(),
                                                           # IN_DISC_PEDAGOGICAS = col_logical(),
                                                           # IN_DISC_OUTRAS = col_double(),
                                                           # TP_TIPO_ATENDIMENTO_TURMA = col_double(),
                                                           # TP_TIPO_LOCAL_TURMA = col_double(),
                                                           # TP_MEDIACAO_DIDATICO_PEDAGO = col_double(),
                                                           # TP_ETAPA_ENSINO = col_double(),
                                                           # CO_CURSO_EDUC_PROFISSIONAL = col_double(),
                                                           # IN_ESPECIAL_EXCLUSIVA = col_double(),
                                                           # IN_REGULAR = col_double(),
                                                           # IN_EJA = col_double(),
                                                           # IN_PROFISSIONALIZANTE = col_double(),
############################################################ DADOS DAS ESCOLAS
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
)

# Renomear niveis dos fatores ##################################################
docentes_2020_nordeste <- docentes_2020_nordeste %>% 
  # um dos fatores é um string vazio
  # precisa trocar por algum caractere pra facilitar
  # nesse caso troquei por NA
  mutate(TP_TIPO_CONTRATACAO = na_if(TP_TIPO_CONTRATACAO, '')) %>% 
  # trocar NAs por '(Missing)'
  mutate(across(where(is.factor), fct_explicit_na)) %>%
  mutate(across(where(is.factor), fct_drop)) %>% 
  # agora o recode de fato
  mutate(TP_TIPO_DOCENTE = recode(TP_TIPO_DOCENTE,
                                  `1` = 'Docente',
                                  `2` = 'Auxiliar/Assistente Educacional',
                                  `3` = 'Profissional/Monitor de atividade complementar',
                                  `4` = 'Tradutor Intérprete de Libras',
                                  `5` = 'Docente Titular - coordenador de tutoria (de modulo ou disciplina) - EAD',
                                  `6` = 'Docente Tutor - Auxiliar (de modulo ou disciplina) - EAD',
                                  `7` = 'Guia interprete',
                                  `8` = 'Profissional de apoio escolar para alunos com deficiencia (Lei 13.146/2015)'
                                  ),
         TP_TIPO_CONTRATACAO = recode(TP_TIPO_CONTRATACAO,
                                      `1` = 'Concursado/efetivo/estavel',
                                      `2` = 'Contrato temporario',
                                      `3` = 'Contrato terceirizado',
                                      `4` = 'Contrato CLT',
                                      # NAs foram colocados nesse nivel
                                      `(Missing)` = 'Nao aplicável para nao docentes e escolas privadas'
                                      )
         ) %>% 
  left_join(estados_br, by = c('CO_UF' = 'co_uf')) %>% 
  left_join(municipios_br, by = c('CO_MUNICIPIO' = 'co_municipio_dv'))

# Salvar DB resultante #########################################################
write_rds(docentes_2020_nordeste, here('data/processed/docentes_2020_nordeste.rds'))