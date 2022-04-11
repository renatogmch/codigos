# Dados de educacao 2020 - Base MATRICULA
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
matriculas_2020_nordeste <- read_delim(here('data/raw/microdados_educacao_basica_2020/DADOS/matricula_nordeste.CSV'),
           '|',
           # n_max = 10000,
           escape_double = FALSE,
           trim_ws = TRUE,
           locale = locale(encoding = "ISO-8859-1",
                           decimal_mark = ','),
           col_types = cols_only(# DADOS DA MATRICULA
                                 # NU_ANO_CENSO = 'f',
                                 ID_ALUNO = 'c',
                                 ID_MATRICULA = 'c',
                                 # NU_MES = col_double(),
                                 # NU_ANO = col_double(),
                                 # NU_IDADE_REFERENCIA = col_double(),
                                 NU_IDADE = 'c',
                                 TP_SEXO = 'f',
                                 TP_COR_RACA = 'f',
                                 # TP_NACIONALIDADE = 'c',
                                 # CO_PAIS_ORIGEM = col_double(),
                                 # CO_UF_NASC = col_double(),
                                 # CO_MUNICIPIO_NASC = col_double(),
                                 # CO_UF_END = 'c',
                                 # CO_MUNICIPIO_END = 'c',
                                 # TP_ZONA_RESIDENCIAL = col_double(),
                                 # TP_LOCAL_RESID_DIFERENCIADA = col_logical(),
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
                                 # IN_RECURSO_LEDOR = col_logical(),
                                 # IN_RECURSO_TRANSCRICAO = col_logical(),
                                 # IN_RECURSO_INTERPRETE = col_logical(),
                                 # IN_RECURSO_LIBRAS = col_logical(),
                                 # IN_RECURSO_LABIAL = col_logical(),
                                 # IN_RECURSO_AMPLIADA_18 = col_logical(),
                                 # IN_RECURSO_AMPLIADA_24 = col_logical(),
                                 # IN_RECURSO_CD_AUDIO = col_logical(),
                                 # IN_RECURSO_PROVA_PORTUGUES = col_logical(),
                                 # IN_RECURSO_VIDEO_LIBRAS = col_logical(),
                                 # IN_RECURSO_BRAILLE = col_logical(),
                                 # IN_RECURSO_NENHUM = col_logical(),
                                 # IN_AEE_LIBRAS = col_logical(),
                                 # IN_AEE_LINGUA_PORTUGUESA = col_logical(),
                                 # IN_AEE_INFORMATICA_ACESSIVEL = col_logical(),
                                 # IN_AEE_BRAILLE = col_logical(),
                                 # IN_AEE_CAA = col_logical(),
                                 # IN_AEE_SOROBAN = col_logical(),
                                 # IN_AEE_VIDA_AUTONOMA = col_logical(),
                                 # IN_AEE_OPTICOS_NAO_OPTICOS = col_logical(),
                                 # IN_AEE_ENRIQ_CURRICULAR = col_logical(),
                                 # IN_AEE_DESEN_COGNITIVO = col_logical(),
                                 # IN_AEE_MOBILIDADE = col_logical(),
                                 # TP_OUTRO_LOCAL_AULA = col_double(),
                                 IN_TRANSPORTE_PUBLICO = 'l',
                                 TP_RESPONSAVEL_TRANSPORTE = 'f',
                                 IN_TRANSP_BICICLETA = 'l',
                                 IN_TRANSP_MICRO_ONIBUS = 'l',
                                 IN_TRANSP_ONIBUS = 'l',
                                 IN_TRANSP_TR_ANIMAL = 'l',
                                 IN_TRANSP_VANS_KOMBI = 'l',
                                 IN_TRANSP_OUTRO_VEICULO = 'l',
                                 IN_TRANSP_EMBAR_ATE5 = 'l',
                                 IN_TRANSP_EMBAR_5A15 = 'l',
                                 IN_TRANSP_EMBAR_15A35 = 'l',
                                 IN_TRANSP_EMBAR_35 = 'l',
                                 TP_ETAPA_ENSINO = 'f',
                                 # IN_ESPECIAL_EXCLUSIVA = col_double(),
                                 IN_REGULAR = 'l',
                                 IN_EJA = 'l',
                                 IN_PROFISSIONALIZANTE = 'l',
################################ DADOS DA TURMA
                                 ID_TURMA = 'c',
                                 # CO_CURSO_EDUC_PROFISSIONAL = col_logical(),
                                 # TP_MEDIACAO_DIDATICO_PEDAGO = col_double(),
                                 NU_DURACAO_TURMA = 'd',
                                 # NU_DUR_ATIV_COMP_MESMA_REDE = col_double(),
                                 # NU_DUR_ATIV_COMP_OUTRAS_REDES = col_double(),
                                 # NU_DUR_AEE_MESMA_REDE = col_double(),
                                 # NU_DUR_AEE_OUTRAS_REDES = col_double(),
                                 # NU_DIAS_ATIVIDADE = col_double(),
                                 # TP_UNIFICADA = col_double(),
                                 TP_TIPO_ATENDIMENTO_TURMA = 'f',
                                 # TP_TIPO_LOCAL_TURMA = 'f',
################################ DADOS DA ESCOLA
                                 CO_ENTIDADE = 'c',
                                 # CO_REGIAO = 'c',
                                 # CO_MESORREGIAO = 'c',
                                 # CO_MICRORREGIAO = 'c',
                                 CO_UF = 'c',
                                 CO_MUNICIPIO = 'c',
                                 # CO_DISTRITO = col_double(),
                                 TP_DEPENDENCIA = 'f',
                                 TP_LOCALIZACAO = 'f',
                                 TP_CATEGORIA_ESCOLA_PRIVADA = 'f',
                                 IN_CONVENIADA_PP = 'f',
                                 TP_CONVENIO_PODER_PUBLICO = 'f',
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


# Renomear niveis dos fatores #################################################
matriculas_2020_nordeste <- matriculas_2020_nordeste %>%
  mutate(TP_SEXO = recode(TP_SEXO,
                          `1`='Masculino',
                          `2`='Feminino'),
         TP_COR_RACA = recode(TP_COR_RACA,
                              `0`='Nao declarada',
                              `1`='Branca',
                              `2`='Preta',
                              `3`='Parda',
                              `4`='Amarela',
                              `5`='Indigena'),
         TP_RESPONSAVEL_TRANSPORTE = recode(TP_RESPONSAVEL_TRANSPORTE,
                                            `1`='Estadual',
                                            `2`='Municipal'),
         TP_ETAPA_ENSINO = recode(TP_ETAPA_ENSINO,
                                  `1`='Educacao Infantil=Creche',
                                  `2`='Educacao Infantil=Pre-escola',
                                  `4`='Ensino Fundamental de 8 anos=1 Serie',
                                  `5`='Ensino Fundamental de 8 anos=2 Serie',
                                  `6`='Ensino Fundamental de 8 anos=3 Serie',
                                  `7`='Ensino Fundamental de 8 anos=4 Serie',
                                  `8`='Ensino Fundamental de 8 anos=5 Serie',
                                  `9`='Ensino Fundamental de 8 anos=6 Serie',
                                  `10`='Ensino Fundamental de 8 anos=7 Serie',
                                  `11`='Ensino Fundamental de 8 anos=8 Serie',
                                  `14`='Ensino Fundamental de 9 anos=1 Ano',
                                  `15`='Ensino Fundamental de 9 anos=2 Ano',
                                  `16`='Ensino Fundamental de 9 anos=3 Ano',
                                  `17`='Ensino Fundamental de 9 anos=4 Ano',
                                  `18`='Ensino Fundamental de 9 anos=5 Ano',
                                  `19`='Ensino Fundamental de 9 anos=6 Ano',
                                  `20`='Ensino Fundamental de 9 anos=7 Ano',
                                  `21`='Ensino Fundamental de 9 anos=8 Ano',
                                  `41`='Ensino Fundamental de 9 anos=9 Ano',
                                  `25`='Ensino Medio=1 ano/1 Serie',
                                  `26`='Ensino Medio=2 ano/2 Serie',
                                  `27`='Ensino Medio=3 ano/3 Serie',
                                  `28`='Ensino Medio=4 ano/4 Serie',
                                  `29`='Ensino Medio=Nao Seriada',
                                  `30`='Curso Tecnico Integrado (Ensino Medio Integrado) 1 Serie',
                                  `31`='Curso Tecnico Integrado (Ensino Medio Integrado) 2 Serie',
                                  `32`='Curso Tecnico Integrado (Ensino Medio Integrado) 3 Serie',
                                  `33`='Curso Tecnico Integrado (Ensino Medio Integrado) 4 Serie',
                                  `34`='Curso Tecnico Integrado (Ensino Medio Integrado) Nao Seriada',
                                  `35`='Ensino Medio=Modalidade Normal/Magisterio 1e Serie',
                                  `36`='Ensino Medio=Modalidade Normal/Magisterio 2e Serie',
                                  `37`='Ensino Medio=Modalidade Normal/Magisterio 3e Serie',
                                  `38`='Ensino Medio=Modalidade Normal/Magisterio 4e Serie',
                                  `39`='Curso Tecnico=Concomitante',
                                  `40`='Curso Tecnico=Subsequente',
                                  `65`='EJA=Ensino Fundamental=Projovem Urbano',
                                  `67`='Curso FIC integrado na modalidade EJA =Nivel Medio',
                                  `68`='Curso FIC Concomitante ',
                                  `69`='EJA=Ensino Fundamental=Anos Iniciais',
                                  `70`='EJA=Ensino Fundamental=Anos Finais',
                                  `71`='EJA=Ensino Medio',
                                  `72`='EJA=Ensino Fundamental =Anos iniciais e Anos finais',
                                  `73`='Curso FIC integrado na modalidade EJA=Nivel Fundamental (EJA integrada a Educacao Profissional de Nivel Fundamental) ',
                                  `74`='Curso Tecnico Integrado na Modalidade EJA (EJA integrada a Educacao Profissional de Nivel Medio)'),
         TP_TIPO_ATENDIMENTO_TURMA = recode(TP_TIPO_ATENDIMENTO_TURMA,
                                            `1`='Exclusivo Escolarizacao',
                                            `2`='Escolarizacao e Atividade complementar',
                                            `3`='Atividade complementar',
                                            `4`='Atendimento Educacional Especializado (AEE)'),
         TP_DEPENDENCIA = recode(TP_DEPENDENCIA,
                                 `1`='Federal',
                                 `2`='Estadual',
                                 `3`='Municipal',
                                 `4`='Privada'),
         TP_LOCALIZACAO = recode(TP_LOCALIZACAO,
                                 `1`='Urbana',
                                 `2`='Rural'),
         TP_CATEGORIA_ESCOLA_PRIVADA = recode(TP_CATEGORIA_ESCOLA_PRIVADA,
                                              `1`='Particular',
                                              `2`='Comunitairia',
                                              `3`='Confessional',
                                              `4`='Filantropica'),
         IN_CONVENIADA_PP = recode(IN_CONVENIADA_PP,
                                   `0`='Nao',
                                   `1`='Sim'),
         TP_CONVENIO_PODER_PUBLICO = recode(TP_CONVENIO_PODER_PUBLICO,
                                            `1`='Municipal',
                                            `2`='Estadual',
                                            `3`='Estadual e Municipal')
  )

# Salvar DB resultante #########################################################
write_rds(matriculas_2020_nordeste, here('data/processed/matriculas_2020_nordeste.rds'))
