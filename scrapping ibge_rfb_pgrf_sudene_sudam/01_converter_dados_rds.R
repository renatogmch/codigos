# Leitura e conversao (para RDS) de cada arquivo de estabelecimento da RFB
# Filtrados para apenas estados na abrangencia SUDAM/SUDENE
# v0.2
# Renato Gomes Chaves
# Criação: 08/04/2024

# Pacotes ======================================================================
{
  # install.packages('pacman')
  library(pacman)
  # Opcao pra o print ser completo de todas as colunas:
  p_load(tidyverse,
         readxl,
         here,
         foreach,
         xlsx)
  options(dplyr.width = Inf)
}

# Filtro para apenas municipios de atuacao SUDENE e SUDAM ======================
# relacao de municipios com numero ibge e numero siafi
TABMUN <- read_delim("Data/dados_tn/TABMUN.CSV",
                     delim = ";", 
                     col_types = cols(.default = "c"),
                     col_names = c('cod_mun_siafi',
                                   'cnpj_mun',
                                   'nome_mun',
                                   'uf',
                                   'cod_mun_ibge'),
                     escape_double = FALSE,
                     trim_ws = TRUE)

# arquivo contendo os municipios de atuacao da sudene por numero IBGE
sudeneareadeatuacao <- read_excel("Data/dados_sudene/sudeneareadeatuacao.xlsx",
                                  col_types = 'text',
                                  skip = 1,
                                  col_names = c('cod_mun_ibge',
                                                'nome_mun',
                                                'uf'))

# juntando as duas coisas pra ter os nomes dos municipios com ibge e siafi
municipios_sudene <- TABMUN %>%
  filter(cod_mun_ibge %in% sudeneareadeatuacao$cod_mun_ibge) %>% 
  select(cod_mun_ibge,
         cod_mun_siafi,
         nome_mun,
         uf)

# Loop para cada arquivo de EMPRESAS (1 a 9) ===================================
foreach(file_list = list.files(here('Data/dados_rfb_cnpj/'),
                               pattern = "EMPRECSV$"),
        empresas = c('empresas0',
                     'empresas1',
                     'empresas2',
                     'empresas3',
                     'empresas4',
                     'empresas5',
                     'empresas6',
                     'empresas7',
                     'empresas8',
                     'empresas9')
        ) %do% {
          read_delim(paste0('data/dados_rfb_cnpj/', file_list),
                     delim = ";",
                     col_types = cols(.default = "c"),
                     locale = locale(encoding = "ISO-8859-1"), 
                     escape_double = FALSE,
                     trim_ws = TRUE,
                     col_names = c('CNPJ_BASICO',
                                   'RAZAO_SOCIAL',
                                   'NATUREZA_JURIDICA',
                                   'QUALIFICACAO_DO_RESPONSAVEL',
                                   'CAPITAL_SOCIAL',
                                   'PORTE_DA_EMPRESA',
                                   'ENTE_FEDERATIVO')
                     ) %>% 
    select(CNPJ_BASICO,
           RAZAO_SOCIAL,
           CAPITAL_SOCIAL) %>% 
    write_rds(., file = here(paste0('data/processed/', empresas, '.rds')))
        }


# Loop para cada arquivo de ESTABELECIMENTOS (de 0 a 9) ========================
foreach(file_list = list.files(here('Data/dados_rfb_cnpj/'),
                               pattern = "ESTABELE$"),
        estabelecimentos = c('estabelecimentos0',
                             'estabelecimentos1',
                             'estabelecimentos2',
                             'estabelecimentos3',
                             'estabelecimentos4',
                             'estabelecimentos5',
                             'estabelecimentos6',
                             'estabelecimentos7',
                             'estabelecimentos8',
                             'estabelecimentos9')
) %do% {
  read_delim(paste0('Data/dados_rfb_cnpj/', file_list),
             delim = ";",
             # n_max = 1000,
             col_types = cols(.default = "c"),
             locale = locale(encoding = "ISO-8859-1"), 
             escape_double = FALSE,
             trim_ws = TRUE,
             col_names = c('CNPJ_BASICO',
                           'CNPJ_ORDEM',
                           'CNPJ_DV',
                           'IDENTIFICADOR_MATRIZ_FILIAL',
                           'NOME_FANTASIA',
                           'SITUACAO_CADASTRAL',
                           'DATA_SITUACAO_CADASTRAL',
                           'MOTIVO_SITUACAO_CADASTRAL',
                           'NOME_DA_CIDADE_NO_EXTERIOR',
                           'PAIS',
                           'DATA_DE_INICIO_ATIVIDADE',
                           'CNAE_FISCAL_PRINCIPAL',
                           'CNAE_FISCAL_SECUNDARIA',
                           'TIPO_DE_LOGRADOURO',
                           'LOGRADOURO',
                           'NUMERO',
                           'COMPLEMENTO',
                           'BAIRRO',
                           'CEP',
                           'UF',
                           'MUNICIPIO',
                           'DDD_1',
                           'TELEFONE_1',
                           'DDD_2',
                           'TELEFONE_2',
                           'DDD_DO_FAX',
                           'FAX',
                           'CORREIO_ELETRONICO',
                           'SITUACAO_ESPECIAL',
                           'DATA_DA_SITUACAO_ESPECIAL')
  ) %>% 
    # filtrar apenas municipios de abrangencia SUDENE e SUDAM e diminuir o
    # tamanho do arquivo
    filter(MUNICIPIO %in% municipios_sudene$cod_mun_siafi
           | UF %in% c('AC', 'AP', 'AM', 'MT', 'PA', 'RO', 'RR', 'TO')) %>%
    # juntar apenas uma coluna de CNPJ
    unite('CNPJ', 1:3, sep = '', remove = T) %>% 
    # nomes dos municipios pelo codigo IBGE
    left_join(TABMUN, join_by(MUNICIPIO == cod_mun_siafi)) %>% 
    # selecionar apenas as colunas uteis pra diminuir tamanho de arquivo
    select(CNPJ,
           NOME_FANTASIA,
           SITUACAO_CADASTRAL,
           DATA_SITUACAO_CADASTRAL,
           DATA_DE_INICIO_ATIVIDADE,
           CNAE_FISCAL_PRINCIPAL,
           CNAE_FISCAL_SECUNDARIA,
           TIPO_DE_LOGRADOURO,
           LOGRADOURO,
           NUMERO,
           COMPLEMENTO,
           BAIRRO,
           CEP,
           UF,
           MUNICIPIO = nome_mun,
           DDD_1,
           TELEFONE_1,
           DDD_2,
           TELEFONE_2,
           CORREIO_ELETRONICO) %>%
    # trocar por nomes das situacoes cadastrais
    mutate(SITUACAO_CADASTRAL = case_when(SITUACAO_CADASTRAL == '01' ~ 'NULA',
                                          SITUACAO_CADASTRAL == '02' ~ 'ATIVA',
                                          SITUACAO_CADASTRAL == '03' ~ 'SUSPENSA',
                                          SITUACAO_CADASTRAL == '04' ~ 'INAPTA',
                                          SITUACAO_CADASTRAL == '08' ~ 'BAIXADA'),
           COMPLEMENTO = str_remove_all(COMPLEMENTO, '[\\;]')) %>%
    write_rds(., file = here(paste0('data/processed/',
                                    estabelecimentos, '.rds')))
}
