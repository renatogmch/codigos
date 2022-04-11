# Curso de R basico
# Renato Gomes Chaves
# 25/03/2020

# criar pasta na area de trabalho

# Instalar pacotes ----
  install.packages('tidyverse')

# Carregar pacotes ----
  library(tidyverse)
  ?tidyverse

# Verificando o diretorio que o R esta direcionado ----
  getwd()

#Direcionado o R para o Diretorio alvo ----
  setwd()

# Alguns exemplos de dados publicos ----

  # Dados de eleicoes do TSE (eleicoes):
  # http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais
  # Pacote R para dados: electionsBR
  
  # Microdados da RAIS-MTE:
  # http://pdet.mte.gov.br/microdados-rais-e-caged
  # 'clique aqui' > RAIS > 2018 > 'RAIS_ESTAB_PIB.7z' (precisa do WinRAR)
  
  
  # Dados do DataSUS:
  # http://datasus.saude.gov.br/transferencia-de-arquivos/
  
  # Dados do INEP (educacao):
  # http://inep.gov.br/web/guest/dados
  
  # Dados do SIDRA-IBGE:
  # https://sidra.ibge.gov.br/home
  # Pacote R para dados: sidrar
  
  # Dados macroeconomicos do IPEADATA:
  # http://www.ipeadata.gov.br/Default.aspx
  
  # Dados macroeconomicos do BACEN (series temporais):
  # https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries
  
  # Dados do FMI:
  # https://www.imf.org/en/Data
  
  # Dados do Banco Mundial:
  # https://www.imf.org/en/Data
  
  # Dados gerais (gratuitos e pagos):
  # https://www.quandl.com/
  # Pacote R para dados: Quandl

# R como calculadora ----
  # Operacoes basicas
  2 + 2
  3 * 5
  8 / 4
  2 ^ 3
  sqrt(9)
  exp(10)
  log(20)
  
  # Operacoes com vetores
  vetor01 <- c(2, 4, 6) #definir vetor. c() eh a funcao de criar vetor
  vetor01
  vetor01/2
  vetor01*3
  vetor02 <- c(3, 5, 7)
  vetor01 + vetor02
  vetor01 * vetor02
  vetor01 %*% vetor02 #multiplicacao matricial
2*3+4*5+6*7 #prova da multiplicacao matricial

# Importacao de dados ----
  # Exemplos de dados nos diferentes formatos
  # dados em .csv
  ?readr::read_delim #ajuda do pacote 'readr' que tem funcoes de leitura de dados
  ?readxl::read_excel #ajuda do pacote de leitura de dados no excel
  
  # Outros tipos de dados
  ?readr::read_rds #arquivos *.rds
  ?haven::read_dta #arquivos *.dta (STATA)
  ?haven::read_sas #arquivos *.sas (SPSS)

# Manipulacao de dados ----
  # Exemplo com dados da RAIS estabelecimentos
  # primeiramente usando a interface do RStudio
  # Primeiro selecionar o arquivo RAIS_ESTAB_PUB
  # O delimitador eh o ponto e virgula, selecionar Delimiter: Semicolon
  # Selecionar o locale para que os acentos sejam interpretados corretamente
  # atraves do Locale: Configuure... > Encoding: ISO-8859-1
  # agora por codigo:
  rais_estab <- read_delim(choose.files(),
                           delim = ';', #delimitador das colunas
                           n_max = 1000, #ler apenas as 'n' primeiras linhas
                           locale = locale(encoding = "ISO-8859-1")
                           )
  # Exemplo: contar quantidade de empresas por cada CNAE 2.0
  # para tal, usaremos as funcoes 'group_by' e 'summarise' do pacote 'dplyr'
  # usaremos tambem o operador pipe
  rais_estab_cnae <- rais_estab %>% #pegue a base de dados e passe para a seguinte funcao
    group_by(`CNAE 2.0 Subclasse`) %>% #agrupe por cada cnae distinto
    # summarise 'agrega' os valores de acordo com uma funcao designada pelo usuario. nesse caso, n()
    summarise(quantidade_empresas = n()) #n() significa conte quantas linhas tem cada cnae unico
  # O codigo acima eh equivalente ao abaixo:
  rais_estab_cnae <- summarise(group_by(rais_estab, `CNAE 2.0 Subclasse`), quantidade_empresas = n())
  # mas o codigo com o pipe eh mais legivel e intuitivo
  
  
  # Exemplo com o PIB Municipal do IBGE
  # Fonte:
  # https://sidra.ibge.gov.br/tabela/5938
  # baixado com as configuracoes:
    # nome da tabela: tabela5938
    # formato: CSV (BR)
    # Exibir: codigos e Nomes dos territorios
  # carregar os dados do PIB por municipio
  pib_municipal <- read_delim(choose.files(),
                              skip = 3, #pular as 3 primeiras linhas ao ler o arquivo
                              n_max = 5570, #nao ler as ultimas linhas onde ha referencias
                              delim = ';' #delimitador das colunas
                              )
  
  # Exemplo: reformular a tabela para que seja PIB por estado:
pib_municipal %>%
    # o comando extract vai extrair a sigla do estado do nome de cada municipio
    separate(col = Município, #coluna alvo
            into = 'Estado', #nome da coluna resultado entre aspas par ser character
            sep = '(\\()', #criterio de extracao, duas letras maiusculas quaisquer juntas, regex significa regular expression
            )


%>%
  # selecionar apenas a coluna estado e os anos 2013 -> 2017
  # obs: o comando select exclui as demais variaveis
  select(Estado,
         `2013`:`2017` #seleciopnar 'de' 2013 'a' 2017
         ) %>%
  # agrupar por estado, similar ao exempli da RAIS:
  group_by(Estado) %>%
  # agora sumarizar, ou 'agregar' por sumatorio dos resultados das linhas por estado
  summarise(total_2013 = sum(`2013`),
            total_2014 = sum(`2014`),
            total_2015 = sum(`2015`),
            total_2016 = sum(`2016`),
            total_2017 = sum(`2017`))
  

