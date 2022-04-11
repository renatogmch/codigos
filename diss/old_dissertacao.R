# Script Dissertacao ----
# Data: 17/10/18
# Autor: Renato Gomes Chaves
# Versao: 1.0
# Alguns Tutoriais:
# Exploring Panel data: https://www.princeton.edu/~otorres/Panel101R.pdf
# https://cran.r-project.org/web/packages/ExPanDaR/vignettes/use_ExPanD.html
# Pacotes n utilizados "REAT" "MESS" "lmtest"

# PACOTES -----------------------------------------------------------------

{
  packages = c("tidyverse","here", "rgdal", "geosphere", "stats", "readr", "reshape2", "plm", "normtest", "stargazer", "xtable", "REAT","lmtest","doParallel","foreach")
  package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  no_cores <- detectCores() - 1
  registerDoParallel(cores=no_cores)
  rm(package.check, packages, no_cores)
  gc()
}
{
# PREPARACAO DOS DADOS -----------------------------------------------------------------
# Carregar os bancos de dados
# Converter os arquivos .csv em .rds
# {
#   print(2006)
#   saveRDS(read_delim("~/Dados dissertacao/vinc.brasil2006.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE), "FIRM2006.rds")
#   print(2008)
#   saveRDS(read_delim("~/Dados dissertacao/vinc.brasil2008.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE), "FIRM2006.rds")
#   print(2009)
#   saveRDS(read_delim("~/Dados dissertacao/vinc.brasil2009.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE), "FIRM2006.rds")
#   print(2010)
#   saveRDS(read_delim("~/Dados dissertacao/vinc.brasil2010.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE), "FIRM2006.rds")
#   print(2011)
#   saveRDS(read_delim("~/Dados dissertacao/vinc.brasil2011.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE), "FIRM2006.rds")
#   print(2012)
#   saveRDS(read_delim("~/Dados dissertacao/vinc.brasil2012.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE), "FIRM2006.rds")
#   print(2013)
#   saveRDS(read_delim("~/Dados dissertacao/vinc.brasil2013.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE), "FIRM2006.rds")
#   print(2014)
#   saveRDS(read_delim("~/Dados dissertacao/vinc.brasil2014.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE), "FIRM2006.rds")
#   print(2016)
#   saveRDS(read_delim("~/Dados dissertacao/vinc.brasil2016.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE), "FIRM2006.rds")
#   #FIRM2017 <- read_delim("~/Dados dissertacao/vinc.brasil2017.csv", ",", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
#   gc()
#  teste <- Reduce(function(x, y) merge(x, y, by=c(1)), list(FIRM2006, FIRM2008, FIRM2010, FIRM2012, FIRM2014, FIRM2016))
# }
# 
#   TRAB2006 <- read_delim("D:/Users/Renato/Desktop/Anos/TRAB2006.csv", 
#                           ";", escape_double = FALSE, col_types = cols(cpf = col_double(), 
#                                                                        identificad = col_double(), 
#                                                                        remmedia = col_character(), remmedr = col_character(), 
#                                                                        salcontr = col_character()), trim_ws = TRUE)[,c('identificad','municipio','cpf','remmedr','grauinstr')]
#   TRAB2008 <- read_delim("D:/Users/Renato/Desktop/Anos/TRAB2008.csv", 
#                          ";", escape_double = FALSE, col_types = cols(cpf = col_double(), 
#                                                                       identificad = col_double(), 
#                                                                       remmedia = col_character(), remmedr = col_character(), 
#                                                                       salcontr = col_character()), trim_ws = TRUE)[,c('identificad','municipio','cpf','remmedr','grinstrucao')]
#   TRAB2010 <- read_delim("D:/Users/Renato/Desktop/Anos/TRAB2010.csv", 
#                          ";", escape_double = FALSE, col_types = cols(cpf = col_double(), 
#                                                                       identificad = col_double(), 
#                                                                       remmedia = col_character(), remmedr = col_character(), 
#                                                                       salcontr = col_character()), trim_ws = TRUE)[,c('identificad','municipio','cpf','remmedr','grinstrucao')]
#   TRAB2006$remmedr <- as.numeric(sub(",", ".", TRAB2006$remmedr, fixed = TRUE))
#   TRAB2008$remmedr <- as.numeric(sub(",", ".", TRAB2008$remmedr, fixed = TRUE))
#   TRAB2010$remmedr <- as.numeric(sub(",", ".", TRAB2010$remmedr, fixed = TRUE))
#   
#   TRAB2012 <- read_csv("D:/Users/Renato/Desktop/Anos/TRAB2012.csv", 
#                        col_types = cols(vlremunmdiasm = col_character()), 
#                        locale = locale(encoding = "ISO-8859-1"))[,c('cnpjcei','municpio','cpf','vlremunmdianom','escolaridadeaps2005')]
#   TRAB2012$vlremunmdianom <- as.numeric(sub(",", ".", TRAB2012$vlremunmdianom, fixed = TRUE))
#   
#   TRAB2014 <- read_csv("D:/Users/Renato/Desktop/Anos/TRAB2014.csv", 
#                        col_types = cols(vlremunmdiasm = col_character()), 
#                        locale = locale(encoding = "ISO-8859-1"))[,c('cnpjcei','municpio','cpf','vlremunmdianom','escolaridadeaps2005')]
#   TRAB2014$vlremunmdianom <- as.numeric(sub(",", ".", TRAB2014$vlremunmdianom, fixed = TRUE))
#   
# TRAB2017 <- foreach(i = list.files(pattern = "[A-Z][A-Z]2017[.]csv$"), .packages='tidyverse') %dopar% read_csv(paste0('D:/Users/Renato/Desktop/Estados/',i), locale = locale(encoding = "ISO-8859-1"))[,c('cnpjcei','municpio','cpf','vlremunmdianom','escolaridadeaps2005')]
# TRAB2017 <- as_tibble(rbindlist(TRAB2017))
# saveRDS(TRAB2017, file='TRAB2016.rds')
  
{
  print("MUNICIPIOS")
  MUNICIPIOS <- readxl::read_excel(here("regioes_geograficas_composicao_por_municipios_2017.xlsx"))
  for(i in 2006:2016){
    if (i %% 2){
      next
    }
    print(paste("Carregando FIRM",i))
    assign(x = paste("FIRM", i, sep = ""),
           value = readRDS(file.path("FIRM", i, ".rds", fsep = "")))
  }
  rm(i)
  DADOS_TRAB <- foreach(i = list.files(pattern = "TRAB20[0-1][0-9][.]rds$"), n = list('2006','2008','2010','2012','2014','2016'),.packages='tidyverse') %dopar% setNames(as_tibble(readRDS(as.character(i))),c('cnpj','cod_mun','cpf',paste0('rem_med_',n),paste0('escolr_',n)))
  names(DADOS_TRAB) <- c('2006','2008','2010','2012','2014','2016')
  DADOS_TRAB[['2016']]$rem_med_2016 <- as.numeric(sub(",", ".", DADOS_TRAB[['2016']]$rem_med_2016, fixed = TRUE))
  gc()
}

  # VINCULOS <- foreach(i = DADOS_TRAB,n = list('2006','2008','2010','2012','2014','2016'), .packages='tidyverse') %dopar% setNames(aggregate(.~cnpj,i[,c(1,3)], length),c('cnpj',n))
  # VINCULOS[['TOTAL']] <- as_tibble(Reduce(function(x,y) merge(x,y, by='cnpj'), VINCULOS))
  
  
  # teste <- big_fread2(choose.files(), sep=";") c('cnpjcei','municpio','cpf','escolaridadeaps2005','vlremunmdianom')
  # https://rdrr.io/cran/bigreadr/man/big_fread2.html

# Peraracao individual ====
{
  # for(i in 2006:2016){
  #   if (i %% 2){
  #     next
  #   }
  #   print(paste("Preparando FIRM",i))
  #   assign(x = paste("FIRM", i, sep = ""),
  #          names(x)[1:3] <- c('cnpj','cod_mun',paste0('vinc.',i)))
  #          X[!duplicated(x$cnpj),]
  # }

  
  #2006
  print('Preparando 2006')
  FIRM2006 <- FIRM2006[!duplicated(FIRM2006$identificad),]
  names(FIRM2006)[1:3]=c('cnpj','cod_mun','vinc.2006')
  #FIRM2006.ind.alim <- subset(FIRM2006, FIRM2006$sbclas20 >=1000000 & FIRM2006$sbclas20 < 1100000)
  #FIRM2006.ind.alim <- FIRM2006.ind.alim[!duplicated(FIRM2006.ind.alim$identificad),]
  #colnames(FIRM2006.ind.alim)[colnames(FIRM2006.ind.alim)=="identificad"] <- "cnpj"
  #colnames(FIRM2006.ind.alim)[colnames(FIRM2006.ind.alim)=="estoque"] <- "vinc.2006"
  # teste01 <- teste[,c('cnpjcei','municpio','cpf','vlremunmdianom','escolaridadeaps2005')]

  
  #2008
  print('Preparando 2008')
  FIRM2008 <- FIRM2008[!duplicated(FIRM2008$identificad),]
  names(FIRM2008)[1:3]=c('cnpj','cod_mun','vinc.2008')

  #2009
  # print('Preparando 2009')
  # FIRM2009 <- FIRM2009[!duplicated(FIRM2009$identificad),]
  # colnames(FIRM2009)[colnames(FIRM2009)=="identificad"] <- "cnpj"
  # colnames(FIRM2009)[colnames(FIRM2009)=="municipio"] <- "cod_mun"
  # colnames(FIRM2009)[colnames(FIRM2009)=="estoque"] <- "vinc.2009"
  
  #2010
  print('Preparando 2010')
  FIRM2010 <- FIRM2010[!duplicated(FIRM2010$identificad),]
  names(FIRM2010)[1:3]=c('cnpj','cod_mun','vinc.2010')
  
  #2011
  # print('Preparando 2011')
  # FIRM2011 <- FIRM2011[!duplicated(FIRM2011$cnpjcei),]
  # colnames(FIRM2011)[colnames(FIRM2011)=="cnpjcei"] <- "cnpj"
  # colnames(FIRM2011)[colnames(FIRM2011)=="municpio"] <- "cod_mun"
  # colnames(FIRM2011)[colnames(FIRM2011)=="qtdvnculosativos"] <- "vinc.2011"
  
  #2012
  print('Preparando 2012')
  FIRM2012 <- FIRM2012[!duplicated(FIRM2012$cnpjcei),]
  names(FIRM2012)[1:3]=c('cnpj','cod_mun','vinc.2012')

  
  #2013
  # print('Preparando 2013')
  # FIRM2013 <- FIRM2013[!duplicated(FIRM2013$cnpjcei),]
  # colnames(FIRM2013)[colnames(FIRM2013)=="cnpjcei"] <- "cnpj"
  # colnames(FIRM2013)[colnames(FIRM2013)=="municpio"] <- "cod_mun"
  # colnames(FIRM2013)[colnames(FIRM2013)=="qtdvnculosativos"] <- "vinc.2013"
  
  #2014
  print('Preparando 2014')
  FIRM2014 <- FIRM2014[!duplicated(FIRM2014$cnpjcei),]
  names(FIRM2014)[1:3]=c('cnpj','cod_mun','vinc.2014')

  
  #2016
  print('Preparando 2016')
  FIRM2016 <- FIRM2016[!duplicated(FIRM2016$cnpjcei),]
  names(FIRM2016)[1:3]=c('cnpj','cod_mun','vinc.2016')

  #FIRM2016 <- merge(FIRM2016, MUNICIPIOS, by="cod_mun")
  #FIRM2016$address <- paste(paste(FIRM2016$nmerologradouro, FIRM2016$nomelogradouro, sep = " "), FIRM2016$nome_mun, FIRM2016$nome_uf, "Brasil.", sep = ", ")
  
  gc()
}



# 
# # Criacao do DB unico com todos os anos apenas para a industria de alimentos ####
# {
#   tic('Merging dos DBs anuais')
#   print('Preparando as firmas...')
#   # FIRMAS <- merge(merge(merge(merge(merge(merge(merge(merge(merge(subset(FIRM2006, FIRM2006$sbclas20 >=1000000 & FIRM2006$sbclas20 < 1100000)[,c(1:3)],
#   #                                                                 FIRM2008[,c(1:3)], by = c("cnpj", "cod_mun")),
#   #                                                                 FIRM2009[,c(1:3)], by = c("cnpj", "cod_mun")),
#   #                                                                 FIRM2010[,c(1:3)], by = c("cnpj", "cod_mun")),
#   #                                                                 FIRM2011[,c(1:3)], by = c("cnpj", "cod_mun")),
#   #                                                                 FIRM2012[,c(1:3)], by = c("cnpj", "cod_mun")),
#   #                                                                 FIRM2013[,c(1:3)], by = c("cnpj", "cod_mun")),
#   #                                                                 FIRM2014[,c(1:3)], by = c("cnpj", "cod_mun")),
#   #                                                                 FIRM2016[,c(1:3)], by = c("cnpj", "cod_mun")),
#   #                                                                 MUNICIPIOS, by="cod_mun")
#   
#   FIRMAS <- merge(merge(merge(merge(merge(merge(subset(FIRM2006, FIRM2006$sbclas20 >=1000000 & FIRM2006$sbclas20 < 1100000)[,c(1:3)],
#                   FIRM2008[,c(1:3)], by = c("cnpj", "cod_mun")),
#                   FIRM2010[,c(1:3)], by = c("cnpj", "cod_mun")),
#                   FIRM2012[,c(1:3)], by = c("cnpj", "cod_mun")),
#                   FIRM2014[,c(1:3)], by = c("cnpj", "cod_mun")),
#                   FIRM2016[,c(1:3)], by = c("cnpj", "cod_mun")),
#                   MUNICIPIOS, by="cod_mun")
#   toc()
#   gc()
# saveRDS(FIRMAS, file="FIRMAS.rds")
# }
FIRMAS <- as_tibble(readRDS(file = here("FIRMAS.rds")))
# FIRMAS_PAINEL <- readRDS(file = here("FIRMAS_PAINEL.rds"))
# FIRMAS_ALL <- Reduce(function(x, y) merge(x, y, by=c('cod_mun','cnpj'), all=TRUE), list(FIRM2006,FIRM2008,FIRM2010,FIRM2012,FIRM2014,FIRM2016))
  
gc()
}

# Variavel FGROWTH ====
{
  # ln(vinc)t+1 - ln(vinc)t
  print('Calculando FGROWTH.')
  FGROWTH <- FIRMAS[,c(1:2)]
  FGROWTH$"2006" <- log(FIRMAS$vinc.2008 +0.00001)-log(FIRMAS$vinc.2006 +0.00001)
  FGROWTH$"2008" <- log(FIRMAS$vinc.2010 +0.00001)-log(FIRMAS$vinc.2008 +0.00001)
  # FGROWTH$"2009" <- log(FIRMAS$vinc.2010 +0.1)-log(FIRMAS$vinc.2009 +0.1)
  FGROWTH$"2010" <- log(FIRMAS$vinc.2012 +0.00001)-log(FIRMAS$vinc.2010 +0.00001)
  # FGROWTH$"2011" <- log(FIRMAS$vinc.2012 +0.1)-log(FIRMAS$vinc.2011 +0.1)
  FGROWTH$"2012" <- log(FIRMAS$vinc.2014 +0.00001)-log(FIRMAS$vinc.2012 +0.00001)
  # FGROWTH$"2013" <- log(FIRMAS$vinc.2014 +0.1)-log(FIRMAS$vinc.2013 +0.1)
  FGROWTH$"2014" <- log(FIRMAS$vinc.2016 +0.00001)-log(FIRMAS$vinc.2014 +0.00001)
  # Linha abaixo faz subset apenas par PE.
  # FGROWTH = subset(FGROWTH, FGROWTH$cod_mun>=260000 & FGROWTH$cod_mun<270000)
  gc()
}
# Variavel SIZE ====
{
  # ln(vinc)t
  print('Calculando SIZE.')
  SIZE <- FIRMAS[,c(1,2)]
  SIZE$"2006" <- log(FIRMAS$vinc.2006 +0.00001)
  SIZE$"2008" <- log(FIRMAS$vinc.2008 +0.00001)
  # SIZE$"2009" <- log(FIRMAS$vinc.2009 +0.1)
  SIZE$"2010" <- log(FIRMAS$vinc.2010 +0.00001)
  # SIZE$"2011" <- log(FIRMAS$vinc.2011 +0.1)
  SIZE$"2012" <- log(FIRMAS$vinc.2012 +0.00001)
  # SIZE$"2013" <- log(FIRMAS$vinc.2013 +0.1)
  SIZE$"2014" <- log(FIRMAS$vinc.2014 +0.00001)
  # Linha abaixo faz subset apenas par PE.
  # SIZE = subset(SIZE, SIZE$cod_mun>=260000 & SIZE$cod_mun<270000)
  gc()
}

# Variavel ESCOLARIDADE ====
{
  ESCOLARIDADE <- foreach(i = DADOS_TRAB,
                          n = list('2006','2008','2010','2012','2014','2016'),
                          .packages = 'tidyverse') %dopar% {
                            i %>%
                              group_by(cnpj) %>%
                              summarise(
                                vinc_superior = length(cpf[get(paste0('escolr_',n))>7]),
                                vinc_total = length(cpf),
                                vinc_porcentagem = vinc_superior/vinc_total
                              ) %>%
                              setNames(c('cnpj','vinc_superior','vinc_total',as.character(n))) %>%
                              select(1,4)
                          }
  names(ESCOLARIDADE) <- c('2006','2008','2010','2012','2014','2016')
  ESCOLARIDADE[['VAR']] <- as_tibble(merge(FIRMAS[,c(2)], Reduce(function(x,y) merge(x,y, by='cnpj', all=T), ESCOLARIDADE[1:6]),  by='cnpj', all.x=T))
  gc()
}



# Variavel REMUNERACAO ====
# Media por firma da media anual por trabalhador.
{
  REMUNERACAO <- foreach(i = DADOS_TRAB,
                         .packages=c('tidyverse','stats')) %dopar% 
    as_tibble(aggregate(. ~ cnpj ,i[,c(1,4)], FUN = mean))
  
  names(REMUNERACAO) <- c('2006','2008','2010','2012','2014','2016')
  REMUNERACAO[['TOTAL']] <- as_tibble(Reduce(function(x,y) merge(x,y, by='cnpj', all=T), REMUNERACAO))
  REMUNERACAO[['VAR']] <- as_tibble(merge(FIRMAS[,c(2)], REMUNERACAO[['TOTAL']], by='cnpj', all.x=T))[,-c(7)] %>%
  setNames(c('cnpj','2006','2008','2010','2012','2014'))
  gc()
}


# Variavel LOCQ ====
{
# E= Emprego
# F= Firmas
# i= Industria
# j= Municipio
# t= Ano

# Metodologia (por MUN):
# (e_ijt)Emprego/Firmas da industria i, no municipio j, no ano t;
# (e_jt)Emprego/Firmas total no municipio j no ano t;
# (e_it)Emprego/Firmas total na industria i no ano t;
# (e_t)Emprego/Firmas total no ano t;

# e_ijt
e_ij_2006 <- aggregate(vinc.2006~cod_mun,subset(FIRM2006, FIRM2006$sbclas20 >=1000000 & FIRM2006$sbclas20 < 1100000)[,c('cod_mun','vinc.2006')], sum)
e_ij_2008 <- aggregate(vinc.2008~cod_mun,subset(FIRM2008, FIRM2008$sbclas20 >=1000000 & FIRM2008$sbclas20 < 1100000)[,c('cod_mun','vinc.2008')], sum)
e_ij_2010 <- aggregate(vinc.2010~cod_mun,subset(FIRM2010, FIRM2010$sbclas20 >=1000000 & FIRM2010$sbclas20 < 1100000)[,c('cod_mun','vinc.2010')], sum)
e_ij_2012 <- aggregate(vinc.2012~cod_mun,subset(FIRM2012, FIRM2012$cnae20subclasse >=1000000 & FIRM2012$cnae20subclasse < 1100000)[,c('cod_mun','vinc.2012')], sum)
e_ij_2014 <- aggregate(vinc.2014~cod_mun,subset(FIRM2014, FIRM2014$cnae20subclasse >=1000000 & FIRM2014$cnae20subclasse < 1100000)[,c('cod_mun','vinc.2014')], sum)

# e_jt
e_j_2006 <- aggregate(vinc.2006~cod_mun,FIRM2006[,c('cod_mun','vinc.2006')], sum)
e_j_2008 <- aggregate(vinc.2008~cod_mun,FIRM2008[,c('cod_mun','vinc.2008')], sum)
e_j_2010 <- aggregate(vinc.2010~cod_mun,FIRM2010[,c('cod_mun','vinc.2010')], sum)
e_j_2012 <- aggregate(vinc.2012~cod_mun,FIRM2012[,c('cod_mun','vinc.2012')], sum)
e_j_2014 <- aggregate(vinc.2014~cod_mun,FIRM2014[,c('cod_mun','vinc.2014')], sum)

# e_it
e_i_2006 <- setNames(data.frame(e_ij_2006$cod_mun, sum(aggregate(vinc.2006~cod_mun,subset(FIRM2006, FIRM2006$sbclas20 >=1000000 & FIRM2006$sbclas20 < 1100000)[,c('cod_mun','vinc.2006')], sum)[,c('vinc.2006')])), c('cod_mun', 'vinc.2006'))
e_i_2008 <- setNames(data.frame(e_ij_2008$cod_mun, sum(aggregate(vinc.2008~cod_mun,subset(FIRM2008, FIRM2008$sbclas20 >=1000000 & FIRM2008$sbclas20 < 1100000)[,c('cod_mun','vinc.2008')], sum)[,c('vinc.2008')])), c('cod_mun', 'vinc.2008'))
e_i_2010 <- setNames(data.frame(e_ij_2010$cod_mun, sum(aggregate(vinc.2010~cod_mun,subset(FIRM2010, FIRM2010$sbclas20 >=1000000 & FIRM2010$sbclas20 < 1100000)[,c('cod_mun','vinc.2010')], sum)[,c('vinc.2010')])), c('cod_mun', 'vinc.2010'))
e_i_2012 <- setNames(data.frame(e_ij_2012$cod_mun, sum(aggregate(vinc.2012~cod_mun,subset(FIRM2012, FIRM2012$cnae20subclasse >=1000000 & FIRM2012$cnae20subclasse < 1100000)[,c('cod_mun','vinc.2012')], sum)[,c('vinc.2012')])), c('cod_mun', 'vinc.2012'))
e_i_2014 <- setNames(data.frame(e_ij_2014$cod_mun, sum(aggregate(vinc.2014~cod_mun,subset(FIRM2014, FIRM2014$cnae20subclasse >=1000000 & FIRM2014$cnae20subclasse < 1100000)[,c('cod_mun','vinc.2014')], sum)[,c('vinc.2014')])), c('cod_mun', 'vinc.2014'))

# e_t
e_2006 <- setNames(data.frame(e_j_2006$cod_mun, sum(FIRM2006$vinc.2006)), c('cod_mun','vinc.2006'))
e_2008 <- setNames(data.frame(e_j_2008$cod_mun, sum(FIRM2008$vinc.2008)), c('cod_mun','vinc.2008'))
e_2010 <- setNames(data.frame(e_j_2010$cod_mun, sum(FIRM2010$vinc.2010)), c('cod_mun','vinc.2010'))
e_2012 <- setNames(data.frame(e_j_2012$cod_mun, sum(FIRM2012$vinc.2012)), c('cod_mun','vinc.2012'))
e_2014 <- setNames(data.frame(e_j_2014$cod_mun, sum(FIRM2014$vinc.2014)), c('cod_mun','vinc.2014'))

LOCQ_2006 <- setNames(Reduce(function(x, y) merge(x, y, by='cod_mun', all=TRUE), list(e_ij_2006, e_j_2006, e_i_2006, e_2006)), c('cod_mun','e_ij_2006','e_j_2006','e_i_2006','e_2006'))
LOCQ_2006$e_ij_2006[is.na(LOCQ_2006$e_ij_2006)] <- 0
LOCQ_2006$e_i_2006[is.na(LOCQ_2006$e_i_2006)] <- sum(LOCQ_2006$e_ij_2006)
LOCQ_2006$LOCQ_2006 <- apply(LOCQ_2006, 1, function(x){locq(x[2],x[3],x[4],x[5])})

LOCQ_2008 <- setNames(Reduce(function(x, y) merge(x, y, by='cod_mun', all=TRUE), list(e_ij_2008, e_j_2008, e_i_2008, e_2008)), c('cod_mun','e_ij_2008','e_j_2008','e_i_2008','e_2008'))
LOCQ_2008$e_ij_2008[is.na(LOCQ_2008$e_ij_2008)] <- 0
LOCQ_2008$e_i_2008[is.na(LOCQ_2008$e_i_2008)] <- sum(LOCQ_2008$e_ij_2008)
LOCQ_2008$LOCQ_2008 <- apply(LOCQ_2008, 1, function(x){locq(x[2],x[3],x[4],x[5])})

LOCQ_2010 <- setNames(Reduce(function(x, y) merge(x, y, by='cod_mun', all=TRUE), list(e_ij_2010, e_j_2010, e_i_2010, e_2010)), c('cod_mun','e_ij_2010','e_j_2010','e_i_2010','e_2010'))
LOCQ_2010$e_ij_2010[is.na(LOCQ_2010$e_ij_2010)] <- 0
LOCQ_2010$e_i_2010[is.na(LOCQ_2010$e_i_2010)] <- sum(LOCQ_2010$e_ij_2010)
LOCQ_2010$LOCQ_2010 <- apply(LOCQ_2010, 1, function(x){locq(x[2],x[3],x[4],x[5])})

LOCQ_2012 <- setNames(Reduce(function(x, y) merge(x, y, by='cod_mun', all=TRUE), list(e_ij_2012, e_j_2012, e_i_2012, e_2012)), c('cod_mun','e_ij_2012','e_j_2012','e_i_2012','e_2012'))
LOCQ_2012$e_ij_2012[is.na(LOCQ_2012$e_ij_2012)] <- 0
LOCQ_2012$e_i_2012[is.na(LOCQ_2012$e_i_2012)] <- sum(LOCQ_2012$e_ij_2012)
LOCQ_2012$LOCQ_2012 <- apply(LOCQ_2012, 1, function(x){locq(x[2],x[3],x[4],x[5])})

LOCQ_2014 <- setNames(Reduce(function(x, y) merge(x, y, by='cod_mun', all=TRUE), list(e_ij_2014, e_j_2014, e_i_2012, e_2014)), c('cod_mun','e_ij_2014','e_j_2014','e_i_2014','e_2014'))
LOCQ_2014$e_ij_2014[is.na(LOCQ_2014$e_ij_2014)] <- 0
LOCQ_2014$e_i_2014[is.na(LOCQ_2014$e_i_2014)] <- sum(LOCQ_2014$e_ij_2014)
LOCQ_2014$LOCQ_2014 <- apply(LOCQ_2014, 1, function(x){locq(x[2],x[3],x[4],x[5])})

LOCQ_mun <- drop_na(setNames(Reduce(function(x, y) merge(x, y, by='cod_mun', all=TRUE), list(MUNICIPIOS[,2],LOCQ_2006[,c(1,6)],LOCQ_2008[,c(1,6)],LOCQ_2010[,c(1,6)],LOCQ_2012[,c(1,6)],LOCQ_2014[,c(1,6)])),c('cod_mun','2006','2008','2010','2012','2014')))
LOCQ <- merge(FGROWTH[,1:2], LOCQ_mun, by='cod_mun', all.x=T)
}

# Variavel MKTPOT ====
  {
  # fonte: https://stackoverflow.com/questions/45784094/geosphere-dplyr-create-matrix-of-distance-between-coordinates
  print("Calculando MKTPOT..")
  # o mktpot de uma rgi ? a roma dos potenciais dos municipios.
  # PIB per capita x Popula??o da unidade geogr?fica sobre a distancia entre as regi?es.
  # no caso ser? usado o pib do municipio mesmo

  #pra n?o mexer no arquivo original, precisei editar os tipos de colunas no comando de importacao abaixo por caiuse de linhas NA.
  pib_municipal <- readxl::read_excel("pib_per_capita_anual_mun.xlsx", skip=3, col_types = c("text", "text", "numeric", 
                                                                                     "numeric", "numeric", "numeric", 
                                                                                     "numeric", "numeric", "numeric", 
                                                                                     "numeric", "numeric", "numeric"))[-5571,-c(4,6,8,10,12)]
  # pib_municipal[is.na(pib_municipal)] <- 0
  pib_municipal <- setNames(pib_municipal, c("cod_mun_dv","municipio","pib.2006","pib.2008","pib.2010","pib.2012","pib.2014"))
  mun_georef <- merge(separate(readxl::read_excel("mun_georef.xlsx"),'ns1:coordinates', into = c("longitude","latitude","zero"), sep = ",")[,c(1,3:4)], setNames(readxl::read_excel("AR_BR_RG_UF_MUN_2016.xlsx")[,c(5,7)], c("cod_mun_dv","area")),by=c("cod_mun_dv"))
  # Linha abaixo faz subset apenas para PE.
  # mun_georef <- subset(mun_georef, mun_georef$cod_mun_dv>=2600000 & mun_georef$cod_mun_dv<2700000)
  mun_georef$latitude <- as.numeric(mun_georef$latitude)
  mun_georef$longitude <- as.numeric(mun_georef$longitude)
  # A distancia e medida em METROS. A multiplicaçao e pra transformar em Km.
  dist.mat <- data.frame(distm(mun_georef[,c(2:3)], fun = distGeo))
  dist.mat <- dist.mat*1000
  diag(dist.mat) <- (2/3)*sqrt(mun_georef$area/pi)
  # Algumas distancias estao dando 0. Melhor excluir.
  # sum(mun_dist==0)
  # sum(is.na(dist.mat)
  # sum(mapply(is.infinite, dist.mat))
  # dist.mat <- 1/dist.mat
  # dist.mat[mapply(is.infinite, dist.mat)] <- NA
  dist.mat[dist.mat==0] <- NA
  # is.na(dist.mat) <- !dist.mat
  # mun_georef = cbind.data.frame(mun_georef, m)[,c(1,5:ncol(mun_georef))]
  dist.mat <- cbind.data.frame(mun_georef, dist.mat)[,-c(2:4)]
  dist.mat <- merge(pib_municipal[,-c(2)], dist.mat, by="cod_mun_dv")
  MKTPOT <- setNames(data.frame(dist.mat[,1]),c('cod_mun_dv'))
  MKTPOT$`2006` <- log(rowSums(dist.mat$pib.2006/dist.mat[,7:ncol(dist.mat)]))
  MKTPOT$`2008` <- log(rowSums(dist.mat$pib.2008/dist.mat[,7:ncol(dist.mat)]))
  MKTPOT$`2010` <- log(rowSums(dist.mat$pib.2010/dist.mat[,7:ncol(dist.mat)]))
  MKTPOT$`2012` <- log(rowSums(dist.mat$pib.2012/dist.mat[,7:ncol(dist.mat)]))
  MKTPOT$`2014` <- log(rowSums(dist.mat$pib.2014/dist.mat[,7:ncol(dist.mat)]))
  MKTPOT[mapply(is.nan, MKTPOT)] <- NA
  MKTPOT <- merge(FIRMAS[,c(2,10)], MKTPOT, by="cod_mun_dv")
  # rm(dist.mat)
  # Linha abaixo faz subset apenas par PE.
  gc()
}
# PLM FE e POOLED OLS ====
# Primeira linha só carrega o p.FIRMAS.rds
p.FIRMAS <- readRDS(here("p.FIRMAS.rds"))
{
  p.FIRMAS <- melt(FGROWTH[,-c(1)], id.vars = "cnpj")
  colnames(p.FIRMAS)[colnames(p.FIRMAS)=="variable"] <- "year"
  colnames(p.FIRMAS)[colnames(p.FIRMAS)=="value"] <- "FGROWTH"
  p.FIRMAS$SIZE <- melt(SIZE[,-c(1)], id.vars = "cnpj")$value
  p.FIRMAS$MKTPOT <- melt(MKTPOT[,-c(1)], id.vars = "cnpj")$value
  p.FIRMAS$LOCQ <- melt(LOCQ[,-c(1)], id.vars = "cnpj")$value
  
  p.FIRMAS <- REMUNERACAO[['VAR']] %>%
    melt(id.vars='cnpj') %>%
    setNames(c('cnpj','year','AVGINC')) %>%
    merge(p.FIRMAS, by=c('cnpj','year'))
  
  p.FIRMAS <- ESCOLARIDADE[['VAR']] %>%
    melt(id.vars='cnpj') %>%
    setNames(c('cnpj','year','SCHOOL')) %>%
    merge(p.FIRMAS, by=c('cnpj','year'))
}
# REFAZER TODOS OS PAINEIS, E SUAS ORDENS, PRA REFAZER NO STARGAZER

# SIZE
# log(AVGINC+0.00001)
# log(SCHOOL+0.00001)
# MKTPOT
# log(LOCQ+0.00001)
# factor(year)

{
  panel_01 <- plm(FGROWTH ~ SIZE + factor(year), data = p.FIRMAS, model = "within", index = c("cnpj", "year"))
  summary(panel_01)
}

{
  panel_02 <- plm(FGROWTH ~ SIZE + log(AVGINC+0.00001) + log(SCHOOL+0.00001) + factor(year), data = p.FIRMAS, model = "within", index = c("cnpj", "year"))
  summary(panel_02)
}

{
  panel_03 <- plm(FGROWTH ~ SIZE + log(AVGINC+0.00001) + log(SCHOOL+0.00001) + MKTPOT + log(LOCQ+0.00001) + factor(year), data = p.FIRMAS, model = "within", index = c("cnpj", "year"))
  summary(panel_03)
}


{
  panel_01pooled <- plm(FGROWTH ~ SIZE + factor(year), data = p.FIRMAS, model = "pooling", index = c("cnpj", "year"))
  summary(panel_01pooled)
}

{
  panel_02pooled <- plm(FGROWTH ~ SIZE + log(AVGINC+0.00001) + log(SCHOOL+0.00001) + factor(year), data = p.FIRMAS, model = "pooling", index = c("cnpj", "year"))
  summary(panel_02pooled)
}

{
  panel_03pooled <- plm(FGROWTH ~ SIZE + log(AVGINC+0.00001) + log(SCHOOL+0.00001) + MKTPOT + log(LOCQ+0.00001) + factor(year), data = p.FIRMAS, model = "pooling", index = c("cnpj", "year"))
  summary(panel_03pooled)
}



# Robustez 01: Portos ====
robustez <- readRDS(here('robustez.rds'))

# 
# FIRMAS_geo <- readRDS('FIRMAS_geo.rds')
# PORTOS_geo <- readRDS('PORTOS_geo.rds')
# 
# F_lonlat <- data.frame(as.numeric(FIRMAS_geo$lon), as.numeric(FIRMAS_geo$lat))
# P_lonlat <- data.frame(as.numeric(PORTOS_geo$lon), as.numeric(PORTOS_geo$lat))
# # matriz_distancias <- data.frame(distm(F_lonlat,P_lonlat, fun = distGeo))
# FIRMAS_dist <- setNames(data.frame(FIRMAS_geo[,1], rowMins(as.matrix(distm(F_lonlat,P_lonlat, fun = distGeo)), value = T)), c('cnpj','DIST'))
# 
# # FIRMAS_dist <- filter(FIRMAS_dist, DIST!=0)
# 
#   portos <- aggregate(NOME_PORT ~ cod_uf, readxl::read_xlsx(here("portos.xlsx"), skip=1), FUN = length)
#   robustez <- merge(subset(FIRM2006, FIRM2006$sbclas20 >=1000000 & FIRM2006$sbclas20 < 1100000)[,c(1:3)], FIRM2016[,c(1,3)], by="cnpj")
#   robustez$cod_uf <- substr(robustez$cod_mun, start = 1, stop = 2)
#   robustez <- merge(robustez, FIRMAS_dist, by="cnpj")
#   robustez$FGROWTH <- log(robustez$vinc.2016+0.0001)-log(robustez$vinc.2006+0.0001)
#   robustez$SIZE <- log(robustez$vinc.2006+0.0001)
#   robustez$DIST <- log(robustez$DIST+0.0001)
#   robustez <- merge(robustez, merge( setNames(data.frame(dist.mat$cod_mun_dv, log(rowSums(dist.mat$pib.2006/dist.mat[,7:ncol(dist.mat)]))), c('cod_mun_dv', 'MKTPOT')), MUNICIPIOS[,c(2:3)], by='cod_mun_dv')[,-c(1)],by='cod_mun')
#   robustez$LOCQ <- merge(robustez, LOCQ_mun[,1:2], by='cod_mun', all.x=T)[,c('2006')]
#   robustez <- merge(robustez, ESCOLARIDADE[['2006']], by='cnpj', all.x=T)
#   robustez <- merge(robustez, REMUNERACAO[['2006']], by='cnpj', all.x=T)
#   robustez <- setNames(as_tibble(robustez[,c(1,6:12)]),c('cnpj','DIST','FGROWTH','SIZE','MKTPOT','LOCQ','SCHOOL','AVGINC'))
  
  robustez <- drop_na(robustez)
  

  robustez_01 <- lm(FGROWTH ~ SIZE, robustez)
  robustez_02 <- lm(FGROWTH ~ SIZE + log(SCHOOL+0.000001) + log(AVGINC+0.000001), robustez)
  robustez_03 <- lm(FGROWTH ~ SIZE + DIST + MKTPOT + log(LOCQ+0.0001), robustez)
  robustez_04 <- lm(FGROWTH ~ SIZE + log(SCHOOL+0.000001) + log(AVGINC+0.000001) + DIST + MKTPOT + log(LOCQ+0.0001), robustez)
  
 
















# Estatistica Descritiva ====
  
  # Sumario dos vinculos
  xtable(summary(FIRMAS[,c(3:8)]))
  
  # Numero de firmas consideradas por divisao geografica
  xtable(arrange(aggregate(cnpj~nome_uf, FIRMAS, FUN=length), -cnpj)[1:5,])
  xtable(arrange(aggregate(cnpj~nome_rgint, FIRMAS, FUN=length), -cnpj)[1:5,])
  xtable(arrange(aggregate(cnpj~nome_rgi, FIRMAS, FUN=length), -cnpj)[1:5,])
  xtable(arrange(aggregate(cnpj~nome_mun, FIRMAS, FUN=length), -cnpj)[1:5,])
  
  # Tabela dos resultados
  stargazer(panel_01,
            panel_02,
            panel_03,
            panel_01pooled,
            panel_02pooled,
            panel_03pooled,
            title="Panel Data Regressions",
            align = T,
            column.sep.width = "1pt",
            omit.stat=c("f"),
            df=FALSE
            )
  stargazer(robustez_01,
            robustez_02,
            robustez_03,
            robustez_04,
            title="OLS Regressions",
            align = T,
            column.sep.width = "1pt",
            df=FALSE,
            dep.var.caption = c('Firm Employment Growth'),
            dep.var.labels = c('FGROWTH'),
            covariate.labels = c('SIZE','SCHOOLING','AVG INCOME','DISTANCE','MKT POTENTIAL','LOC QUOTIENT','Constant'),
            df=F,
            column.sep.width="1pt",
            align=TRUE
            )


q(save='yes')