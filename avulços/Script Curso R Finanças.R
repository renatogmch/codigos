# Curso de Financas Aplicadas com Software R
# Renato Gomes Chaves
# 09/03/2020

# criar pasta na area de trabalho e
# definir como o 'working directory'
setwd()

# Instalacao de pacotes
install.packages('tidyverse')
install.packages('tidyquant')
install.packages('PortfolioAnalytics')
install.packages('xml2')

# Alem de instalar, é cnessario carregar os pacotes
# sempre que se reiniciar o RStudio
library(tidyverse)
library(tidyquant)
library(PortfolioAnalytics)
library(xml2)

# carregar dados
# acessar o link:
# http://www.b3.com.br/pt_br/market-data-e-indices/indices/indices-amplos/indice-ibovespa-ibovespa-composicao-da-carteira.htm
# Download do arquivo ao final da pagina
