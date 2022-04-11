##################################################
# Script Dados em Painel no R - ENABER 2018
# Data: 02/10/2018
# Autor: Joao Ricardo F de Lima
# Versao: 1.0
##################################################

#Verificando o diretorio que o R esta direcionado
getwd()

#Direcionado o R para o Diretorio a ser trabalhado
setwd('c:/tempecon/dados_enaber2018')

#Limpa o Ambiente Global
rm(list=ls())

#Pacotes a serem utilizados
library(lmtest)
library(plm)
library(tseries)

#Extração dos dados 
cps<-read.csv2(file='cps.csv', header=T,sep=";",dec=",")
regressao1 <- lm(lwage ~ y85*(educ+female)+exper+I((exper^2)/100)+union,
                 data=cps)
summary(regressao1)

#Estimador de Diferenças em Diferenças
kielmc<-read.csv2(file='kielmc.csv', header=T,sep=";",dec=",")

# Regressões separadas para 1978 e 1981
coef.78<-coef(lm(rprice~nearinc, data=kielmc, subset=(year==1978)))
coef.81<-coef(lm(rprice~nearinc, data=kielmc, subset=(year==1981)))
coef.81[2]-coef.78[2]

#Mesmos Resultados, outra forma
coeftest(lm(rprice~nearinc*y81, data=kielmc))
coeftest(lm(log(rprice)~nearinc*y81, data=kielmc))

#Inclusão de covariaveis
coeftest(lm(log(price)~nearinc*y81+lintst+lland+larea+rooms, data=kielmc) )

#Painel de dois períodos
crime2<-read.csv2(file='crime2.csv', header=T,sep=";",dec=",")

#Modelo de regressão simples para 1987
reg3 <- lm(crmrte~unem, data=crime2, subset=(year==87))
summary(reg3)

#Modelo simples de efeitos fixos
reg4 <- lm(crmrte~d87+unem, data=crime2)
summary(reg4)

#Estimação da equação de primeiras diferenças
dcrmrte<-crime2$crmrte[crime2$year=='87']-crime2$crmrte[crime2$year=='82']
dunem<-crime2$unem[crime2$year=='87']-crime2$unem[crime2$year=='82']
reg5 <- lm(dcrmrte~dunem)
summary(reg5)

#Estimação com dados organizados em Painel
crime2.p <- pdata.frame(crime2, index=46)
pdim(crime2.p)
# Cálculo manual da primeira diferença
crime2.p$dcrmrte <- diff(crime2.p$crmrte)
crime2.p$dunem   <- diff(crime2.p$unem)
# Estimando o modelo com a função lm sobre as primeiras diferenças
coeftest(lm(dcrmrte~dunem, data=crime2.p))
# Estimando o modelo com a função plm sobre os dados originais
# Observe que a interpretação muda completamente.
coeftest(plm(crmrte~unem, data=crime2.p, model="fd"))

#A Diferenciação com mais de dois períodos de tempo
crime4<-read.csv2(file='crime4.csv', header=T,sep=";",dec=",")

crime4.p <- pdata.frame(crime4, index=c("county","year") )
reg7<- plm(diff(log(crmrte))~d83+d84+d85+d86+d87+diff(lprbarr)+
               diff(lprbconv)+diff(lprbpris)+diff(lavgsen)+diff(lpolpc),
             data=crime4.p, model='pooling')
summary(reg7)

#Modelo de efeitos Fixos
wagepan<-read.csv2(file='wagepan.csv', header=T,sep=";",dec=",")

#Criar o data frame para painel
wagepan.p <- pdata.frame(wagepan, index=c("nr","year") )
pdim(wagepan.p)
# Estimar o modelo de efeitos fixos
summary(plm(lwage~married+union+factor(year)*educ,
            data=wagepan.p, model="within"))
#Pode-se verificar que o retorno da educação é cerca de 
#três pontos percentuais maior em 1987 do que no ano-base, 
#que é 1980.

#Estimar o modelo de efeitos Aleatórios
wagepan.p$yr <- factor(wagepan.p$year)
pvar(wagepan.p)
summary(plm(lwage~educ+black+hisp+exper+I(exper^2)+married+union+yr, 
             data=wagepan.p, model="random"))

mfre<- plm(lwage~educ+black+hisp+exper+I(exper^2)+married+union+yr, 
    data=wagepan.p, model="random")
mffe<- plm(lwage~educ+black+hisp+exper+I(exper^2)+married+union+yr, 
           data=wagepan.p, model="within")
#Teste de Hausman
phtest(mffe, mfre)

#Extracao dos dados Gujarati
dados <- read.csv2('painel_gujarati.csv', header=T, sep=";", dec=".")
names(dados)[1] <- c("crossid")

#Regressao com dados empilhados
regressao1 <- lm(ct ~ q+lf+pf, data=dados)
summary(regressao1)

#Regressao com o uso de variaveis Dummy e Efeitos Fixos
fixed.dum <- lm(ct ~ q+lf+pf+factor(crossid)-1, data=dados)
summary(fixed.dum)

#Configuracao como dados em painel
pdados <- pdata.frame(dados, c("crossid","dateid"))
pdados <- pdata.frame(dados, index=6) #opção para painel balanceado (n=90/t=15)
#Checar as dimensoes do painel
pdim(pdados)
#Checar por variação no corte transversal e no tempo
pvar(pdados)

#Pool Test
#teste F de estabilidade dos coeficientes do modelo em painel. Testa a hipótese de que os coefs
# excluindo os interceptos sao iguais.
pooltest(ct ~ q+lf+pf, data=pdados, model='within')
#Testa a hipótese de que os coefs incluindo os interceptos sao iguais.
pooltest(ct ~ q+lf+pf, data=pdados, model='pooling')

#Testa se não há efeitos (individual u tempo) não 
# observados nos resíduos (ausencia de autocorrelacao residuos).
# H0=correlação zero entre os residuos do mesmo grupo. 
pwtest(ct ~ q+lf+pf, data=pdados, model='pooling')

#Teste para verificar conjuntamente a existencia de efeito aleatorio
# e correlação serial. A H0 é ausencia dos dois.
pbsytest(ct ~ q+lf+pf, data=pdados, model='pooling', test = 'j')

#Teste para verificar a existencia de 
# correlação serial. A H0 é ausencia de correlação.
pbsytest(ct ~ q+lf+pf, data=pdados, model='pooling')

#Teste para verificar a existencia de efeito aleatorio
#. A H0 é ausencia de efeito aleatório.
pbsytest(ct ~ q+lf+pf, data=pdados, model='pooling', test = 're')

#Teste para verificar a existencia de correlação serial
# em erros sob efeitos aleatórios (AR(1) ou MA(1). 
# A H0 é ausencia de correlação.
pbltest(ct ~ q+lf+pf, data=pdados, model='pooling', alternative = 'oneside')

#Uso de Funções diversas com Panel Data
pdados$q.l <- lag(pdados$q) #1 Lag
pdados$q.d <- diff(pdados$q) #1 Diff
pdados$q.B <- Between(pdados$q) #Média por Cross-Section
pdados$q.W <- Within(pdados$q) #x-x.bar

#Estimacao de modelo com efeitos Fixos
ffe <- plm(ct ~ q+lf+pf, model="within", data = pdados)
summary(ffe)
#O coeficiente estimado indica quanto ct muda ao longo do tempo
# na media por empresa aerea, quando a variavel aumenta uma unidade 
fixef(ffe) #mostra os seis diferentes interceptos
summary(fixef(ffe)) #mostra os seis diferentes interceptos com signif.

#Teste do RFE contra MqO
#Hipotese Nula: Modelo de MqO e melhor
pFtest(ffe, regressao1)

#Teste de Correlação serial quando T for pequeno em 
#Modelo de Efeito Fixo.H0: ausencia de correlacao serial
pwartest(ffe)

#Wooldridge first-difference-based test for AR(1) errors 
#in levels or first-differenced panel models
pwfdtest(ct ~ q+lf+pf, data = pdados)

#Estimacao de modelo com Efeitos Aleatorios
fre <- plm(ct ~ q+lf+pf, model="random", data = pdados)
summary(fre)
#Os coeficientes estimados indicam o efeito medio de X sobre Y quando
# X muda ao longo do tempo e entre as empresas aereas em uma unidade.

#Testes de Hausman
#HO: Efeitos aleatorios e melhor
#basicamente testa se os erros sao correlacionados
#com os regressores e a H0 é que nao sao.

phtest(ffe, fre)

#Teste de estacionariedade das series
#Hipotese nula: os dados possuem raiz unitaria
#library(tseries)
adf.test(pdados$ct, k=2)
adf.test(pdados$lf, k=2)
adf.test(pdados$pf, k=2)
adf.test(pdados$q, k=2)

#Teste para Correlacao Serial
#Hipotese Nula: ausencia de autocorrelacao serial
pbgtest(ffe, order=2)

#Teste de Breusch-Pagan de homocedasticidade
#Hipotese nula eh homocedasticidade
bptest(ct ~ q+lf+pf+factor(crossid), data=dados, studentize=F)

#Solucao de problema de Heterocedasticidade
#Estimacao com erros padroes robustos Modelo Efeitos Fixos
coeftest(ffe) #resultado sem correcao
coeftest(ffe, vcovHC(ffe, type="HC4")) #resultado corrigido
#Estimacao com erros padroes robustos Modelo Efeitos Aleatorios
coeftest(fre)
coeftest(fre, vcovHC(fre, type="HC4"))

#Solucao de problema de Autocorrelacao
#Estimacao com erros padroes robustos Modelo Efeitos Fixos
coeftest(ffe) #resultado sem correcao
coeftest(ffe, vcovHC(ffe, cluster="group")) #resultado corrigido
#Estimacao com erros padroes robustos Modelo Efeitos Aleatorios
coeftest(fre) #resultado sem correcao
coeftest(fre, vcovHC(fre, cluster="group")) #resultado corrigido

q(save='yes')