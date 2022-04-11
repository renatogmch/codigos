# Instalar o pacote de dados em painel. Para maiores informações, visitar:
# https://cran.r-project.org/web/packages/plm/vignettes/plm.pdf
# https://cran.r-project.org/web/packages/plm/plm.pdf
# https://www.princeton.edu/~otorres/Panel101R.pdf
# Vídeo explicativo (inglês): https://www.youtube.com/watch?v=1pST2lUx6QM&t=203s

#### Instalar package para Dados em Painel
{
install.packages("plm")
library(plm)
}

# aqui cria-se uma outra base de dados adaptada em painel de acordo com o cross section e o tempo. Precisa renomear. Ex: Meus dados são "sua_base_de_dados", e agora fica "p_sua_base_de_dados".
p_sua_base_de_dados <- pdata.frame(sua_base_de_dados, index=c("Coluna_Cross_Section","Coluna_Tempo"), drop.index=TRUE, row.names=TRUE)

# comando em si:
regressaowithin <- plm(y ~ x1 , data=p_sua_base_de_dados, model="within")
regressaobetween <- plm(y ~ x1 , data=p_sua_base_de_dados, model="between")
# model pode ser pooling, within(fixo) e between(variável).

# Rodar tanto o within quanto o between para fazer o teste de Hausman e verificar qual deve ser usado:
phtest(regressaowithin, regressaobetween)
# se o p-value <0,05 use o within(efeitos fixos).