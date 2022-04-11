# Script Artigo Convergencia IDEB ----
# Data: 10/04/19
# Autor: Renato Gomes Chaves
# Versao: 1.4


# pacotes:
# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
  {
    packages = c("here","tidyverse","dplyr","gvlma","lmtest", "readr", "data.table", "reshape2", "plm", "normtest", "lmtest", "MESS", "stargazer", "xtable", "SAScii", "data.table", "tseries", "gtools","doParallel","foreach")
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

# CONVERGENCIA ABSOLUTA ------------------------------------------------------------------------------

{
  # importacao dos DBs ja sem NA's e com colunas renomeadas
  ideb_iniciais_br <- drop_na(setNames(readxl::read_excel(here("raw/divulgacao_anos_iniciais-escolas-2017.xlsx"), na = "-"),c('nom_uf','cod_mun','nom_mun','cod_esc','nom_esc','rede','ideb_2005','ideb_2007','ideb_2009','ideb_2011','ideb_2013','ideb_2015','ideb_2017'))[,c(2,4,7:13)])
  ideb_iniciais <- subset(drop_na(setNames(readxl::read_excel(here("raw/divulgacao_anos_iniciais-escolas-2017.xlsx"), na = "-"),c('nom_uf','cod_mun','nom_mun','cod_esc','nom_esc','rede','ideb_2005','ideb_2007','ideb_2009','ideb_2011','ideb_2013','ideb_2015','ideb_2017'))[,c(2,4,7:13)]), cod_mun>= 2300000 & cod_mun<2400000)
  ideb_finais_br <- drop_na(setNames(readxl::read_excel(here("raw/divulgacao_anos_finais-escolas-2017.xlsx"), na = "-"),c('nom_uf','cod_mun','nom_mun','cod_esc','nom_esc','rede','ideb_2005','ideb_2007','ideb_2009','ideb_2011','ideb_2013','ideb_2015','ideb_2017'))[,c(2,4,7:13)])
  ideb_finais <- subset(drop_na(setNames(readxl::read_excel(here("raw/divulgacao_anos_finais-escolas-2017.xlsx"), na = "-"),c('nom_uf','cod_mun','nom_mun','cod_esc','nom_esc','rede','ideb_2005','ideb_2007','ideb_2009','ideb_2011','ideb_2013','ideb_2015','ideb_2017'))[,c(2,4,7:13)]), cod_mun>= 2300000 & cod_mun<2400000)
  
  # dados em formato longo pro ceara
  p_ideb_iniciais <- setNames(melt(ideb_iniciais[,c(2:8)], id.vars = 'cod_esc'),c('cod_esc','ano','nota_base'))
  p_ideb_finais <- setNames(melt(ideb_finais[,c(2:8)], id.vars = 'cod_esc'),c('cod_esc','ano','nota_base'))
  p_ideb_iniciais$nota_crescimento <- setNames(melt(
    setNames(data.frame(ideb_iniciais$cod_esc,ideb_iniciais$ideb_2007/ideb_iniciais$ideb_2005, ideb_iniciais$ideb_2009/ideb_iniciais$ideb_2007, 
                        ideb_iniciais$ideb_2011/ideb_iniciais$ideb_2009, ideb_iniciais$ideb_2013/ideb_iniciais$ideb_2011, 
                        ideb_iniciais$ideb_2015/ideb_iniciais$ideb_2013, ideb_iniciais$ideb_2017/ideb_iniciais$ideb_2015),c('cod_esc','ideb_2005','ideb_2007','ideb_2009','ideb_2011','ideb_2013','ideb_2015')), 
    id.vars = 'cod_esc'),c('cod_mun','ano','nota_crescimento'))[,c('nota_crescimento')]
  p_ideb_finais$nota_crescimento <- setNames(melt(
    setNames(data.frame(ideb_finais$cod_esc,ideb_finais$ideb_2007/ideb_finais$ideb_2005, ideb_finais$ideb_2009/ideb_finais$ideb_2007, 
                        ideb_finais$ideb_2011/ideb_finais$ideb_2009, ideb_finais$ideb_2013/ideb_finais$ideb_2011, 
                        ideb_finais$ideb_2015/ideb_finais$ideb_2013, ideb_finais$ideb_2017/ideb_finais$ideb_2015),c('cod_esc','ideb_2005','ideb_2007','ideb_2009','ideb_2011','ideb_2013','ideb_2015')), 
    id.vars = 'cod_esc'),c('cod_mun','ano','nota_crescimento'))[,c('nota_crescimento')]
  
  #  Dados em formato longo pro brasil
  p_ideb_iniciais_br <- setNames(melt(ideb_iniciais_br[,c(2:8)], id.vars = 'cod_esc'),c('cod_esc','ano','nota_base'))
  p_ideb_finais_br <- setNames(melt(ideb_finais_br[,c(2:8)], id.vars = 'cod_esc'),c('cod_esc','ano','nota_base'))
  p_ideb_iniciais_br$nota_crescimento <- setNames(melt(
    setNames(data.frame(ideb_iniciais_br$cod_esc,ideb_iniciais_br$ideb_2007/ideb_iniciais_br$ideb_2005, ideb_iniciais_br$ideb_2009/ideb_iniciais_br$ideb_2007, 
                        ideb_iniciais_br$ideb_2011/ideb_iniciais_br$ideb_2009, ideb_iniciais_br$ideb_2013/ideb_iniciais_br$ideb_2011, 
                        ideb_iniciais_br$ideb_2015/ideb_iniciais_br$ideb_2013, ideb_iniciais_br$ideb_2017/ideb_iniciais_br$ideb_2015),c('cod_esc','ideb_2005','ideb_2007','ideb_2009','ideb_2011','ideb_2013','ideb_2015')), 
    id.vars = 'cod_esc'),c('cod_mun','ano','nota_crescimento'))[,c('nota_crescimento')]
  p_ideb_finais_br$nota_crescimento <- setNames(melt(
    setNames(data.frame(ideb_finais_br$cod_esc,ideb_finais_br$ideb_2007/ideb_finais_br$ideb_2005, ideb_finais_br$ideb_2009/ideb_finais_br$ideb_2007, 
                        ideb_finais_br$ideb_2011/ideb_finais_br$ideb_2009, ideb_finais_br$ideb_2013/ideb_finais_br$ideb_2011, 
                        ideb_finais_br$ideb_2015/ideb_finais_br$ideb_2013, ideb_finais_br$ideb_2017/ideb_finais_br$ideb_2015),c('cod_esc','ideb_2005','ideb_2007','ideb_2009','ideb_2011','ideb_2013','ideb_2015')), 
    id.vars = 'cod_esc'),c('cod_mun','ano','nota_crescimento'))[,c('nota_crescimento')]
  
  
  # saveRDS(p_ideb_iniciais, file=here("processed/p_ideb_iniciais_ce.rds"))
  # saveRDS(p_ideb_finais, file=here("processed/p_ideb_finais_ce.rds"))

  # PLM de convergencia absoluta
  # usando within
  absoluta_iniciais_ce <- plm(log(nota_crescimento) ~ log(nota_base), p_ideb_iniciais, model = 'within', index = c('cod_esc','ano'))
  absoluta_finais_ce <- plm(log(nota_crescimento) ~ log(nota_base), p_ideb_finais, model = 'within', index = c('cod_esc','ano'))
  absoluta_iniciais_br <- plm(log(nota_crescimento) ~ log(nota_base), p_ideb_iniciais_br, model = 'within', index = c('cod_esc','ano'))
  absoluta_finais_br <- plm(log(nota_crescimento) ~ log(nota_base), p_ideb_finais_br, model = 'within', index = c('cod_esc','ano'))
  
  absoluta_iniciais_ce_pooled <- plm(log(nota_crescimento) ~ log(nota_base), p_ideb_iniciais, model = 'pooling', index = c('cod_esc','ano'))
  absoluta_finais_ce_pooled <- plm(log(nota_crescimento) ~ log(nota_base), p_ideb_finais, model = 'pooling', index = c('cod_esc','ano'))
  absoluta_iniciais_br_pooled <- plm(log(nota_crescimento) ~ log(nota_base), p_ideb_iniciais_br, model = 'pooling', index = c('cod_esc','ano'))
  absoluta_finais_br_pooled <- plm(log(nota_crescimento) ~ log(nota_base), p_ideb_finais_br, model = 'pooling', index = c('cod_esc','ano'))
  
}




# CONVERGENCIA CONDICIONAL ------------------------------------------------------------------------------

  # Importacao dos dados ja setado pro CEARA
  # Variaveis: Cod da escola, Num de funcionarios e alunos/turma.
  # saveRDS(read_delim("~/Dados Artigo Macro/CENSOESC_2015.CSV", "|", escape_double = FALSE, trim_ws = TRUE), file=here("processed/CENSOESC_2015.rds"))
VAR_ESCOLAS <-  list(
  #  2007
  c('PK_COD_ENTIDADE',
               'ID_DEPENDENCIA_ADM',
               'ID_AGUA_INEXISTENTE',
               'ID_ENERGIA_INEXISTENTE',
               'ID_ESGOTO_INEXISTENTE',
               'ID_BIBLIOTECA',
               'ID_COZINHA',
               'ID_ALIMENTACAO',
               'ID_LABORATORIO_CIENCIAS',
               'ID_QUADRA_ESPORTES',
               'NUM_COMP_ALUNOS',
               'NUM_FUNCIONARIOS'
              ),
  # 2009
  c('PK_COD_ENTIDADE',
    'ID_DEPENDENCIA_ADM',
    'ID_AGUA_INEXISTENTE',
    'ID_ENERGIA_INEXISTENTE',
    'ID_ESGOTO_INEXISTENTE',
    'ID_BIBLIOTECA',
    'ID_COZINHA',
    'ID_ALIMENTACAO',
    'ID_LABORATORIO_CIENCIAS',
    'ID_QUADRA_ESPORTES',
    'NUM_COMP_ALUNOS',
    'NUM_FUNCIONARIOS'
  ),
  # 2011
  c(
    'PK_COD_ENTIDADE',
    'ID_DEPENDENCIA_ADM',
    'ID_AGUA_INEXISTENTE',
    'ID_ENERGIA_INEXISTENTE',
    'ID_ESGOTO_INEXISTENTE',
    'ID_BIBLIOTECA',
    'ID_COZINHA',
    'ID_ALIMENTACAO',
    'ID_LABORATORIO_CIENCIAS',
    'ID_QUADRA_ESPORTES_COBERTA',
    'ID_QUADRA_ESPORTES_DESCOBERTA',
    'NUM_COMP_ALUNOS',
    'NUM_FUNCIONARIOS'
  ),
  # 2013
  c(
    'PK_COD_ENTIDADE',
    'ID_DEPENDENCIA_ADM',
    'ID_AGUA_INEXISTENTE',
    'ID_ENERGIA_INEXISTENTE',
    'ID_ESGOTO_INEXISTENTE',
    'ID_BIBLIOTECA',
    'ID_COZINHA',
    'ID_ALIMENTACAO',
    'ID_LABORATORIO_CIENCIAS',
    'ID_QUADRA_ESPORTES_COBERTA',
    'ID_QUADRA_ESPORTES_DESCOBERTA',
    'NUM_COMP_ALUNOS',
    'NUM_FUNCIONARIOS'
  ),
  # 2015
  c(
    'CO_ENTIDADE',
    'TP_DEPENDENCIA',
    'IN_AGUA_INEXISTENTE',
    'IN_ENERGIA_INEXISTENTE',
    'IN_ESGOTO_INEXISTENTE',
    'IN_BIBLIOTECA',
    'IN_COZINHA',
    'IN_ALIMENTACAO',
    'IN_LABORATORIO_CIENCIAS',
    'IN_QUADRA_ESPORTES',
    'NU_COMP_ALUNO',
    'NU_FUNCIONARIOS'
  ),
  # 2017
  c(
    'CO_ENTIDADE',
    'TP_DEPENDENCIA',
    'IN_AGUA_INEXISTENTE',
    'IN_ENERGIA_INEXISTENTE',
    'IN_ESGOTO_INEXISTENTE',
    'IN_BIBLIOTECA',
    'IN_COZINHA',
    'IN_ALIMENTACAO',
    'IN_LABORATORIO_CIENCIAS',
    'IN_QUADRA_ESPORTES',
    'NU_COMP_ALUNO',
    'NU_FUNCIONARIOS'
  )
)

  

  CENSO_ESCOLAS <- foreach(i = list.files(path = here('/raw'), pattern = "CENSOESC_20[0-1][0-9][.]CSV$"),
                     n = VAR_ESCOLAS,
                    .packages='tidyverse') %dopar% {
                      as.character(i) %>%
                      read_delim('|', escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE) %>%
                      select(as.character(n))
                    }
  names(CENSO_ESCOLAS) <- c('2007','2009','2011','2013','2015','2017')
  
  
  TURMAS
  DOCENTE
  
  
  
  Variaveis = lista
  i = lista de arquivos
  readcsv(i)
  select(as.character(lista das var))
  filter(c(1) >=23000000 & c(1)<24000000)
  
  



{  
  CENSOESC <- merge(readRDS(here('processed/CENSOESC.rds')),readRDS(here('processed/TURMAS.rds')), by=c(1))
  CENSOESC_iniciais <- merge(ideb_iniciais[,-c(1,3)],CENSOESC, by='cod_esc')
  CENSOESC_finais <- merge(ideb_finais[,-c(1,3)],CENSOESC, by='cod_esc')
  
  p_CENSOESC_iniciais <- setNames(melt(CENSOESC_iniciais[,c(1:6)], id.vars = 'cod_esc'), c('cod_esc','ano','base'))
  p_CENSOESC_iniciais$'seguinte' <- melt(CENSOESC_iniciais[,c(1,3:7)], id.vars = 'cod_esc')$value
  p_CENSOESC_iniciais$'alu_por_sala' <- melt(CENSOESC_iniciais[,c(1,13:17)], id.vars = 'cod_esc')$value
  p_CENSOESC_iniciais$'num_func' <- melt(CENSOESC_iniciais[,c(1,8:12)], id.vars = 'cod_esc')$value
  
  p_CENSOESC_finais <- setNames(melt(CENSOESC_finais[,c(1:6)], id.vars = 'cod_esc'), c('cod_esc','ano','base'))
  p_CENSOESC_finais$'seguinte' <- melt(CENSOESC_finais[,c(1,3:7)], id.vars = 'cod_esc')$value
  p_CENSOESC_finais$'alu_por_sala' <- melt(CENSOESC_finais[,c(1,13:17)], id.vars = 'cod_esc')$value
  p_CENSOESC_finais$'num_func' <- melt(CENSOESC_finais[,c(1,8:12)], id.vars = 'cod_esc')$value
  
  condic_inicias_01 <- plm(log(seguinte/base) ~ log(base+0.000001) + log(alu_por_sala+0.000001) + log(num_func+0.000001), p_CENSOESC_iniciais, model="within", index=c("cod_esc","ano"))
  condic_inicias_02 <- plm(log(seguinte/base) ~ log(base+0.000001) + log(alu_por_sala+0.000001) + log(num_func+0.000001) + factor(ano), p_CENSOESC_iniciais, model="within", index=c("cod_esc","ano"))
  condic_finais_01 <- plm(log(seguinte/base) ~ log(base+0.000001) + log(alu_por_sala+0.000001) + log(num_func+0.000001), p_CENSOESC_finais, model="within", index=c("cod_esc","ano"))
  condic_finais_02 <- plm(log(seguinte/base) ~ log(base+0.000001) + log(alu_por_sala+0.000001) + log(num_func+0.000001) + factor(ano), p_CENSOESC_finais, model="within", index=c("cod_esc","ano"))
  condic_inicias_01_pooled <- plm(log(seguinte/base) ~ log(base+0.000001) + log(alu_por_sala+0.000001) + log(num_func+0.000001), p_CENSOESC_iniciais, model="pooling", index=c("cod_esc","ano"))
  condic_finais_01_pooled <-  plm(log(seguinte/base) ~ log(base+0.000001) + log(alu_por_sala+0.000001) + log(num_func+0.000001), p_CENSOESC_finais, model="pooling", index=c("cod_esc","ano"))
  gc()
  }
  # summary(condic_inicias_01)
  # summary(condic_inicias_02)
  # summary(condic_finais_01)
  # summary(condic_finais_02)
  

  
  ## Teste de Hausman: se o p-value <0,05 (tipo < 2.2e-16) use o within(efeitos fixos)
  # model pode ser pooling(OLS), within(fixo) e between(vari?vel)
  hausmanT_condicional_inicial <- phtest(plm_w_condicional_inicial, plm_b_condicional_inicial)
  hausmanT_condicional_final <- phtest(plm_w_condicional_final, plm_b_condicional_final)



# TESTES ------------------------------------------------------------------------------


  ## Teste de Hausman: se o p-value <0,05 (tipo < 2.2e-16) use o within(efeitos fixos)
  # model pode ser pooling(OLS), within(fixo) e between(vari?vel).
  phtest(plm_w_absoluto_inicial, plm_b_absoluto_inicial)
  phtest(plm_w_absoluto_final, plm_b_absoluto_final)
  #usa-se within em ambos os casos.
  
  ## Teste de Hausman: se o p-value <0,05 (tipo < 2.2e-16) use o within(efeitos fixos)
  # model pode ser pooling(OLS), within(fixo) e between(vari?vel)
  phtest(plm_w_condicional_inicial, plm_b_condicional_inicial)
  phtest(plm_w_condicional_final, plm_b_condicional_final)
  
  # Teste de Breusch-Pagan para heterocedasticidade:
  bptest(plm_w_absoluto_inicial)
  bptest(plm_w_absoluto_final)
  bptest(plm_w_condicional_inicial)
  bptest(plm_w_condicional_final)
  

#ESTATISTICA DESCRITIVA ------------------------------------------------------------------------------

  # No latex precisei fazer alguns tweaks:
  # Ajeitar cabecalho, mudar de en pra pt, adicionar meia vida e velocidade manualmente 
  
  stargazer(absoluta_iniciais_br,
            absoluta_iniciais_ce,
            absoluta_finais_br,
            absoluta_finais_ce,
            title="Resultados da Regressão de Convergência Absoluta - Painel Efeitos Fixos",
            dep.var.caption = c('Crescimento Anual'),
            column.labels = c('Anos Iniciais', 'Anos Finais'),
            column.separate = c(2,2),
            covariate.labels = c('Notas Base'),
            add.lines= c('Velocidade (tau)','Meia-Vida'),
            df=F,
            column.sep.width="1pt",
            align=TRUE)
  
  stargazer(absoluta_iniciais_br_pooled,
            absoluta_iniciais_ce_pooled,
            absoluta_finais_br_pooled,
            absoluta_finais_ce_pooled,
            title="Resultados da Regressão de Convergência Absoluta - Painel Pooled",
            dep.var.caption = c('Crescimento Anual'),
            column.labels = c('Anos Iniciais', 'Anos Finais'),
            column.separate = c(2,2),
            covariate.labels = c('Notas Base','(Intercepto)'),
            add.lines= c('Velocidade (tau)','Meia-Vida'),
            df=F,
            column.sep.width="1pt",
            align=TRUE)
  
  stargazer(condic_inicias_01,
            condic_inicias_02,
            condic_finais_01,
            condic_finais_02,
            title="Resultados da Regressão de Convergência Condicional - Painel Efeitos Fixos",
            dep.var.caption = c('Crescimento Anual'),
            column.labels = c('Anos Iniciais','Anos Finais'),
            column.separate = c(2,2),
            covariate.labels = c('Notas Base','Alunos por Turma','Nº de Funcionários','Ano 2009','Ano 2011','Ano 2013', 'Ano 2015'),
            add.lines= c('Velocidade (tau)','Meia-Vida'),
            df=F,
            column.sep.width="1pt",
            align=TRUE)
  
  stargazer(condic_inicias_01_pooled,
            condic_finais_01_pooled,
            title="Resultados da Regressão de Convergência Condicional - Painel Pooled",
            dep.var.caption = c('Crescimento Anual'),
            column.labels = c('Anos Iniciais', 'Anos Finais'),
            column.separate = c(1,1),
            covariate.labels = c('Notas Base','Alunos por Turma','Nº de Funcionários','(Intercepto)'),
            add.lines= c('Velocidade (tau)','Meia-Vida'),
            df=F,
            column.sep.width="1pt",
            align=TRUE)  
  
  
  
  
  
  
  
  
  
  
  
  
{
  "Tabela 1" <- xtable(summary(ce_inicial_tab))
  "Tabela 2" <- xtable(summary(ce_final_tab))
}

  stargazer(plm_w_absoluto_inicial)
  stargazer(plm_w_absoluto_final)
  stargazer(plm_w_condicional_inicial)
  stargazer(plm_w_condicional_final)

{
  par(mfrow=c(3,2))
  hist(ce_final_tab$`2005`, main = "IDEB 2005", col = "gray", xlab = "Notas", ylab = "Frequencia", xlim = c(0,10))
  hist(ce_final_tab$`2007`, main = "IDEB 2007", col = "gray", xlab = "Notas", ylab = "Frequencia", xlim = c(0,10))
  hist(ce_final_tab$`2009`, main = "IDEB 2009", col = "gray", xlab = "Notas", ylab = "Frequencia", xlim = c(0,10))
  hist(ce_final_tab$`2011`, main = "IDEB 2011", col = "gray", xlab = "Notas", ylab = "Frequencia", xlim = c(0,10))
  hist(ce_final_tab$`2013`, main = "IDEB 2013", col = "gray", xlab = "Notas", ylab = "Frequencia", xlim = c(0,10))
  hist(ce_final_tab$`2015`, main = "IDEB 2015", col = "gray", xlab = "Notas", ylab = "Frequencia", xlim = c(0,10))
}





## Carregar os arquivos DadosIDEB.xlsx, DadosIdebBrutos.xlsx e DadosIdebBrutosPainel no R.

## Transformando em dados em painel
{
  p_DadosIdebBrutosPainel <- pdata.frame(DadosIdebBrutosPainel, index=c("COD_ESC","ANO"), drop.index=TRUE, row.names=TRUE)
  View(p_DadosIdebBrutosPainel)
}

## Aggregate para estat?stica Descritiva:
{
  agg_por_ano <- aggregate(DadosIdebBrutosPainel$NOTA, list(ANO = DadosIdebBrutosPainel$ANO), mean)
  View(agg_por_ano)
}
# Gr?ficos de estat?stica descritiva.
{
  hist(DadosIdebBrutosPainel$NOTA, xlab = "Distribui??o das notas da amostra no Ideb", ylab = "Frequ?ncia", main = "Distribui??o das notas")
  panel.hist(DadosIdebBrutosPainel$NOTA)
  agg_por_ano
  barplot(agg_por_ano$x, names.arg =agg_por_ano$ano, ylab = "M?dia das Notas", xlab = "Ano do Exame", main = "M?dia das notas da amostra")
  text(0.7, 3.4, "3,02")
  text(1.9, 3.9, "3.35")
  text(3.1, 4.2, "3.84")
  text(4.3, 4.7, "4.23")
  text(5.5, 4.9, "4.49")
  text(6.7, 4.9, "5.05")
}

## Modelo de Converg?ncia com um ?nico cross-section 2005-2015 pelo pacote REAT usando o comando rca:
{
  Convergencia_beta_absoluta_e_sigma <- rca(DadosIDEB$`2005`, 1 , DadosIDEB$`2015`, 6, output = "all")
  Convergencia_beta_absoluta_e_sigma
  OLS_da_Convergencia_Absoluta <- lm(lnGROWTH ~ lnNOTA, data=DadosIDEB)
  OLS_da_Convergencia_Condicional <- lm(lnGROWTH ~ lnNOTA + NUM_FUNCIONARIOS + NUM_SALAS, data=DadosIDEB)
  summary(OLS_da_Convergencia_Absoluta)
  summary(OLS_da_Convergencia_Condicional)
}
{
  par(mfrow=c(2,2))
  plot(OLS_da_Convergencia_Absoluta)
}
{
  par(mfrow=c(2,2))
  plot(OLS_da_Convergencia_Condicional)
}


#Teste de Heterescedasticidade:
{
  bptest(OLS_da_Convergencia_Absoluta)
  bptest(OLS_da_Convergencia_Condicional)
}


# Tabelas das regress?es:

stargazer(OLS_da_Convergencia_Absoluta)
stargazer(OLS_da_Convergencia_Condicional)

## Modelo de converg?ncia em painel:
{
  IDEBwithin <- plm(lnGROWTH ~ log(NOTA) , data=p_DadosIdebBrutosPainel, model="within")
  IDEBwithin
}
{
  IDEBbetween <- plm(lnGROWTH ~ log(NOTA) , data=p_DadosIdebBrutosPainel, model="between")
  IDEBbetween
}

## Teste de Hausman: se o p-value <0,05 use o within(efeitos fixos)
# model pode ser pooling, within(fixo) e between(vari?vel).

  phtest(IDEBwithin, IDEBbetween)

