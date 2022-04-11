# SAEB BASE ALUNO_9EF 2019
# Informações das respostas ao questionário, à prova do SAEB e das proficiências dos alunos do 9º ano EF
# Renato Gomes Chaves
# Criacao: 13/12/21
# Ultima edicao: 09/01/2022

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
here('data/raw/SAEB/2019/DADOS/TS_ALUNO_9EF.csv') %>% 
  read_delim(',',
             # n_max = 100,
             escape_double = FALSE,
             trim_ws = TRUE,
             locale = locale(encoding = "ISO-8859-1",
                             decimal_mark = ','),
             # caso queira importar tudo como character
             col_types = cols(.default = "c")
  ) %>% 
  {. ->> saeb_2019_aluno_9ef}
# Renomear niveis dos fatores #################################################
# pegar os nomes das variaveis gerais
here('data/raw/SAEB/2019/DICIONÁRIO/Dicionario_Saeb_2019.xlsx') %>% 
  # pegar sheet pela posicao dela no arquivo
  # nao sei pq mas n achou a sheet por string de nome
  readxl::read_excel(sheet = 4,
                     skip = 2) %>% 
  select(variavel = `Variável`) %>% 
  drop_na() %>% 
  slice(2:80) %>% 
  mutate(variavel = str_remove(variavel, '[0-9]$')) %>% 
  {. ->> var_names}

# pegar os titulos das perguntas dos questionarios
here('data/raw/SAEB/2019/DICIONÁRIO/Dicionario_Saeb_2019.xlsx') %>% 
  # pegar sheet pela posicao dela no arquivo
  # nao sei pq mas n achou a sheet por string de nome
  readxl::read_excel(sheet = 4,
                     skip = 142) %>% 
  select(texto_perguntas = `Descrição (texto da pergunta)`) %>% 
  drop_na() %>%
  pull(texto_perguntas) %>% 
  {. ->> var_names_survey}

saeb_2019_aluno_9ef <- saeb_2019_aluno_9ef %>%
  mutate(ID_SAEB = NULL,
         ID_REGIAO = factor(ID_REGIAO,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c( 'Norte',
                                        'Nordeste',
                                        'Sudeste',
                                        'Sul',
                                        'Centro-Oeste')
         ),
         ID_UF = factor(ID_UF,
                        levels = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26,
                                   27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52,
                                   53), 
                        labels = c( 'RO', 'AC', 'AM', 'RR', 'PA', 'AP', 'TO', 'MA', 'PI',
                                    'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA', 'MG', 'ES',
                                    'RJ', 'SP', 'PR', 'SC', 'RS', 'MS', 'MT', 'GO', 'DF')
         ),
         ID_AREA = factor(ID_AREA,
                          levels = c(1, 2),
                          labels = c( 'Capital',
                                      'Interior')
         ),
         ID_DEPENDENCIA_ADM = factor(ID_DEPENDENCIA_ADM,
                                     levels = c(1,2,3,4),
                                     labels = c('Federal',
                                                'Estadual',
                                                'Municipal',
                                                'Privada')
         ),
         ID_LOCALIZACAO = factor(ID_LOCALIZACAO,
                                 levels = c(1, 2),
                                 labels = c('Urbana',
                                            'Rural')
         ),
         ID_SERIE = factor(ID_SERIE, levels = c(5, 9, 12, 13),
                           labels = c('5º ano do Ensino Fundamental',
                                      '9º ano do Ensino Fundamental',
                                      '3ª/4ª séries do Ensino Médio Tradicional',
                                      '3ª/4ª séries do Ensino Médio Integrado')
         ),
         IN_SITUACAO_CENSO = factor(IN_SITUACAO_CENSO, levels = c(0, 1),
                                    labels = c('Não consistente',
                                               'Consistente')
         ),
         IN_PREENCHIMENTO_LP = factor(IN_PREENCHIMENTO_LP,
                                      levels = c(0, 1),
                                      labels = c('Prova não preenchida',
                                                 'Prova preenchida'
                                      )
         ),
         IN_PREENCHIMENTO_MT = factor(IN_PREENCHIMENTO_MT,
                                      levels = c(0, 1),
                                      labels = c('Prova não preenchida',
                                                 'Prova preenchida'
                                      )
         ), 
         # cuidado com factor vazio:
         # Vazio - Não selecionado para a amostra de Ciencias Humanas
         IN_PREENCHIMENTO_CH = factor(IN_PREENCHIMENTO_CH,
                                      levels = c(0, 1),
                                      labels = c('Prova não preenchida',
                                                 'Prova preenchida'
                                      )
         ),
         # cuidado com factor vazio:
         # Vazio - Não selecionado para a amostra de Ciencias Naturais
         IN_PREENCHIMENTO_CN = factor(IN_PREENCHIMENTO_CN,
                                      levels = c(0, 1),
                                      labels = c('Prova não preenchida',
                                                 'Prova preenchida'
                                      )
         ),
         IN_PRESENCA_LP = factor(IN_PRESENCA_LP,
                                 levels = c(0, 1),
                                 labels = c('Ausente',
                                            'Presente'
                                 )
         ),
         IN_PRESENCA_MT = factor(IN_PRESENCA_MT,
                                 levels = c(0, 1),
                                 labels = c('Ausente',
                                            'Presente'
                                 )
         ),
         IN_PRESENCA_CH = factor(IN_PRESENCA_CH,
                                 levels = c(0, 1),
                                 labels = c('Ausente',
                                            'Presente'
                                 )
         ),
         IN_PRESENCA_CN = factor(IN_PRESENCA_CN,
                                 levels = c(0, 1),
                                 labels = c('Ausente',
                                            'Presente'
                                 )
         ),
         # daqui pra baixo foi adaptado do input que vem com os dados
         # troquei apenas as letras com acentos
         # varias variaves ficaram como caractere sendo numerico mesmo
         # Arquivo INPUT_R_TS_ALUNO_9EF.R
         ID_REGIAO = factor(ID_REGIAO, levels = c (1,2,3,4,5),
                            labels = c( 'Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro-Oeste')),
         ID_UF = factor(ID_UF, levels = c (11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
                        labels = c( 'RO', 'AC', 'AM', 'RR', 'PA', 'AP', 'TO', 'MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA', 'MG', 'ES', 'RJ', 'SP', 'PR', 'SC', 'RS', 'MS', 'MT', 'GO', 'DF')),
         ID_AREA = factor(ID_AREA, levels = c (1,2),
                          labels = c( 'Capital', 'Interior')),
         ID_DEPENDENCIA_ADM = factor(ID_DEPENDENCIA_ADM, levels = c (1,2,3,4),
                                     labels = c( 'Federal', 'Estadual', 'Municipal', 'Privada')),
         ID_LOCALIZACAO = factor(ID_LOCALIZACAO, levels = c (1,2),
                                 labels = c( 'Urbana', 'Rural')),
         ID_TURNO = factor(ID_TURNO, levels = c (1,2,3),
                           labels = c( 'Matutino', 'Vespertino', 'Noturno')),
         ID_SERIE = factor(ID_SERIE, levels = c (9),
                           labels = c( '9º ano Ensino Fundamental')),
         IN_SITUACAO_CENSO = factor(IN_SITUACAO_CENSO, levels = c (0,1),
                                    labels = c( 'Não consistente', 'Consistente')),
         IN_PREENCHIMENTO_LP = factor(IN_PREENCHIMENTO_LP, levels = c (0,1),
                                      labels = c( 'Prova não preenchida', 'Prova preenchida')),
         IN_PREENCHIMENTO_MT = factor(IN_PREENCHIMENTO_MT, levels = c (0,1),
                                      labels = c( 'Prova não preenchida', 'Prova preenchida')),
         IN_PREENCHIMENTO_CH = factor(IN_PREENCHIMENTO_CH, levels = c (0,1),
                                      labels = c( 'Prova não preenchida', 'Prova preenchida')),
         IN_PREENCHIMENTO_CN = factor(IN_PREENCHIMENTO_CN, levels = c (0,1),
                                      labels = c( 'Prova não preenchida', 'Prova preenchida')),
         IN_PRESENCA_LP = factor(IN_PRESENCA_LP, levels = c (0,1),
                                 labels = c( 'Ausente', 'Presente')),
         IN_PRESENCA_MT = factor(IN_PRESENCA_MT, levels = c (0,1),
                                 labels = c( 'Ausente', 'Presente')),
         IN_PRESENCA_CH = factor(IN_PRESENCA_CH, levels = c (0,1),
                                 labels = c( 'Ausente', 'Presente')),
         IN_PRESENCA_CN = factor(IN_PRESENCA_CN, levels = c (0,1),
                                 labels = c( 'Ausente', 'Presente')),
         CO_CONCEITO_Q1_CH = factor(CO_CONCEITO_Q1_CH, levels = c ('0','1','2','7','.'),
                                    labels = c( 'Nenhum crédito', 'Crédito parcial', 'Crédito total', 'Erros de impressão ou digitalização', 'Branco')),
         CO_CONCEITO_Q2_CH = factor(CO_CONCEITO_Q2_CH, levels = c ('0','1','2','7','.'),
                                    labels = c( 'Nenhum crédito', 'Crédito parcial', 'Crédito total', 'Erros de impressão ou digitalização', 'Branco')),
         CO_CONCEITO_Q1_CN = factor(CO_CONCEITO_Q1_CN, levels = c ('0','1','2','7','.'),
                                    labels = c( 'Nenhum crédito', 'Crédito parcial', 'Crédito total', 'Erros de impressão ou digitalização', 'Branco')),
         CO_CONCEITO_Q2_CN = factor(CO_CONCEITO_Q2_CN, levels = c ('0','1','2','7','.'),
                                    labels = c( 'Nenhum crédito', 'Crédito parcial', 'Crédito total', 'Erros de impressão ou digitalização', 'Branco')),
         IN_PROFICIENCIA_LP = factor(IN_PROFICIENCIA_LP, levels = c (0,1),
                                     labels = c( 'Não', 'Sim')),
         IN_PROFICIENCIA_MT = factor(IN_PROFICIENCIA_MT, levels = c (0,1),
                                     labels = c( 'Não', 'Sim')),
         IN_PROFICIENCIA_CH = factor(IN_PROFICIENCIA_CH, levels = c (0,1),
                                     labels = c( 'Não', 'Sim')),
         IN_PROFICIENCIA_CN = factor(IN_PROFICIENCIA_CN, levels = c (0,1),
                                     labels = c( 'Não', 'Sim')),
         IN_AMOSTRA = factor(IN_AMOSTRA, levels = c (0,1),
                             labels = c( 'Não' , 'Sim')),
         IN_PREENCHIMENTO_QUESTIONARIO = factor(IN_PREENCHIMENTO_QUESTIONARIO, levels = c (0,1),
                                                labels = c( 'Não preenchido', 'Preenchido parcial ou totalmente')),
         TX_RESP_Q001 = factor(TX_RESP_Q001, levels = c ('A','B','C','*','.'),
                              labels = c( 'Português.' , 'Espanhol.', 'Outra lingua.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q002 = factor(TX_RESP_Q002, levels = c ('A','B','C','D','E','F','*','.'),
                               labels = c( 'Branca.', 'Preta.', 'Parda.', 'Amarela.', 'Indígena.', 'Não quero declarar.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q003A = factor(TX_RESP_Q003A, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q003B = factor(TX_RESP_Q003B, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q003C = factor(TX_RESP_Q003C, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q003D = factor(TX_RESP_Q003D, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q003E = factor(TX_RESP_Q003E, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q004 = factor(TX_RESP_Q004, levels = c ('A','B','C','D','E','F','*','.'),
                               labels = c( 'Não completou o 5º ano do Ensino Fundamental.' , 'Ensino Fundamental, até o 5º ano.' , 'Ensino Fundamental completo.' , 'Ensino Médio completo.' , 'Ensino Superior completo (faculdade ou graduação).' , 'Não sei.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q005 = factor(TX_RESP_Q005, levels = c ('A','B','C','D','E','F','*','.'),
                               labels = c( 'Não completou o 5º ano do Ensino Fundamental.' , 'Ensino Fundamental, até o 5º ano.' , 'Ensino Fundamental completo.' , 'Ensino Médio completo.' , 'Ensino Superior completo (faculdade ou graduação).' , 'Não sei.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q006A = factor(TX_RESP_Q006A, levels = c ('A','B','C','*','.'),
                                labels = c( 'Nunca ou quase nunca.' , 'De vez em quando.' , 'Sempre ou quase sempre.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q006B = factor(TX_RESP_Q006B, levels = c ('A','B','C','*','.'),
                                labels = c( 'Nunca ou quase nunca.' , 'De vez em quando.' , 'Sempre ou quase sempre.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q006C = factor(TX_RESP_Q006C, levels = c ('A','B','C','*','.'),
                                labels = c( 'Nunca ou quase nunca.' , 'De vez em quando.' , 'Sempre ou quase sempre.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q006D = factor(TX_RESP_Q006D, levels = c ('A','B','C','*','.'),
                                labels = c( 'Nunca ou quase nunca.' , 'De vez em quando.' , 'Sempre ou quase sempre.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q006E = factor(TX_RESP_Q006E, levels = c ('A','B','C','*','.'),
                                labels = c( 'Nunca ou quase nunca.' , 'De vez em quando.' , 'Sempre ou quase sempre.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q007 = factor(TX_RESP_Q007, levels = c ('A','B','C','*','.'),
                               labels = c( 'Nunca ou quase nunca.' , 'De vez em quando (uma vez por semana, a cada quinze dias etc.).' , 'Sempre ou quase sempre (ex.: três ou mais dias por semana).', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q008A = factor(TX_RESP_Q008A, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q008B = factor(TX_RESP_Q008B, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q008C = factor(TX_RESP_Q008C, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q009A = factor(TX_RESP_Q009A, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Nenhum.' , '1.' , '2.' , '3 ou mais.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q009B = factor(TX_RESP_Q009B, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Nenhum.' , '1.' , '2.' , '3 ou mais.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q009C = factor(TX_RESP_Q009C, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Nenhum.' , '1.' , '2.' , '3 ou mais.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q009D = factor(TX_RESP_Q009D, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Nenhum.' , '1.' , '2.' , '3 ou mais.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q009E = factor(TX_RESP_Q009E, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Nenhum.' , '1.' , '2.' , '3 ou mais.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q009F = factor(TX_RESP_Q009F, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Nenhum.' , '1.' , '2.' , '3 ou mais.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q009G = factor(TX_RESP_Q009G, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Nenhum.' , '1.' , '2.' , '3 ou mais.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q010A = factor(TX_RESP_Q010A, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q010B = factor(TX_RESP_Q010B, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q010C = factor(TX_RESP_Q010C, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q010D = factor(TX_RESP_Q010D, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q010E = factor(TX_RESP_Q010E, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q010F = factor(TX_RESP_Q010F, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q010G = factor(TX_RESP_Q010G, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q010H = factor(TX_RESP_Q010H, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q010I = factor(TX_RESP_Q010I, levels = c ('A','B','*','.'),
                                labels = c( 'Não.', 'Sim.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q011 = factor(TX_RESP_Q011, levels = c ('A','B','C','*','.'),
                               labels = c( 'Menos de 30 minutos.' , 'Entre 30 minutos e uma hora.' , 'Mais de uma hora.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q012 = factor(TX_RESP_Q012, levels = c ('A','B','C','D','E','F','G','*','.'),
                               labels = c( 'À pé.' , 'De ônibus urbano.' , 'De transporte escolar.' , 'De barco.' , 'De bicicleta.', 'De carro.' , 'Outros meios de transporte.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q013 = factor(TX_RESP_Q013, levels = c ('A','B','C','D','*','.'),
                               labels = c(  '3 anos ou menos.' , '4 ou 5 anos.' , '6 ou 7 anos.' , '8 anos ou mais.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q014 = factor(TX_RESP_Q014, levels = c ('A','B','C','*','.'),
                               labels = c( 'Somente em escola pública.' , 'Somente em escola particular.' , 'Em escola pública e em escola particular.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q015 = factor(TX_RESP_Q015, levels = c ('A','B','C','*','.'),
                               labels = c( 'Não.' , 'Sim, uma vez.' , 'Sim, duas vezes ou mais.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q016 = factor(TX_RESP_Q016, levels = c ('A','B','C','*','.'),
                               labels = c( 'Nunca.' , 'Sim, uma vez.' , 'Sim, duas vezes ou mais.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q017A = factor(TX_RESP_Q017A, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Não uso meu tempo para isso.' , 'Menos de 1 hora.' , 'Entre 1 e 2 horas.' , 'Mais de 2 horas.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q017B = factor(TX_RESP_Q017B, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Não uso meu tempo para isso.' , 'Menos de 1 hora.' , 'Entre 1 e 2 horas.' , 'Mais de 2 horas.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q017C = factor(TX_RESP_Q017C, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Não uso meu tempo para isso.' , 'Menos de 1 hora.' , 'Entre 1 e 2 horas.' , 'Mais de 2 horas.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q017D = factor(TX_RESP_Q017D, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Não uso meu tempo para isso.' , 'Menos de 1 hora.' , 'Entre 1 e 2 horas.' , 'Mais de 2 horas.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q017E = factor(TX_RESP_Q017E, levels = c ('A','B','C','D','*','.'),
                                labels = c( 'Não uso meu tempo para isso.' , 'Menos de 1 hora.' , 'Entre 1 e 2 horas.' , 'Mais de 2 horas.', 'Dupla marcação.', 'Em branco.' )),
         TX_RESP_Q018A = factor(TX_RESP_Q018A, levels = c ('A','B','C','*','.'),
                                labels = c( 'Nunca ou quase nunca.' , 'De vez em quando.' , 'Sempre ou quase sempre.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q018B = factor(TX_RESP_Q018B, levels = c ('A','B','C','*','.'),
                                labels = c( 'Nunca ou quase nunca.' , 'De vez em quando.' , 'Sempre ou quase sempre.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q018C = factor(TX_RESP_Q018C, levels = c ('A','B','C','*','.'),
                                labels = c( 'Nunca ou quase nunca.' , 'De vez em quando.' , 'Sempre ou quase sempre.', 'Dupla marcação.', 'Em branco.')),
         TX_RESP_Q019 = factor(TX_RESP_Q019, levels = c ('A','B','C','D','*','.'),
                               labels = c( 'Somente continuar estudando.' , 'Somente trabalhar.' , 'Continuar estudando e trabalhar.', 'Ainda não sei.', 'Dupla marcação.', 'Em branco.')),
         )
