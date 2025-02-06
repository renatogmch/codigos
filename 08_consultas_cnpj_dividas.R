# Planilha das consultas
# Renato Gomes Chaves
# jan/2025

# Pacotes ======================================================================
{
  # install.packages('pacman')
  library(pacman)
  # Sys.unsetenv("GITHUB_PAT")
  # remotes::install_github("georgevbsantiago/qsacnpj")
  # Opcao pra o print ser completo de todas as colunas:
  p_load(tidyverse,
         remotes,
         qsacnpj,
         janitor,
         readxl,
         # writexl,
         openxlsx,
         here,
         foreach)
  options(dplyr.width = Inf)
}

# Carregar dados ===============================================================
{
  # script 07_database_final_cnpj.R
  # Filter pra cnpj de eleicao
  lista_estab_empresas <- read_rds(here('data/processed/lista_estab_empresas.rds')) %>% 
    filter(!str_detect(razao_social, '^ELEIC'))
  
  # script 02_dividas_criar_database.R
  # filter pra apenas devedor principal
  dividas_dau <- read_rds(here('data/processed/dividas_ativas_cnpj_norte_nordeste.rds')) %>% 
    filter(str_detect(tipo_devedor, 'PRINCIPAL'))
  
  # tabela de cnaes filtrados por belzinha no script 03_filtros_cnae.R
  filtro_belzinha <- read_rds(
    here('data/processed/filtro_cnae_belzinha.rds')) %>% 
    select(numero_cnae) %>% 
    filter(str_length(numero_cnae) == 7) %>%
    pull() %>% 
    paste(., collapse = '|^') %>% 
    paste0('^', .)
  
  # script 04_incentivos_sudene.R
  sudene <- read_rds(here('data/processed/incentivos_sudene_2013a2023.rds'))
  
  # script 05_incentivos_sudam.R
  sudam <- read_rds(here('data/processed/incentivos_sudam_2010a2023.rds'))
  
  # script 03_filtros_cnae.R, tabela com todos os cnaes e seus nomes
  cnae_subclasses <- read_rds(here('data/processed/cnae_subclasses.rds'))
}

# Filtro de CNPJ ===============================================================
{
  cnpj_consulta <- c('49145491000158')
  
  lista_estab_empresas %>% 
    filter(cnpj %in% cnpj_consulta)
  
  sudene %>% 
    filter(cnpj %in% cnpj_consulta)
  
  sudam %>% 
    filter(cnpj %in% cnpj_consulta)
  
  dividas_dau %>%
    filter(cnpj %in% cnpj_consulta) %>% 
    summarise(.by = receita_principal,
              total_por_divida = sum(valor_consolidado))
  
  rm(cnpj_consulta)
}
{
  nome_consulta <- c('OURO')
  
  lista_estab_empresas %>% 
    filter(cnpj %in% nome_fantasia |
           cnpj %in% razao_social)
  
  rm(nome_consulta)
}

# Filtros Gerais ===============================================================
# Precisa descomentar parte do codigo dependendo do tipo de busca

# # filtro_belzinha customizado
# filtro_belzinha <- paste(c('^55'),
#                          collapse = '|^')

# String para busca EXATA no %in%
consulta <- paste(c('JAIBA',
                    '^MANGA',
                    'MONTES CLAROS'),
                    collapse = '|')


{
  # Realizar a busca
  lista_estab_empresas %>% 
    
    # Filtrar municipios ou UF por match parcial de nome
    filter(str_detect(municipio, consulta)) %>%
    # filter(str_detect(uf, resultado)) %>%
    
    # Filtro de regime tributario
    filter(str_detect(tributacao_2023, 'REAL')) %>%
    # filter(str_detect(tributacao_2023, 'PRESUMIDO')) %>%
    # filter(str_detect(tributacao_2023, 'REAL|PRESUMIDO')) %>%
    
    # Criar coluna indicando se tem alguma divida em DAU
    mutate(status_divida = if_else(cnpj %in% dividas_dau$cnpj,
                                   'Possui Divida', 'Nada Consta')
    ) %>% 
    
    # Criar as colunas individuais pra cada CNAE secundario
    # mantendo a coluna original com todos cnaes secundarios juntos
    separate_wider_delim(cnae_secundaria,
                         cols_remove = F,
                         delim = ',',
                         names_sep = "_",
                         too_few = "align_start") %>%
    
    # prepara o nome e o local das colunas pra facilitar proximos passos
    mutate(cnae_secundaria = cnae_secundaria_cnae_secundaria,
           .keep = 'unused') %>% 
    mutate(cnae_secundaria_extra = cnae_principal,
           .after = cnae_principal) %>% 
    
    # # filtrar o filtro_belzinha de cnaes elegiveis pra incentivo
    # filter(if_any(starts_with('cnae_princ'),
    #               ~ str_detect(., filtro_belzinha))) %>%
    
    # Criar var de cnae_incentivavel
    mutate(across(starts_with('cnae_secundaria_'),
                  ~ replace(., !str_detect(., filtro_belzinha), NA))
    ) %>% 
    unite('cnae_incentivavel',
          contains('cnae_secundaria_'),
          sep = ', ',
          remove = T,
          na.rm = T) %>% 
    
    # realinhar ordem de cols
    relocate(c(status_incentivada,
               status_divida),
             .after = cnpj) %>%
    relocate(cnae_principal,
             .after = tributacao_2023) %>% 
    mutate(across(everything(),
                  ~ stringi::stri_trans_general(., "latin-ascii"))
           ) %>% 
    {. ->> resultado} %>% 
    print()
  
  # Incentivadas SUDENE
  sudene %>% 
    filter(cnpj %in% resultado$cnpj) %>%
    {. ->> inc_sudene} %>% 
    print()
  
  # Incentivadas SUDAM
  sudam %>% 
    filter(cnpj %in% resultado$cnpj) %>%
    {. ->> inc_sudam} %>% 
    print()
  
  # Filtros de cnae que tem direito ao incentivo
  cnae_subclasses %>% 
    filter(str_detect(numero_cnae, filtro_belzinha)) %>%
    select(-cnae_com_nome,
           nome_cnae = cnae) %>% 
    {. ->> resultado_cnae} %>% 
    print()
  
  # Dividas das empresas do filtro ativo
  dividas_dau %>%
    filter(cnpj %in% resultado$cnpj) %>%
    summarise(.by = c(cnpj,
                      nome_devedor,
                      receita_principal),
              acoes = n(),
              divida_total = sum(as.numeric(valor_consolidado))
    ) %>%
    arrange(desc(cnpj)) %>%
    {. ->> dau_cnpj} %>%
    print()
  
  # Dividas por tipo de dívida do filtro ativo
  dividas_dau %>%
    filter(cnpj %in% resultado$cnpj,) %>%
    summarise(.by = c(receita_principal),
              acoes = n(),
              divida_consolidada = sum(as.numeric(valor_consolidado))
    ) %>%
    arrange(desc(acoes)) %>%
    janitor::adorn_totals() %>%
    {. ->> dau_por_tipo} %>%
    print()
  
  # Relacao de divida sobre capital social
  dividas_dau %>% 
    filter(cnpj %in% resultado$cnpj) %>% 
    summarise(.by = cnpj,
              valor_total_consolidado = sum(valor_consolidado)) %>% 
    left_join(resultado, by = 'cnpj') %>% 
    mutate(capital_social = capital_social %>% 
             str_replace(',', '.') %>% 
             as.double(),
           divida_pelo_capsoc = if_else(valor_total_consolidado/capital_social == Inf,
                                   'CapSoc zero',
                                   paste0(
                                     round(valor_total_consolidado/capital_social*100,
                                           digits = 2), '%')
           )
    ) %>% 
    select(cnpj,
           valor_total_consolidado,
           capital_social,
           divida_pelo_capsoc) %>% 
    arrange(desc(valor_total_consolidado)) %>% 
    {. ->> dau_pelo_capsoc} %>%
    print()
  
  # Relação de empresas incentivadas na busca
  resultado %>%
    group_by(municipio) %>%
    summarise(total_empresas = n(),
              empresas_L_presumido = sum(str_detect(tributacao_2023,
                                                    'PRESUMIDO')),
              empresas_L_real = sum(str_detect(tributacao_2023,
                                                      'REAL')),
              empresas_incentivadas = sum(!str_detect(status_incentivada,
                                                      'Nao'))
              ) %>% 
    janitor::adorn_totals() %>%
    {. ->> breve_sumario} %>%
    print()
}

{
  # Exportar tabelas em xlsx
  list(breve_sumario,
       resultado,
       inc_sudene,
       inc_sudam,
       dau_cnpj,
       dau_por_tipo,
       dau_pelo_capsoc,
       resultado_cnae) %>% 
    setNames(c('Sumario',
               'Lista de Empresas',
               'Incentivadas SUDENE',
               'Incentivadas SUDAM',
               'Dividas por CNPJ',
               'Dividas por tipo',
               'Dividas Vs CapSoc',
               'Filtros CNAE'
               )) %>% 
    openxlsx::write.xlsx(here(paste0('Data/Consultas/consulta_',
                                     readline('Nome do Arquivo: '), '.xlsx')),
                         withFilter = c(F, T, T, T, T, T, T, T),
                         firstRow = c(F, T, T, T, T, T, T, T),
                         colWidths = 'auto'
    )

  rm(resultado,
     inc_sudene,
     inc_sudam,
     resultado_cnae,
     dau_cnpj,
     dau_por_tipo,
     dau_pelo_capsoc,
     breve_sumario)
}
