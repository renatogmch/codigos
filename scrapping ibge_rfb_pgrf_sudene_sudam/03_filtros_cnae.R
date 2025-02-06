# Criacao dos filtros de CNAE
# v0.1
# Renato Gomes Chaves
# Criação: 24/04/2024

# Pacotes ======================================================================
{
  # install.packages('pacman')
  library(pacman)
  # Opcao pra o print ser completo de todas as colunas:
  p_load(tidyverse,
         readxl,
         here,
         foreach,
         writexl)
  options(dplyr.width = Inf)
}

{
  # filtros indicados por belzinha em 05/2024
  read_excel('data/dados_ibge/FILTROS-BELZINHA-CNAE_Subclasses_2_3_Estrutura_Detalhada.xlsx',
             col_types = 'text',
             skip = 1,
             col_names = c('SECAO',
                           'DIVISAO',
                           'GRUPO',
                           'CLASSE',
                           'SUBCLASSE',
                           'cnae')
  ) %>%
    mutate(across(SECAO:SUBCLASSE, ~ str_remove_all(., "[\\.\\-\\/]")),
           across(everything(), ~ stringi::stri_trans_general(., "latin-ascii"))) %>% 
    unite('numero_cnae', SECAO:SUBCLASSE,
          remove = T,
          na.rm = T) %>% 
    write_rds(here('data/processed/filtro_cnae_belzinha.rds'))
  
  # Todos os CNAEs
  read_excel("data/dados_ibge/cnae_subclasses_2_3_Estrutura_Detalhada.xlsx",
             col_types = 'text',
             skip = 4,
             col_names = c('SECAO',
                           'DIVISAO',
                           'GRUPO',
                           'CLASSE',
                           'SUBCLASSE',
                           'cnae')
  ) %>%
    mutate(across(SECAO:SUBCLASSE, ~ str_remove_all(., "[\\.\\-\\/]")),
           across(everything(), ~ stringi::stri_trans_general(., "latin-ascii"))) %>% 
    unite('numero_cnae', SECAO:SUBCLASSE,
          remove = T,
          na.rm = T) %>% 
    mutate(cnae_com_nome = paste(numero_cnae, cnae, sep = ' - ')) %>% 
    write_rds(here('data/processed/cnae_subclasses.rds'))
}


# criar vetor com todos os cnaes por secao
{
  # vetores de filtro por divisao de cnae
  # a_agricultura
  a_agricultura <- paste0(c(
    '^0111301',	# Cultivo de arroz
    '^0111302',	# Cultivo de milho
    '^0111303',	# Cultivo de trigo
    '^0111399',	# Cultivo de outros cereais não especificados anteriormente
    '^0112101',	# Cultivo de algodão herbáceo
    '^0112102',	# Cultivo de juta
    '^0112199',	# Cultivo de outras fibras de lavoura temporária não especificadas anteriormente
    '^0113000',	# Cultivo de canadeaçúcar
    '^0115600',	# Cultivo de soja
    '^0116401',	# Cultivo de amendoim
    '^0116402',	# Cultivo de girassol
    '^0116403',	# Cultivo de mamona
    '^0116499',	# Cultivo de outras oleaginosas de lavoura temporária não especificadas anteriormente
    '^0119901',	# Cultivo de abacaxi
    '^0119902',	# Cultivo de alho
    '^0119903',	# Cultivo de batatainglesa
    '^0119904',	# Cultivo de cebola
    '^0119905',	# Cultivo de feijão
    '^0119906',	# Cultivo de mandioca
    '^0119907',	# Cultivo de melão
    '^0119908',	# Cultivo de melancia
    '^0119909',	# Cultivo de tomate rasteiro
    '^0119999',	# Cultivo de outras plantas de lavoura temporária não especificadas anteriormente
    '^0121101',	# Horticultura, exceto morango
    '^0121102',	# Cultivo de morango
    '^0122900',	# Cultivo de flores e plantas ornamentais
    '^0131800',	# Cultivo de laranja
    '^0132600',	# Cultivo de uva
    '^0133401',	# Cultivo de açaí
    '^0133402',	# Cultivo de banana
    '^0133403',	# Cultivo de caju
    '^0133404',	# Cultivo de cítricos, exceto laranja
    '^0133405',	# Cultivo de cocodabaía
    '^0133406',	# Cultivo de guaraná
    '^0133407',	# Cultivo de maçã
    '^0133408',	# Cultivo de mamão
    '^0133409',	# Cultivo de maracujá
    '^0133410',	# Cultivo de manga
    '^0133411',	# Cultivo de pêssego
    '^0133499',	# Cultivo de frutas de lavoura permanente não especificadas anteriormente
    '^0134200',	# Cultivo de café
    '^0135100',	# Cultivo de cacau
    '^0139301',	# Cultivo de chádaíndia
    '^0139302',	# Cultivo de ervamate
    '^0139303',	# Cultivo de pimentadoreino
    '^0139304',	# Cultivo de plantas para condimento, exceto pimentadoreino
    '^0139305',	# Cultivo de dendê
    '^0139306',	# Cultivo de seringueira
    '^0139399',	# Cultivo de outras plantas de lavoura permanente não especificadas anteriormente
    '^0141501',	# Produção de sementes certificadas, exceto de forrageiras para pasto
    '^0141502',	# Produção de sementes certificadas de forrageiras para formação de pasto
    '^0142300',	# Produção de mudas e outras formas de propagação vegetal, certificadas
    '^0210101',	# Cultivo de eucalipto
    '^0210102',	# Cultivo de acácianegra
    '^0210103',	# Cultivo de pinus
    '^0210104',	# Cultivo de teca
    '^0210105',	# Cultivo de espécies madeireiras, exceto eucalipto, acácianegra, pinus e teca
    '^0210106',	# Cultivo de mudas em viveiros florestais
    '^0210107',	# Extração de madeira em florestas plantadas
    '^0210108',	# Produção de carvão vegetal  florestas plantadas
    '^0210109',	# Produção de casca de acácianegra  florestas plantadas
    '^0210199',	# Produção de produtos não madeireiros não especificados anteriormente em florestas plantadas
    '^0220901',	# Extração de madeira em florestas nativas
    '^0220902',	# Produção de carvão vegetal  florestas nativas
    '^0220903',	# Coleta de castanhadopará em florestas nativas
    '^0220904',	# Coleta de látex em florestas nativas
    '^0220905',	# Coleta de palmito em florestas nativas
    '^0220906',	# Conservação de florestas nativas
    '^0220999',	# Coleta de produtos não madeireiros não especificados anteriormente em florestas nativas
    '^0230600',	# Atividades de apoio à produção florestal
    '^0311601',	# Pesca de peixes em água salgada
    '^0311602',	# Pesca de crustáceos e moluscos em água salgada
    '^0311603',	# Coleta de outros produtos marinhos
    '^0311604',	# Atividades de apoio à pesca em água salgada
    '^0312401',	# Pesca de peixes em água doce
    '^0312402',	# Pesca de crustáceos e moluscos em água doce
    '^0312403',	# Coleta de outros produtos aquáticos de água doce
    '^0312404',	# Atividades de apoio à pesca em água doce
    '^0321301',	# Criação de peixes em água salgada e salobra
    '^0321302',	# Criação de camarões em água salgada e salobra
    '^0321303',	# Criação de ostras e mexilhões em água salgada e salobra
    '^0321304',	# Criação de peixes ornamentais em água salgada e salobra
    '^0321305',	# Atividades de apoio à aquicultura em água salgada e salobra
    '^0321399',	# Cultivos e semicultivos da aquicultura em água salgada e salobra não especificados anteriormente
    '^0322101',	# Criação de peixes em água doce
    '^0322102',	# Criação de camarões em água doce
    '^0322103',	# Criação de ostras e mexilhões em água doce
    '^0322104',	# Criação de peixes ornamentais em água doce
    '^0322105',	# Ranicultura
    '^0322106',	# Criação de jacaré
    '^0322107',	# Atividades de apoio à aquicultura em água doce
    '^0322199'	# Cultivos e semicultivos da aquicultura em água doce não especificados anteriormente
  ), collapse = '|')
  
  # b_industria_extrativa
  b_industria_extrativa <- paste0(c(
    '^0500301',	# Extração de carvão mineral
    '^0500302',	# Beneficiamento de carvão mineral
    '^0600001',	# Extração de petróleo e gás natural
    '^0600002',	# Extração e beneficiamento de xisto
    '^0600003',	# Extração e beneficiamento de areias betuminosas
    '^0710301',	# Extração de minério de ferro
    '^0710302',	# Pelotização, sinterização e outros beneficiamentos de minério de ferro
    '^0721901',	# Extração de minério de alumínio
    '^0721902',	# Beneficiamento de minério de alumínio
    '^0722701',	# Extração de minério de estanho
    '^0722702',	# Beneficiamento de minério de estanho
    '^0723501',	# Extração de minério de manganês
    '^0723502',	# Beneficiamento de minério de manganês
    '^0724301',	# Extração de minério de metais preciosos
    '^0724302',	# Beneficiamento de minério de metais preciosos
    '^0725100',	# Extração de minerais radioativos
    '^0729401',	# Extração de minérios de nióbio e titânio
    '^0729402',	# Extração de minério de tungstênio
    '^0729403',	# Extração de minério de níquel
    '^0729404',	# Extração de minérios de cobre, chumbo, zinco e outros minerais metálicos não ferrosos não especificados anteriormente
    '^0729405',	# Beneficiamento de minérios de cobre, chumbo, zinco e outros minerais metálicos não ferrosos não especificados anteriormente
    '^0810001',	# Extração de ardósia e beneficiamento associado
    '^0810002',	# Extração de granito e beneficiamento associado
    '^0810003',	# Extração de mármore e beneficiamento associado
    '^0810004',	# Extração de calcário e dolomita e beneficiamento associado
    '^0810005',	# Extração de gesso e caulim
    '^0810006',	# Extração de areia, cascalho ou pedregulho e beneficiamento associado
    '^0810007',	# Extração de argila e beneficiamento associado
    '^0810008',	# Extração de saibro e beneficiamento associado
    '^0810009',	# Extração de basalto e beneficiamento associado
    '^0810010',	# Beneficiamento de gesso e caulim associado à extração
    '^0810099',	# Extração e britamento de pedras e outros materiais para construção e beneficiamento associado
    '^0891600',	# Extração de minerais para fabricação de adubos, fertilizantes e outros produtos químicos
    '^0892401',	# Extração de sal marinho
    '^0892402',	# Extração de salgema
    '^0892403',	# Refino e outros tratamentos do sal
    '^0893200',	# Extração de gemas (pedras preciosas e semipreciosas)
    '^0899101',	# Extração de grafita
    '^0899102',	# Extração de quartzo
    '^0899103',	# Extração de amianto
    '^0899199'	# Extração de outros minerais não metálicos não especificados anteriormente
  ), collapse = '|')
  
  # c_industria_de_transformacao
  c_industria_de_transformacao <- paste0(c(
    '^1011201',	# Frigorífico  abate de bovinos
    '^1011202',	# Frigorífico  abate de equinos
    '^1011203',	# Frigorífico  abate de ovinos e caprinos
    '^1011204',	# Frigorífico  abate de bufalinos
    '^1011205',	# Matadouro  abate de reses sob contrato, exceto abate de suínos
    '^1012101',	# Abate de aves
    '^1012102',	# Abate de pequenos animais
    '^1012103',	# Frigorífico  abate de suínos
    '^1012104',	# Matadouro  abate de suínos sob contrato
    '^1013901',	# Fabricação de produtos de carne
    '^1013902',	# Preparação de subprodutos do abate
    '^1020101',	# Preservação de peixes, crustáceos e moluscos
    '^1020102',	# Fabricação de conservas de peixes, crustáceos e moluscos
    '^1031700',	# Fabricação de conservas de frutas
    '^1032501',	# Fabricação de conservas de palmito
    '^1032599',	# Fabricação de conservas de legumes e outros vegetais, exceto palmito
    '^1033301',	# Fabricação de sucos concentrados de frutas, hortaliças e legumes
    '^1033302',	# Fabricação de sucos de frutas, hortaliças e legumes, exceto concentrados
    '^1041400',	# Fabricação de óleos vegetais em bruto, exceto óleo de milho
    '^1042200',	# Fabricação de óleos vegetais refinados, exceto óleo de milho
    '^1043100',	# Fabricação de margarina e outras gorduras vegetais e de óleos não comestíveis de animais
    '^1051100',	# Preparação do leite
    '^1052000',	# Fabricação de laticínios
    '^1053800',	# Fabricação de sorvetes e outros gelados comestíveis
    '^1061901',	# Beneficiamento de arroz
    '^1061902',	# Fabricação de produtos do arroz
    '^1062700',	# Moagem de trigo e fabricação de derivados
    '^1063500',	# Fabricação de farinha de mandioca e derivados
    '^1064300',	# Fabricação de farinha de milho e derivados, exceto óleos de milho
    '^1065101',	# Fabricação de amidos e féculas de vegetais
    '^1065102',	# Fabricação de óleo de milho em bruto
    '^1065103',	# Fabricação de óleo de milho refinado
    '^1066000',	# Fabricação de alimentos para animais
    '^1069400',	# Moagem e fabricação de produtos de origem vegetal não especificados anteriormente
    '^1071600',	# Fabricação de açúcar em bruto
    '^1072401',	# Fabricação de açúcar de cana refinado
    '^1072402',	# Fabricação de açúcar de cereais (dextrose) e de beterraba
    '^1081301',	# Beneficiamento de café
    '^1081302',	# Torrefação e moagem de café
    '^1082100',	# Fabricação de produtos à base de café
    '^1091101',	# Fabricação de produtos de panificação industrial
    '^1091102',	# Fabricação de produtos de padaria e confeitaria com predominância de produção própria
    '^1092900',	# Fabricação de biscoitos e bolachas
    '^1093701',	# Fabricação de produtos derivados do cacau e de chocolates
    '^1093702',	# Fabricação de frutas cristalizadas, balas e semelhantes
    '^1094500',	# Fabricação de massas alimentícias
    '^1095300',	# Fabricação de especiarias, molhos, temperos e condimentos
    '^1096100',	# Fabricação de alimentos e pratos prontos
    '^1099601',	# Fabricação de vinagres
    '^1099602',	# Fabricação de pósalimentícios
    '^1099603',	# Fabricação de fermentos e leveduras
    '^1099604',	# Fabricação de gelo comum
    '^1099605',	# Fabricação de produtos para infusão (chá, mate, etc.)
    '^1099606',	# Fabricação de adoçantes naturais e artificiais
    '^1099607',	# Fabricação de alimentos dietéticos e complementos alimentares
    '^1099699',	# Fabricação de outros produtos alimentícios não especificados anteriormente
    '^1111901',	# Fabricação de aguardente de canadeaçúcar
    '^1111902',	# Fabricação de outras aguardentes e bebidas destiladas
    '^1112700',	# Fabricação de vinho
    '^1113501',	# Fabricação de malte, inclusive malte uísque
    '^1113502',	# Fabricação de cervejas e chopes
    '^1121600',	# Fabricação de águas envasadas
    '^1122401',	# Fabricação de refrigerantes
    '^1122402',	# Fabricação de chá mate e outros chás prontos para consumo
    '^1122403',	# Fabricação de refrescos, xaropes e pós para refrescos, exceto refrescos de frutas
    '^1122404',	# Fabricação de bebidas isotônicas
    '^1122499',	# Fabricação de outras bebidas não alcoólicas não especificadas anteriormente
    '^1311100',	# Preparação e fiação de fibras de algodão
    '^1312000',	# Preparação e fiação de fibras têxteis naturais, exceto algodão
    '^1313800',	# Fiação de fibras artificiais e sintéticas
    '^1314600',	# Fabricação de linhas para costurar e bordar
    '^1321900',	# Tecelagem de fios de algodão
    '^1322700',	# Tecelagem de fios de fibras têxteis naturais, exceto algodão
    '^1323500',	# Tecelagem de fios de fibras artificiais e sintéticas
    '^1330800',	# Fabricação de tecidos de malha
    '^1340501',	# Estamparia e texturização em fios, tecidos, artefatos têxteis e peças do vestuário
    '^1340502',	# Alvejamento, tingimento e torção em fios, tecidos, artefatos têxteis e peças do vestuário
    '^1340599',	# Outros serviços de acabamento em fios, tecidos, artefatos têxteis e peças do vestuário
    '^1351100',	# Fabricação de artefatos têxteis para uso doméstico
    '^1352900',	# Fabricação de artefatos de tapeçaria
    '^1353700',	# Fabricação de artefatos de cordoaria
    '^1354500',	# Fabricação de tecidos especiais, inclusive artefatos
    '^1359600',	# Fabricação de outros produtos têxteis não especificados anteriormente
    '^1411801',	# Confecção de roupas íntimas
    '^1412601',	# Confecção de peças do vestuário, exceto roupas íntimas e as confeccionadas sob medida
    '^1412602',	# Confecção, sob medida, de peças do vestuário, exceto roupas íntimas
    '^1413401',	# Confecção de roupas profissionais, exceto sob medida
    '^1413402',	# Confecção, sob medida, de roupas profissionais
    '^1413403',	# Facção de roupas profissionais
    '^1414200',	# Fabricação de acessórios do vestuário, exceto para segurança e proteção
    '^1421500',	# Fabricação de meias
    '^1422300',	# Fabricação de artigos do vestuário, produzidos em malharias e tricotagens, exceto meias
    '^1510600',	# Curtimento e outras preparações de couro
    '^1521100',	# Fabricação de artigos para viagem, bolsas e semelhantes de qualquer material
    '^1529700',	# Fabricação de artefatos de couro não especificados anteriormente
    '^1531901',	# Fabricação de calçados de couro
    '^1531902',	# Acabamento de calçados de couro sob contrato
    '^1532700',	# Fabricação de tênis de qualquer material
    '^1533500',	# Fabricação de calçados de material sintético
    '^1539400',	# Fabricação de calçados de materiais não especificadosanteriormente
    '^1540800',	# Fabricação de partes para calçados, de qualquer material
    '^1610203',	# Serrarias com desdobramento de madeira em bruto
    '^1610204',	# Serrarias sem desdobramento de madeira em bruto  Resseragem
    '^1610205',	# Serviço de tratamento de madeira realizado sob contrato
    '^1621800',	# Fabricação de madeira laminada e de chapas de madeira compensada, prensada e aglomerada
    '^1622601',	# Fabricação de casas de madeira préfabricadas
    '^1622602',	# Fabricação de esquadrias de madeira e de peças de madeira para instalações industriais e comerciais
    '^1622699',	# Fabricação de outros artigos de carpintaria para construção
    '^1623400',	# Fabricação de artefatos de tanoaria e de embalagens de madeira
    '^1629301',	# Fabricação de artefatos diversos de madeira, exceto móveis
    '^1629302',	# Fabricação de artefatos diversos de cortiça, bambu, palha, vime e outros materiais trançados, exceto móveis
    '^1710900',	# Fabricação de celulose e outras pastas para a fabricação de papel
    '^1721400',	# Fabricação de papel
    '^1722200',	# Fabricação de cartolina e papelcartão
    '^1731100',	# Fabricação de embalagens de papel
    '^1732000',	# Fabricação de embalagens de cartolina e papelcartão
    '^1733800',	# Fabricação de chapas e de embalagens de papelão ondulado
    '^1741901',	# Fabricação de formulários contínuos
    '^1741902',	# Fabricação de produtos de papel, cartolina, papelcartão e papelão ondulado para uso comercial e de escritório
    '^1742701',	# Fabricação de fraldas descartáveis
    '^1742702',	# Fabricação de absorventes higiênicos
    '^1742799',	# Fabricação de produtos de papel para uso doméstico e higiênicosanitário não especificados anteriormente
    '^1749400',	# Fabricação de produtos de pastas celulósicas, papel, cartolina, papelcartão e papelão ondulado não especificados anteriormente
    '^1830003',	# Reprodução de software em qualquer suporte
    '^1910100',	# Coquerias
    '^1921700',	# Fabricação de produtos do refino de petróleo
    '^1922501',	# Formulação de combustíveis
    '^1922502',	# Rerrefino de óleos lubrificantes
    '^1922599',	# Fabricação de outros produtos derivados do petróleo, exceto produtos do refino
    '^1931400',	# Fabricação de álcool
    '^1932200',	# Fabricação de biocombustíveis, exceto álcool
    '^2011800',	# Fabricação de cloro e álcalis
    '^2012600',	# Fabricação de intermediários para fertilizantes
    '^2013401',	# Fabricação de adubos e fertilizantes organominerais
    '^2013402',	# Fabricação de adubos e fertilizantes, exceto organominerais
    '^2014200',	# Fabricação de gases industriais
    '^2019301',	# Elaboração de combustíveis nucleares
    '^2019399',	# Fabricação de outros produtos químicos inorgânicos não especificados anteriormente
    '^2021500',	# Fabricação de produtos petroquímicos básicos
    '^2022300',	# Fabricação de intermediários para plastificantes, resinas e fibras
    '^2029100',	# Fabricação de produtos químicos orgânicos não especificados anteriormente
    '^2031200',	# Fabricação de resinas termoplásticas
    '^2032100',	# Fabricação de resinas termofixas
    '^2033900',	# Fabricação de elastômeros
    '^2040100',	# Fabricação de fibras artificiais e sintéticas
    '^2051700',	# Fabricação de defensivos agrícolas
    '^2052500',	# Fabricação de desinfestantes domissanitários
    '^2061400',	# Fabricação de sabões e detergentes sintéticos
    '^2062200',	# Fabricação de produtos de limpeza e polimento
    '^2063100',	# Fabricação de cosméticos, produtos de perfumaria e de higiene pessoal
    '^2071100',	# Fabricação de tintas, vernizes, esmaltes e lacas
    '^2072000',	# Fabricação de tintas de impressão
    '^2073800',	# Fabricação de impermeabilizantes, solventes e produtos afins
    '^2091600',	# Fabricação de adesivos e selantes
    '^2092401',	# Fabricação de pólvoras, explosivos e detonantes
    '^2092402',	# Fabricação de artigos pirotécnicos
    '^2092403',	# Fabricação de fósforos de segurança
    '^2093200',	# Fabricação de aditivos de uso industrial
    '^2094100',	# Fabricação de catalisadores
    '^2099101',	# Fabricação de chapas, filmes, papéis e outros materiais e produtos químicos para fotografia
    '^2099199',	# Fabricação de outros produtos químicos não especificados anteriormente
    '^2110600',	# Fabricação de produtos farmoquímicos
    '^2121101',	# Fabricação de medicamentos alopáticos para uso humano
    '^2121102',	# Fabricação de medicamentos homeopáticos para uso humano
    '^2121103',	# Fabricação de medicamentos fitoterápicos para uso humano
    '^2123800',	# Fabricação de preparações farmacêuticas
    '^2211100',	# Fabricação de pneumáticos e de câmarasdear
    '^2212900',	# Reforma de pneumáticos usados
    '^2219600',	# Fabricação de artefatos de borracha não especificados anteriormente
    '^2221800',	# Fabricação de laminados planos e tubulares de material plástico
    '^2222600',	# Fabricação de embalagens de material plástico
    '^2223400',	# Fabricação de tubos e acessórios de material plástico para uso na construção
    '^2229301',	# Fabricação de artefatos de material plástico para uso pessoal e doméstico
    '^2229302',	# Fabricação de artefatos de material plástico para usos industriais
    '^2229303',	# Fabricação de artefatos de material plástico para uso na construção, exceto tubos e acessórios
    '^2229399',	# Fabricação de artefatos de material plástico para outros usos não especificados anteriormente
    '^2311700',	# Fabricação de vidro plano e de segurança
    '^2312500',	# Fabricação de embalagens de vidro
    '^2319200',	# Fabricação de artigos de vidro
    '^2320600',	# Fabricação de cimento
    '^2330301',	# Fabricação de estruturas prémoldadas de concreto armado, em série e sob encomenda
    '^2330302',	# Fabricação de artefatos de cimento para uso na construção
    '^2330303',	# Fabricação de artefatos de fibrocimento para uso na construção
    '^2330304',	# Fabricação de casas prémoldadas de concreto
    '^2330305',	# Preparação de massa de concreto e argamassa para construção
    '^2330399',	# Fabricação de outros artefatos e produtos de concreto, cimento, fibrocimento, gesso e materiais semelhantes
    '^2341900',	# Fabricação de produtos cerâmicos refratários
    '^2342701',	# Fabricação de azulejos e pisos
    '^2342702',	# Fabricação de artefatos de cerâmica e barro cozido para uso na construção, exceto azulejos e pisos
    '^2349401',	# Fabricação de material sanitário de cerâmica
    '^2349499',	# Fabricação de produtos cerâmicos não refratários não especificados anteriormente
    '^2391501',	# Britamento de pedras, exceto associado à extração
    '^2391502',	# Aparelhamento de pedras para construção, exceto associado à extração
    '^2391503',	# Aparelhamento de placas e execução de trabalhos em mármore, granito, ardósia e outras pedras
    '^2392300',	# Fabricação de cal e gesso
    '^2399101',	# Decoração, lapidação, gravação, vitrificação e outros trabalhos em cerâmica, louça, vidro e cristal
    '^2399102',	# Fabricação de abrasivos
    '^2399199',	# Fabricação de outros produtos de minerais não metálicos não especificados anteriormente
    '^2411300',	# Produção de ferrogusa
    '^2412100',	# Produção de ferroligas
    '^2421100',	# Produção de semiacabados de aço
    '^2422901',	# Produção de laminados planos de aço ao carbono, revestidos ou não
    '^2422902',	# Produção de laminados planos de aços especiais
    '^2423701',	# Produção de tubos de aço sem costura
    '^2423702',	# Produção de laminados longos de aço, exceto tubos
    '^2424501',	# Produção de arames de aço
    '^2424502',	# Produção de relaminados, trefilados e perfilados de aço, exceto arames
    '^2431800',	# Produção de tubos de aço com costura
    '^2439300',	# Produção de outros tubos de ferro e aço
    '^2441501',	# Produção de alumínio e suas ligas em formas primárias
    '^2441502',	# Produção de laminados de alumínio
    '^2442300',	# Metalurgia dos metais preciosos
    '^2443100',	# Metalurgia do cobre
    '^2449101',	# Produção de zinco em formas primárias
    '^2449102',	# Produção de laminados de zinco
    '^2449103',	# Fabricação de ânodos para galvanoplastia
    '^2449199',	# Metalurgia de outros metais não ferrosos e suas ligas não especificados anteriormente
    '^2451200',	# Fundição de ferro e aço
    '^2452100',	# Fundição de metais não ferrosos e suas ligas
    '^2511000',	# Fabricação de estruturas metálicas
    '^2512800',	# Fabricação de esquadrias de metal
    '^2513600',	# Fabricação de obras de caldeiraria pesada
    '^2521700',	# Fabricação de tanques, reservatórios metálicos e caldeiras para aquecimento central
    '^2522500',	# Fabricação de caldeiras geradoras de vapor, exceto para aquecimento central e para veículos
    '^2531401',	# Produção de forjados de aço
    '^2531402',	# Produção de forjados de metais não ferrosos e suas ligas
    '^2532201',	# Produção de artefatos estampados de metal
    '^2532202',	# Metalurgia do pó
    '^2539001',	# Serviços de usinagem, torneiria e solda
    '^2539002',	# Serviços de tratamento e revestimento em metais
    '^2541100',	# Fabricação de artigos de cutelaria
    '^2542000',	# Fabricação de artigos de serralheria, exceto esquadrias
    '^2543800',	# Fabricação de ferramentas
    '^2591800',	# Fabricação de embalagens metálicas
    '^2592601',	# Fabricação de produtos de trefilados de metal padronizados
    '^2592602',	# Fabricação de produtos de trefilados de metal, exceto padronizados
    '^2593400',	# Fabricação de artigos de metal para uso doméstico e pessoal
    '^2599399',	# Fabricação de outros produtos de metal não especificados anteriormente
    '^2610800',	# Fabricação de componentes eletrônicos
    '^2621300',	# Fabricação de equipamentos de informática
    '^2622100',	# Fabricação de periféricos para equipamentos de informática
    '^2631100',	# Fabricação de equipamentos transmissores de comunicação, peças e acessórios
    '^2632900',	# Fabricação de aparelhos telefônicos e de outros equipamentos de comunicação, peças e acessórios
    '^2640000',	# Fabricação de aparelhos de recepção, reprodução, gravação e amplificação de áudio e vídeo
    '^2651500',	# Fabricação de aparelhos e equipamentos de medida, teste e controle
    '^2652300',	# Fabricação de cronômetros e relógios
    '^2660400',	# Fabricação de aparelhos eletromédicos e eletroterapêuticos e equipamentos de irradiação
    '^2670101',	# Fabricação de equipamentos e instrumentos ópticos, peças e acessórios
    '^2670102',	# Fabricação de aparelhos fotográficos e cinematográficos, peças e acessórios
    '^2680900',	# Fabricação de mídias virgens, magnéticas e ópticas
    '^2710401',	# Fabricação de geradores de corrente contínua e alternada, peças e acessórios
    '^2710402',	# Fabricação de transformadores, indutores, conversores, sincronizadores e semelhantes, peças e acessórios
    '^2710403',	# Fabricação de motores elétricos, peças e acessórios
    '^2721000',	# Fabricação de pilhas, baterias e acumuladores elétricos, exceto para veículos automotores
    '^2722801',	# Fabricação de baterias e acumuladores para veículos automotores
    '^2731700',	# Fabricação de aparelhos e equipamentos para distribuição e controle de energia elétrica
    '^2732500',	# Fabricação de material elétrico para instalações em circuito de consumo
    '^2733300',	# Fabricação de fios, cabos e condutores elétricos isolados
    '^2740601',	# Fabricação de lâmpadas
    '^2740602',	# Fabricação de luminárias e outros equipamentos de iluminação
    '^2751100',	# Fabricação de fogões, refrigeradores e máquinas de lavar e secar para uso doméstico, peças e acessórios
    '^2759701',	# Fabricação de aparelhos elétricos de uso pessoal, peças e acessórios
    '^2759799',	# Fabricação de outros aparelhos eletrodomésticos não especificados anteriormente, peças e acessórios
    '^2790201',	# Fabricação de eletrodos, contatos e outros artigos de carvão e grafita para uso elétrico, eletroímãs e isoladores
    '^2790202',	# Fabricação de equipamentos para sinalização e alarme
    '^2790299',	# Fabricação de outros equipamentos e aparelhos elétricos não especificados anteriormente
    '^2811900',	# Fabricação de motores e turbinas, peças e acessórios, exceto para aviões e veículos rodoviários
    '^2812700',	# Fabricação de equipamentos hidráulicos e pneumáticos, peças e acessórios, exceto válvulas
    '^2813500',	# Fabricação de válvulas, registros e dispositivos semelhantes, peças e acessórios
    '^2814301',	# Fabricação de compressores para uso industrial, peças e acessórios
    '^2814302',	# Fabricação de compressores para uso não industrial, peças e acessórios
    '^2815101',	# Fabricação de rolamentos para fins industriais
    '^2815102',	# Fabricação de equipamentos de transmissão para fins industriais, exceto rolamentos
    '^2821601',	# Fabricação de fornos industriais, aparelhos e equipamentos não elétricos para instalações térmicas, peças e acessórios
    '^2821602',	# Fabricação de estufas e fornos elétricos para fins industriais, peças e acessórios
    '^2822401',	# Fabricação de máquinas, equipamentos e aparelhos para transporte e elevação de pessoas, peças e acessórios
    '^2822402',	# Fabricação de máquinas, equipamentos e aparelhos para transporte e elevação de cargas, peças e acessórios
    '^2823200',	# Fabricação de máquinas e aparelhos de refrigeração e ventilação para uso industrial e comercial, peças e acessórios
    '^2824101',	# Fabricação de aparelhos e equipamentos de ar condicionado para uso industrial
    '^2824102',	# Fabricação de aparelhos e equipamentos de ar condicionado para uso não industrial
    '^2825900',	# Fabricação de máquinas e equipamentos para saneamento básico e ambiental, peças e acessórios
    '^2829101',	# Fabricação de máquinas de escrever, calcular e outros equipamentos não eletrônicos para escritório, peças e acessórios
    '^2829199',	# Fabricação de outras máquinas e equipamentos de uso geral não especificados anteriormente, peças e acessórios
    '^2831300',	# Fabricação de tratores agrícolas, peças e acessórios
    '^2832100',	# Fabricação de equipamentos para irrigação agrícola, peças e acessórios
    '^2833000',	# Fabricação de máquinas e equipamentos para a agricultura e pecuária, peças e acessórios, exceto para irrigação
    '^2840200',	# Fabricação de máquinasferramenta, peças e acessórios
    '^2851800',	# Fabricação de máquinas e equipamentos para a prospecção e extração de petróleo, peças e acessórios
    '^2852600',	# Fabricação de outras máquinas e equipamentos para uso na extração mineral, peças e acessórios, exceto na extração de petróleo
    '^2853400',	# Fabricação de tratores, peças e acessórios, exceto agrícolas
    '^2854200',	# Fabricação de máquinas e equipamentos para terraplenagem, pavimentação e construção, peças e acessórios, exceto tratores
    '^2861500',	# Fabricação de máquinas para a indústria metalúrgica, peças e acessórios, exceto máquinasferramenta
    '^2862300',	# Fabricação de máquinas e equipamentos para as indústrias de alimentos, bebidas e fumo, peças e acessórios
    '^2863100',	# Fabricação de máquinas e equipamentos para a indústria têxtil, peças e acessórios
    '^2864000',	# Fabricação de máquinas e equipamentos para as indústrias do vestuário, do couro e de calçados, peças e acessórios
    '^2865800',	# Fabricação de máquinas e equipamentos para as indústrias de celulose, papel e papelão e artefatos, peças e acessórios
    '^2866600',	# Fabricação de máquinas e equipamentos para a indústria do plástico, peças e acessórios
    '^2869100',	# Fabricação de máquinas e equipamentos para uso industrial específico não especificados anteriormente, peças e acessórios
    '^2910701',	# Fabricação de automóveis, camionetas e utilitários
    '^2910702',	# Fabricação de chassis com motor para automóveis, camionetas e utilitários
    '^2910703',	# Fabricação de motores para automóveis, camionetas e utilitários
    '^2920401',	# Fabricação de caminhões e ônibus
    '^2920402',	# Fabricação de motores para caminhões e ônibus
    '^2930101',	# Fabricação de cabines, carrocerias e reboques para caminhões
    '^2930102',	# Fabricação de carrocerias para ônibus
    '^2930103',	# Fabricação de cabines, carrocerias e reboques para outros veículos automotores, exceto caminhões e ônibus
    '^2941700',	# Fabricação de peças e acessórios para o sistema motor de veículos automotores
    '^2942500',	# Fabricação de peças e acessórios para os sistemas de marcha e transmissão de veículos automotores
    '^2943300',	# Fabricação de peças e acessórios para o sistema de freios de veículos automotores
    '^2944100',	# Fabricação de peças e acessórios para o sistema de direção e suspensão de veículos automotores
    '^2945000',	# Fabricação de material elétrico e eletrônico para veículos automotores, exceto baterias
    '^2949201',	# Fabricação de bancos e estofados para veículos automotores
    '^2949299',	# Fabricação de outras peças e acessórios para veículos automotores não especificadas anteriormente
    '^2950600',	# Recondicionamento e recuperação de motores para veículos automotores
    '^3011301',	# Construção de embarcações de grande porte
    '^3011302',	# Construção de embarcações para uso comercial e para usos especiais, exceto de grande porte
    '^3012100',	# Construção de embarcações para esporte e lazer
    '^3031800',	# Fabricação de locomotivas, vagões e outros materiais rodantes
    '^3032600',	# Fabricação de peças e acessórios para veículos ferroviários
    '^3041500',	# Fabricação de aeronaves
    '^3042300',	# Fabricação de turbinas, motores e outros componentes e peças para aeronaves
    '^3050400',	# Fabricação de veículos militares de combate
    '^3091101',	# Fabricação de motocicletas
    '^3091102',	# Fabricação de peças e acessórios para motocicletas
    '^3092000',	# Fabricação de bicicletas e triciclos não motorizados, peças e acessórios
    '^3099700',	# Fabricação de equipamentos de transporte não especificados anteriormente
    '^3101200',	# Fabricação de móveis com predominância de madeira
    '^3102100',	# Fabricação de móveis com predominância de metal
    '^3103900',	# Fabricação de móveis de outros materiais, exceto madeira e metal
    '^3104700',	# Fabricação de colchões
    '^3211601',	# Lapidação de gemas
    '^3211602',	# Fabricação de artefatos de joalheria e ourivesaria
    '^3211603',	# Cunhagem de moedas e medalhas
    '^3212400',	# Fabricação de bijuterias e artefatos semelhantes
    '^3220500',	# Fabricação de instrumentos musicais, peças e acessórios
    '^3230200',	# Fabricação de artefatos para pesca e esporte
    '^3240001',	# Fabricação de jogos eletrônicos
    '^3240002',	# Fabricação de mesas de bilhar, de sinuca e acessórios não associada à locação
    '^3240003',	# Fabricação de mesas de bilhar, de sinuca e acessórios associada à locação
    '^3240099',	# Fabricação de outros brinquedos e jogos recreativos não especificados anteriormente
    '^3250701',	# Fabricação de instrumentos não eletrônicos e utensílios para uso médico, cirúrgico, odontológico e de laboratório
    '^3250702',	# Fabricação de mobiliário para uso médico, cirúrgico, odontológico e de laboratório
    '^3250703',	# Fabricação de aparelhos e utensílios para correção de defeitos físicos e aparelhos ortopédicos em geral sob encomenda
    '^3250704',	# Fabricação de aparelhos e utensílios para correção de defeitos físicos e aparelhos ortopédicos em geral, exceto sob encomenda
    '^3250705',	# Fabricação de materiais para medicina e odontologia
    '^3250707',	# Fabricação de artigos ópticos
    '^3291400',	# Fabricação de escovas, pincéis e vassouras
    '^3292201',	# Fabricação de roupas de proteção e segurança e resistentes a fogo
    '^3292202',	# Fabricação de equipamentos e acessórios para segurança pessoal e profissional
    '^3299001',	# Fabricação de guardachuvas e similares
    '^3299002',	# Fabricação de canetas, lápis e outros artigos para escritório
    '^3299003',	# Fabricação de letras, letreiros e placas de qualquer material, exceto luminosos
    '^3299004',	# Fabricação de painéis e letreiros luminosos
    '^3299005',	# Fabricação de aviamentos para costura
    '^3299006',	# Fabricação de velas, inclusive decorativas
    '^3299099'	# Fabricação de produtos diversos não especificados anteriormente
  ), collapse = '|')
  
  # d_eletricidade_e_gas
  d_eletricidade_e_gas <- paste0(c(
    '^3511501',	# Geração de energia elétrica
    '^3511502',	# Atividades de coordenação e controle da operação da geração e transmissão de energia elétrica
    '^3512300',	# Transmissão de energia elétrica
    '^3514000',	# Distribuição de energia elétrica
    '^3520401'	# Produção de gás; processamento de gás natural
  ), collapse = '|')
  
  # e_agua_esgoto_atividades_de_gestao
  e_agua_esgoto_atividades_de_gestao <- paste0(c(
    '^3600601',	# Captação, tratamento e distribuição de água
    '^3600602',	# Distribuição de água por caminhões
    '^3701100',	# Gestão de redes de esgoto
    '^3702900',	# Atividades relacionadas a esgoto, exceto a gestão de redes
    '^3839401',	# Usinas de compostagem
    '^3839499',	# Recuperação de materiais não especificados anteriormente
    '^3900500'	# Descontaminação e outros serviços de gestão de resíduos
  ), collapse = '|')
  
  # f_construcao
  f_construcao <- paste0(c(
    '^4221901',	# Construção de barragens e represas para geração de energia elétrica
    '^4221902',	# Construção de estações e redes de distribuição de energia elétrica
    '^4221904',	# Construção de estações e redes de telecomunicações
    '^4221905',	# Manutenção de estações e redes de telecomunicações
    '^4222701',	# Construção de redes de abastecimento de água, coleta de esgoto e construções correlatas, exceto obras de irrigação
    '^4222702',	# Obras de irrigação
    '^4223500',	# Construção de redes de transportes por dutos, exceto para água e esgoto
    '^4291000'	# Obras portuárias, marítimas e fluviais
  ), collapse = '|')
  
  # h_transporte_armazenagem_e_correio
  h_transporte_armazenagem_e_correio <- paste0(c(
    '^5211701',	# Armazéns gerais  emissão de warrant
    '^5211702',	# Guardamóveis
    '^5211799'	# Depósitos de mercadorias para terceiros, exceto armazéns gerais e guardamóveis
  ), collapse = '|')
  
  # i_alojamento_e_alimentacao
  i_alojamento_e_alimentacao <- paste0(c(
    '^5510801',	# Hotéis
    '^5510802'	# Aparthotéis
  ), collapse = '|')
  
  # j_informacao_e_comunicacao
  j_informacao_e_comunicacao <- paste0(c(
    '^6110801',	# Serviços de telefonia fixa comutada  STFC
    '^6110802',	# Serviços de redes de transporte de telecomunicações  SRTT
    '^6110803',	# Serviços de comunicação multimídia  SCM
    '^6110899',	# Serviços de telecomunicações por fio não especificados anteriormente
    '^6120501',	# Telefonia móvel celular
    '^6120502',	# Serviço móvel especializado  SME
    '^6120599',	# Serviços de telecomunicações sem fio não especificados anteriormente
    '^6130200',	# Telecomunicações por satélite
    '^6141800',	# Operadoras de televisão por assinatura por cabo
    '^6142600',	# Operadoras de televisão por assinatura por microondas
    '^6143400',	# Operadoras de televisão por assinatura por satélite
    '^6190601',	# Provedores de acesso às redes de comunicações
    '^6190602',	# Provedores de voz sobre protocolo Internet  VOIP
    '^6190699',	# Outras atividades de telecomunicações não especificadas anteriormente
    '^6201501',	# Desenvolvimento de programas de computador sob encomenda
    '^6201502',	# Web desing
    '^6202300',	# Desenvolvimento e licenciamento de programas de computador customizáveis
    '^6203100',	# Desenvolvimento e licenciamento de programas de computador não customizáveis
    '^6319400'	# Portais, provedores de conteúdo e outros serviços de informação na Internet
  ), collapse = '|')
}

# todos_cnae
todos_cnaes_sudene <- paste0(list(
  a_agricultura,
  b_industria_extrativa,
  c_industria_de_transformacao,
  d_eletricidade_e_gas,
  e_agua_esgoto_atividades_de_gestao,
  f_construcao,
  h_transporte_armazenagem_e_correio,
  i_alojamento_e_alimentacao,
  j_informacao_e_comunicacao
), collapse = '|')
