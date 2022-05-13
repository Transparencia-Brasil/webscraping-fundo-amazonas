
#' libs
library(httr)
library(xml2)
library(tidyverse)

# COLETA DE RESULTADOS DA BUSCA ------------------------------------------------

#' url base
base_url <- "http://www.fundoamazonia.gov.br/pt/carteira-de-projetos/busca"
i <- 1L

#' função para iterar nos resultados. Os resultados são divididos por páginas e é 
#' necessário iterar sobre as paginações para encontrar o limite
do_the_pagination <- function(i) {
  
  base_url <- str_glue("http://www.fundoamazonia.gov.br/pt/carteira-de-projetos/busca/index.html?reloaded&page={i}")

  # checa se existe o botão "próxima página"
  # Na última página esse botão não existe e o valor extraído é vazio
  nao_possui_paginacao <- base_url %>% 
    GET() %>% 
    content() %>% 
    xml_find_all('//a[@class="pagination-next"]') %>% 
    rlang::is_empty()
  
  tibble(
    base_url = base_url,
    nao_possui_paginacao = nao_possui_paginacao
  )

}

#' aqui eu busco por 'n' páginas diferentes mas o algoritmo vai retornar somente as 
#' páginas que de fato existem no site do Fundo Amazônia.
urls <- map_df(1:35, do_the_pagination) %>% 
  transmute(base_url, pg_valida = !lag(nao_possui_paginacao, default = FALSE)) %>% 
  filter(pg_valida)

#' Pega a página de cada contrato retornado nas páginas de busca
get_portfolio <- function(base_url) {
  Sys.sleep(.2)
  message(str_glue("get {base_url}"))
  base_url %>% GET() %>% content()
}

#' O título da resposta da busca
get_resp <- function(portfolio) {
  portfolio %>% 
    xml_find_all('//div[@class="project-search-resp"]') %>% 
    xml_text(trim = TRUE)
}

#' O link do contrato do projeto
get_resp_url <- function(portfolio) {
  portfolio %>% 
    xml_find_all('//div[@class="project-search-resp"]/a') %>% 
    xml_attr("href") %>% 
    paste0("http://www.fundoamazonia.gov.br", .)
}

#' O título do projeto
get_title <- function(portfolio) {
  portfolio %>% 
    xml_find_all('//div[@class="project-search-title"]') %>% 
    xml_text(trim = TRUE)
}

#' Um id do projeto que identifica área de atuação
get_type_1 <- function(portfolio) {
  portfolio %>% 
    xml_find_all('//div[@class="project-search-type"][1]') %>% 
    xml_text(trim = TRUE)
}

#' um campo complementar de id do projeto (nenhum projeto retornou valor nesse campo)
#' mas pode ser que algum novo projeto apareça com esse campo preenchido, então é melhor coletar
get_type_2 <- function(portfolio) {
  portfolio %>% 
    xml_find_all('//div[@class="project-search-type"][2]') %>% 
    xml_text(trim = TRUE)
}

#' campos: valor do projeto, valor do apoio, Estado de aplicação e situação do projeto
get_search <- function(portfolio, id_resultado) {
  
  value <- portfolio %>% 
    xml_find_all('//div[@class="project-search-value"]') %>% 
    xml_text(trim = TRUE) %>% 
    as_tibble_col(column_name = "value")
  desc <-   portfolio %>% 
    xml_find_all('//div[@class="project-search-desc"]') %>% 
    xml_text(trim = TRUE) %>% 
    as_tibble_col(column_name = "desc")
  search <- bind_cols(desc, value) %>% 
    pivot_wider(names_from = desc, values_from = value, values_fn = list) %>% 
    unnest(c(`Valor do projeto`, `Valor do apoio`, Estados, Situação)) %>% 
    mutate(id_resul = row_number()) %>% 
    filter(id_resul == id_resultado) %>% 
    select(-id_resul) %>% 
    janitor::clean_names()
    
  return(search)
  
}

#' converte valores monetários em campos númericos
limpa_campo_valor <- function(x) {
  x %>%
    str_remove_all("R\\$|\\.|\\s") %>%
    str_replace(",", ".") %>%
    str_squish() %>% 
    as.numeric()
}

#' coleta as buscas
urls_2 <- urls %>% 
  mutate(
    id_busca = 1:n(),
    portfolio = map(base_url, get_portfolio),
    resp = map(portfolio, get_resp),
    url_contrato = map(portfolio, get_resp_url),
    title = map(portfolio, get_title),
    type_1 = map(portfolio, get_type_1),
    type_2 = map(portfolio, get_type_2)
  ) %>% 
  unnest(c(resp, url_contrato, title, type_1, type_2)) %>%
  group_by(id_busca) %>% 
  mutate(id_resultado = row_number()) %>% 
  ungroup() %>% 
  mutate(
    aux = map2_dfr(portfolio, id_resultado, get_search)
  ) %>% 
  unnest(aux) %>% 
  select(id_busca, id_resultado, pg_valida, base_url, url_contrato,
         title, resp, type_1, type_2, estados, situacao,
         everything())

#' exporta as buscas para uma planilha que pode ser analisada facilmente por humanos
urls_2 %>%
  select(-portfolio, -pg_valida) %>% 
  mutate(across(starts_with("valor"), limpa_campo_valor)) %>% 
  googlesheets4::write_sheet(sheet = "Resultado das buscas",
                             ss = "https://docs.google.com/spreadsheets/d/115SsXwZZp_74qITVYn9Bm_Fkzfm4hWB4HPcQ-QJ-1Ek")
  
# DADOS GERAIS DOS CONTRATOS ---------------------------------------------------

#' Nesse ponto em diante vamos extrair os dados dos contratos que ficaram dentro do campo "portfolio"
#' Na página de cada contrato existe um quadro com dados básicos do contrato, no DOM essa div é referenciada
#' como "aside_tag". Vamos coletar os dados dela com essa função:
get_aside_tag <- function(u){
  u %>%
    GET() %>% 
    content() %>% 
    xml_find_all('//div[@class="filtro"]') %>% 
    as_list() %>% 
    map_df(enframe) %>% 
    filter(name != "") %>% 
    pivot_wider(names_from = name, values_from = value, values_fn = list) %>% 
    unnest(c(div, ul)) %>% 
    unnest(div) %>% 
    unnest(div) %>% 
    unnest(ul) %>% 
    filter(map_chr(ul,class) != "character") %>% 
    unnest(ul) %>% 
    transmute(titulo = str_remove_all(div, "\n(\t)*"),
              desc = str_remove_all(unlist(ul), "\n(\t)*"))
}

#' a coleta da 'aside_tag" retorna os campos:
#' - url_contrato,
#' - eixos
#' - locais
#' - natureza_do_responsavel
#' - situacao
#' - temas
#' - valor_do_apoio_dpo_fundo_amazonia
#' - valor_total_do_projeto
aside_tag <- urls_2 %>% 
  select(url_contrato) %>% 
  mutate(
    aside_tag = map(url_contrato, get_aside_tag)
  ) %>% 
  unnest(aside_tag) %>% 
  group_by(url_contrato, titulo) %>% 
  arrange(titulo, row_number()) %>% 
  mutate(titulo = str_glue("{titulo} {row_number()}")) %>%
  ungroup() %>% 
  pivot_wider(names_from = titulo, values_from = desc) %>% 
  janitor::clean_names() %>% 
  unite("eixos", eixos_1:eixos_4, sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  unite("locais", local_1:local_10, sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  unite("temas", temas_1:temas_3, sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  rename(
    natureza_do_responsavel = natureza_do_responsavel_1,
    situacao = situacao_1,
    valor_do_apoio_do_fundo_amazonia = valor_do_apoio_do_fundo_amazonia_1,
    valor_total_do_projeto = valor_total_do_projeto_1
  )

#' coletando o website do projeto contratado
get_website_projeto <- function(url_contrato) {
  url_contrato %>%
    GET() %>% 
    content() %>% 
    xml_find_all('//div/a[@class="btn btn-verde btn-small btn-website"]') %>% 
    xml_attr("href") %>% 
    str_squish()
}

#' Seção apresentação: campo de texto com descrição do projeto
get_tab1_apresentacao <- function(url_contrato) {
  names <- url_contrato %>% 
    GET() %>% 
    content() %>% 
    xml_find_all('//div[@id="tab1"]/h3') %>% 
    xml_text(trim = TRUE) %>% 
    as_tibble_col(column_name = "names")
  
  values <- url_contrato %>% 
    GET() %>% 
    content() %>% 
    xml_find_all('//div[@id="tab1"]/p') %>% 
    xml_text(trim = TRUE) %>% 
    as_tibble_col(column_name = "values") %>% 
    filter(values != "versão estática do mapa")
  
  bind_cols(names, values) %>% 
    pivot_wider(names_from = names, values_from = values) %>% 
    janitor::clean_names()
  
}

#' 'Seção contextualização', 'o projeto' e 'lógica de intervenção': campos de texto com descrição do projeto
rgx_descricao <- c(
  "CONTEXTUALIZAÇÃO", "O PROJETO", "LÓGICA DE INTERVENÇÃO",
  "Contextualização", "O Projeto", "Lógica de Intervenção") %>% 
  paste(collapse = "$|^") %>% 
  paste0("^", ., "$")

#' coletamos as seções 'Seção contextualização', 'o projeto' e 'lógica de intervenção'
get_tab2_descricao <- function(url_contrato) {
  url_contrato %>% 
    GET() %>% 
    content() %>% 
    xml_find_all('//div[@id="tab2"]/div') %>%
    xml_text(trim = TRUE) %>%
    str_split("\n") %>%
    unlist() %>%
    as_tibble_col() %>%
    mutate(
      names = if_else(str_detect(value, rgx_descricao), value, NA_character_)
    ) %>%
    fill(names) %>%
    filter(names != value) %>%
    select(names, value) %>%
    arrange(names) %>% 
    mutate(names = janitor::make_clean_names(names)) %>%
    pivot_wider(names_from = names, values_from = value)
}

#' Organiza as quebras de textos e parágrafos dos campos narrativos
contextualizacao <- aside_tag %>% select(url_contrato) %>% 
  mutate(contextualizacao = map_dfr(url_contrato, get_tab2_descricao)) %>% 
  unnest(contextualizacao) %>% 
  select(
    url_contrato,
    contextualizacao_1 = contextualizacao,
    contextualizacao_2,
    contextualizacao_3,
    contextualizacao_4,
    contextualizacao_5,
    contextualizacao_6,
    contextualizacao_7,
    contextualizacao_8,
    contextualizacao_9,
    contextualizacao_10,
    o_projeto_1 = o_projeto,
    o_projeto_2,
    o_projeto_3,
    o_projeto_4,
    o_projeto_5,
    o_projeto_6,
    o_projeto_7,
    o_projeto_8,
    o_projeto_9,
    o_projeto_10,
    o_projeto_11,
    o_projeto_12,
    o_projeto_13,
    logica_de_intervencao_1 = logica_de_intervencao,
    logica_de_intervencao_2,
    logica_de_intervencao_3,
    logica_de_intervencao_4,
    logica_de_intervencao_5,
    logica_de_intervencao_6,
    logica_de_intervencao_7
  ) %>% 
  unite("contextualizacao", contextualizacao_1:contextualizacao_10, sep = "\n\r", remove = TRUE, na.rm = TRUE) %>% 
  unite("o_projeto", o_projeto_1:o_projeto_13, sep = "\n\r", remove = TRUE, na.rm = TRUE) %>% 
  unite("logica_de_intervencao", logica_de_intervencao_1:logica_de_intervencao_7, remove = TRUE, na.rm = TRUE)

#' Tabela de evolução do contrato com:
#' - data de aprovação
#' - data de contratação
get_evolucao <- function(url_contrato) {
  
  tbls <- url_contrato %>% 
    GET() %>% 
    content() %>% 
    xml_find_all('//div[@id="tab3"]/table') %>%
    rvest::html_table() %>% 
    set_names(c("Evolução", "Desembolso"))
  
  tbls$Evolução <- tbls$Evolução %>% 
    set_names(c("names", "values")) %>% 
    pivot_wider(names_from = names, values_from = values) %>% 
    janitor::clean_names()
  
  return(tbls$Evolução)
  
}

#' Tabela de desembolsos do contrato com 
#' - Desembolso (numeração 1°, 2°, 3°, etc.)
#' - data_desembolso
#' - valor_desembolso
get_desembolso <- function(url_contrato) {
  
  tbls <- url_contrato %>% 
    GET() %>% 
    content() %>% 
    xml_find_all('//div[@id="tab3"]/table') %>%
    rvest::html_table() %>% 
    set_names(c("Evolução", "Desembolso"))
  
  tbls$Desembolso <- tbls$Desembolso %>% 
    set_names(c("desembolso", "data_desembolso", "valor_desembolso"))
  
  return(tbls$Desembolso)
  
}

#' prazo do desembolso, em meses
get_prazo_desembolso <- function(url_contrato) {
  url_contrato %>% 
    GET() %>% 
    content() %>% 
    xml_find_all('//div[@id="tab3"]/table[1]') %>% 
    xml_text(trim = T) %>% 
    gsub("(.+)(Prazo de desembolso.+$)", "\\2", .) %>% 
    str_extract("[0-9]+") %>% 
    paste("meses")
}

#' preparação dda tabla para exportação em planilha legível por humanos
#' inclui dados de evolução de contrato
evolucao <- aside_tag %>% 
  select(url_contrato) %>% 
  left_join(select(urls_2, url_contrato, titulo_projeto = title, situacao, valor_do_projeto, valor_do_apoio)) %>% 
  mutate(
    evolucao = map_dfr(url_contrato, get_evolucao)
  ) %>% 
  unnest(evolucao)

#' inclui dados de evolução dos desembolsos
evolucao_desembolso <- evolucao %>% 
  select(url_contrato) %>% 
  mutate(desembolsos = map(url_contrato, get_desembolso)) %>% 
  unnest(desembolsos) %>% 
  left_join(evolucao) %>% 
  mutate(across(starts_with("data"), lubridate::dmy),
         across(starts_with("valor"), limpa_campo_valor)) 

# inclui prazo de desembolsos, organiza colunas e exporta para planílha legível
evolucao_desembolso %>%
  mutate(prazo_desembolso = map_chr(url_contrato, get_prazo_desembolso)) %>% 
  select(
    url_contrato, titulo_projeto, 
    desembolso, 
    data_desembolso,
    prazo_desembolso,
    valor_desembolso,
    valor_total_do_projeto = valor_do_projeto,
    valor_total_do_apoio = valor_do_apoio,
    data_da_aprovacao_do_projeto = data_da_aprovacao,
    data_da_contratacao_do_projeto = data_da_contratacao,
    data_da_conclusao_do_projeto = data_da_conclusao,
    situacao
  ) %>% 
  googlesheets4::write_sheet(
    sheet = "Desembolsos2",
    ss = "https://docs.google.com/spreadsheets/d/115SsXwZZp_74qITVYn9Bm_Fkzfm4hWB4HPcQ-QJ-1Ek"
  )

#' guarda dados gerais do contrato em planilha legível
contrato_full <- aside_tag %>% 
  mutate(
    website_projeto = map_chr(url_contrato, get_website_projeto),
    apresentacao = map_dfr(url_contrato, get_tab1_apresentacao)
  ) %>% 
  unnest(apresentacao) %>% 
  left_join(contextualizacao)

contrato_full %>% 
  mutate(across(starts_with("valor"), limpa_campo_valor)) %>% 
  googlesheets4::write_sheet(
    sheet = "Dados dos contratos",
    ss = "https://docs.google.com/spreadsheets/d/115SsXwZZp_74qITVYn9Bm_Fkzfm4hWB4HPcQ-QJ-1Ek"
  )
