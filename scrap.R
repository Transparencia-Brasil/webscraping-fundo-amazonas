library(httr)
library(xml2)
library(tidyverse)

base_url <- "http://www.fundoamazonia.gov.br/pt/carteira-de-projetos/busca"
pagination <- str_glue("{base_url}/index.html?reloaded&page={33:35}")

portfolio <- GET(base_url) %>% 
  content()

nao_possui_paginacao <- function(portfolio) {
  
  portfolio %>% 
    xml_find_all('//a[@class="pagination-next"]') %>% 
    rlang::is_empty()

}

nao_possui_paginacao(portfolio)

resp <- portfolio %>% 
  xml_find_all('//div[@class="project-search-resp"]') %>% 
  xml_text(trim = TRUE)

resp_url <- portfolio %>% 
  xml_find_all('//div[@class="project-search-resp"]/a') %>% 
  xml_attr("href") %>% 
  paste0("http://www.fundoamazonia.gov.br", .)

title <- portfolio %>% 
  xml_find_all('//div[@class="project-search-title"]') %>% 
  xml_text(trim = TRUE)

type_1 <- portfolio %>% 
  xml_find_all('//div[@class="project-search-type"][1]') %>% 
  xml_text(trim = TRUE)

type_2 <- portfolio %>% 
  xml_find_all('//div[@class="project-search-type"][2]') %>% 
  xml_text(trim = TRUE)

url_contrato <- portfolio %>% 
  xml_find_all('//div[@class="icone-contrato"]/a') %>% 
  xml_attr("href") %>% 
  paste0("http://www.fundoamazonia.gov.br", .)

search_value <- portfolio %>% 
  xml_find_all('//div[@class="project-search-value"]') %>% 
  xml_text(trim = TRUE)

search_desc <- portfolio %>% 
  xml_find_all('//div[@class="project-search-desc"]') %>% 
  xml_text(trim = TRUE)

limpa_campo_valor <- function(x) {
  x %>% 
    str_remove_all("R\\$|\\.") %>%
    str_replace(",", ".") %>% 
    str_squish() %>% 
    as.numeric()
}

valor <- tibble(name = search_desc, value = search_value) %>% 
  pivot_wider(names_from = name, values_from = value, values_fn = list) %>% 
  unnest(c(`Valor do projeto`, `Valor do apoio`, Estados, Situação)) %>% 
  janitor::clean_names() %>% 
  mutate(across(starts_with("valor"), limpa_campo_valor))

a <- tibble(
  busca = project_search_resp,
  contrato = project_search_resp_url,
  titulo_projeto = project_search_title,
  tipo_1 = type_1,
  tipo_2 = type_2,
  url_contrato = url_contrato
  ) %>% 
  bind_cols(valor)

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

a %>% 
  mutate(
    aside_tag = map(contrato, get_aside_tag)
  ) %>% 
  select(titulo_projeto, aside_tag) %>%
  unnest(aside_tag) %>% 
  pivot_wider(names_from = titulo, values_from = desc, values_fn = list) %>% 
  unnest()
