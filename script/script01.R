##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Função para obter informações via Webscraping 
#' dados da página Reflora Brasil
#' Importante: este script mostra o passo a passo.

# -------------------------------------------------------------------------
# Load packages

if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here, rvest, glue, chromote)

# -------------------------------------------------------------------------
# Input definition

genero <- "Cattleya"#"Myracrodruon"
especie <- "elongata"#"urundeuva"
grupo <- "6"

# -------------------------------------------------------------------------
# Protocol

# link structure
link <- glue::glue(
  "https://floradobrasil.jbrj.gov.br/consulta/",
  "?grupo={URLencode(grupo)}",
  "&familia=null",
  "&genero={URLencode(genero)}",
  "&especie={URLencode(especie)}",
  "&autor=",
  "&nomeVernaculo=",
  "&nomeCompleto=",
  "&formaVida=null",
  "&substrato=null",
  "&ocorreBrasil=QUALQUER",
  "&ocorrencia=OCORRE",
  "&endemismo=TODOS",
  "&origem=TODOS",
  "&regiao=QUALQUER",
  "&ilhaOceanica=32767",
  "&estado=QUALQUER",
  "&domFitogeograficos=QUALQUER",
  "&vegetacao=TODOS",
  "&mostrarAte=SUBESP_VAR",
  "&opcoesBusca=TODOS_OS_NOMES",
  "&loginUsuario=Visitante",
  "&senhaUsuario=",
  "&contexto=consulta-publica",
  "&pagina=1",
  "#CondicaoTaxonCP"
)

# link test
utils::browseURL(link) # carrega o link direitinho, inclusive trocando a espécie.

# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Abre o Chrome invisível e navega até o link

b <- ChromoteSession$new()
b$Page$navigate(link)
b$Page$loadEventFired()  # espera carregar a página

# Aguarda carregamento de scripts
Sys.sleep(5)

# Captura o HTML renderizado
html <- b$DOM$getDocument()
node <- b$DOM$getOuterHTML(nodeId = html$root$nodeId)
html_renderizado <- read_html(node$outerHTML)

# -------------------------------------------------------------------------
# Extração do conteúdo desejado

# wfo link

wfo_link <- html_renderizado %>%
  html_elements("span + a") %>%
  html_attr("href")

print(wfo_link) # imprimir

utils::browseURL(wfo_link) #test link

# Cities link (species plus)

cities_link <- html_renderizado %>%
  html_elements("#linkCncFlora + a") %>%
  html_attr("href")

print(cities_link) # imprimir

utils::browseURL(cities_link) #test link

# CNCFlora link (não estou conseguindo por algum motivo)

cnnflor_link <- html_renderizado %>%
  html_elements("#linkCncFlora .svelte-kvvkhw") %>% 
  html_text()

print(cnnflor_link) # imprimir

utils::browseURL(cities_link) #test link

# sinonimos relevantes

sino_rel <- html_renderizado %>%
  html_elements("#sinonimos-relevantes .content") %>%
  html_text2()

print(sino_rel) # imprimir

# forma de vida

fvida <- html_renderizado %>%
  html_elements(".forma-de-vida") %>%
  html_text2() %>% 
  stringr::str_remove("Forma de Vida\n")

print(fvida) # imprimir

# substrato

ssubs <- html_renderizado %>%
  html_elements(".substrato") %>%
  html_text2() %>% 
  stringr::str_remove("Substrato\n") # rupicola: vive em superfícies rochosas

print(ssubs) # imprimir

# descrição controlada

desc_controlled <- html_renderizado %>%
  html_elements("div:nth-child(12) div") %>%
  html_text2()

print(desc_controlled) # imprimir

# descrição livre em português

desc_free_pt <- html_renderizado %>%
  html_elements("#descricao-livre-pt p") %>%
  html_text2()

print(desc_free_pt) # imprimir

# descrição livre em inglês (é uma opção)

desc_free_en <- html_renderizado %>%
  html_elements("#descricao-livre-en p") %>%
  html_text2()

print(desc_free_en) # imprimir

# comentário público

pub_comm <- html_renderizado %>%
  html_elements("#comentario-publico-pt") %>%
  html_text2()

print(pub_comm) # imprimir

# origem

origen <- html_renderizado %>%
  html_elements("div:nth-child(20) div") %>%
  html_text2()

print(origen) # imprimir

# endemismo

endm <- html_renderizado %>%
  html_elements("div:nth-child(22) div") %>%
  html_text2()

print(endm) # imprimir

# # distribuicao (este daqui consegui pegar a distribuicao mesmo, mas faltam as outras)
# 
# distbn <- html_renderizado %>%
#   html_elements("#distribuicao .content") %>%
#   html_text2()

# Distribuicao (contempla: distribucao, dominios, tipo de vegetacao)
# depois  trabalhar nisso

distbn <- html_renderizado %>%
  html_elements(".text") %>%
  html_text2() 

print(distbn) # imprimir

# Link to this taxon 

link_taxon <- html_renderizado %>%
  html_elements("div:nth-child(26) a") %>%
  html_attr("href")

print(link_taxon) # imprimir

# references

references <- html_renderizado %>%
  html_elements("div:nth-child(28) div") %>%
  html_text2() 

print(references) # imprimir

# citation

citationp <- html_renderizado %>%
  html_elements(".no-break-print .svelte-iqv7cw+ div") %>%
  html_text2() %>% 
  purrr::discard(~ .x == "") # podemos utilizar: ".[1]"

print(citationp) # imprimir

# -------------------------------------------------------------------------
# Juntar as informacoes num tibble

db_reflora <- tibble::tibble(
  sc_name = paste0(genero," ",especie)
  , genus = genero
  , species = especie
  , cities = cities_link
  , rel_synms = sino_rel
  , life_form = fvida
  , substrate = ssubs
  , ctrl_descrp = desc_controlled
  , free_descrp_pt = desc_free_pt
  , free_descrp_en = ifelse(length(desc_free_en)==0,NA,"")
  , public_comm = pub_comm
  , origin = origen
  , endmsm = endm
  , distribution = distbn
  , taxon_link = link_taxon
  , reference = references
  , citation = citationp) 

# -------------------------------------------------------------------------

# imprimir
print(db_reflora)

# visualizar
View(db_reflora)

# salvar em binario
save(db_reflora,file = here::here("database",'db_reflora.rda'))

# salvar em excel

writexl::write_xlsx(x = db_reflora
                    ,path = here::here("database",'db_reflora.xlsx'))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
