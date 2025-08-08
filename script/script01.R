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
#' Este é um script exploratório.s

# -------------------------------------------------------------------------
# Load packages

if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here, rvest, glue, chromote)

# -------------------------------------------------------------------------
# Input definition

genero <- "Habenaria"
especie <- "gracilis"
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

# função para extrair

# 'get_reflora' Function  

# load 'get_infoR' function
get_infoR <- function(page, selector, text = TRUE) {
  output <- page %>% rvest::html_elements(selector)
  if (length(output) == 0) {
    output <- NA_character_
  } else {
    if (text == TRUE) {
      output <- output %>% rvest::html_text()
    } else {
      output <- output %>% rvest::html_attr("href")
    }
    
    # Se retornar múltiplos valores, colapsa
    if (length(output) > 1) {
      output <- paste(output, collapse = "; ")
    }
  }
  return(output)
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# wfo link

wfo <- get_infoR(page = html_renderizado
                  ,selector = "span + a"
                  , text = F)

print(wfo) # imprimir

utils::browseURL(wfo) #test link

# Cities link (species plus)

cities <- get_infoR(page = html_renderizado
                   ,selector = "#linkCncFlora + a"
                   , text = F)

print(cities) # imprimir

utils::browseURL(cities) #test link

# CNCFlora link (não estou conseguindo por algum motivo)

cnnflor_link <- html_renderizado %>%
  html_elements("#linkCncFlora .svelte-kvvkhw") %>% 
  html_text()

print(cnnflor_link) # imprimir

utils::browseURL(cities_link) #test link

# sinonimos relevantes

rel_synms <- get_infoR(page = html_renderizado
                      ,selector = "#sinonimos-relevantes .content"
                      , text = T)

print(rel_synms) # imprimir

# forma de vida

life_form <- get_infoR(page = html_renderizado
                      ,selector = ".forma-de-vida"
                      ,text = T) %>% 
  stringr::str_remove("Forma de Vida ")

print(life_form) # imprimir

# substrato

substrate <- get_infoR(page = html_renderizado
                      ,selector = ".substrato"
                      ,text = T) %>% 
  stringr::str_remove("Substrato ")

print(substrate) # imprimir

# descrição controlada

ctrl_descrp <- get_infoR(page = html_renderizado
                        ,selector = "div:nth-child(12) div"
                        ,text = T)

print(ctrl_descrp) # imprimir

# descrição livre em português

free_descrp_pt <- get_infoR(page = html_renderizado
                           ,selector = "#descricao-livre-pt p"
                           ,text = T)

print(free_descrp_pt) # imprimir

# descrição livre em inglês (é uma opção)

free_descrp_en <- get_infoR(page = html_renderizado
                           ,selector = "#descricao-livre-en p"
                           ,text = T)

print(free_descrp_en) # imprimir

# comentário público

public_comm <- get_infoR(page = html_renderizado
                        ,selector = "#comentario-publico-pt"
                        ,text = T)

print(public_comm) # imprimir

# origem

origin <- get_infoR(page = html_renderizado
                   ,selector = "div:nth-child(20) div"
                   ,text = T)

print(origin) # imprimir

# endemismo

endmsm <- get_infoR(page = html_renderizado
                   ,selector = "div:nth-child(22) div"
                   ,text = T)

print(endmsm) # imprimir

# distribuicao (este daqui consegui pegar a distribuicao mesmo, mas faltam as outras)

distribution <- get_infoR(page = html_renderizado
                         ,selector = ".text"
                         ,text = T)

print(distribution) # imprimir

# Link to this taxon 

taxon_link <- get_infoR(page = html_renderizado
                       ,selector = "div:nth-child(26) a"
                       ,text = F)

print(taxon_link) # imprimir

# references

reference <- get_infoR(page = html_renderizado
                      ,selector = "div:nth-child(28) div"
                      ,text = T)

print(reference) # imprimir

# citation

citationp <- get_infoR(page = html_renderizado
                     ,selector = ".no-break-print .svelte-iqv7cw+ div"
                     ,text = T) %>% 
  purrr::discard(~ .x == "  ")

print(citationp) # imprimir

# -------------------------------------------------------------------------
# Juntar as informacoes num tibble

db_reflora <- tibble::tibble(
  sc_name = paste0(genero," ",especie)
  , genus = genero
  , species = especie
  , cities = cities
  , rel_synms = rel_synms
  , life_form = life_form
  , substrate = substrate
  , ctrl_descrp = ctrl_descrp
  , free_descrp_pt = free_descrp_pt
  , free_descrp_en = free_descrp_en
  , public_comm = public_comm
  , origin = origin
  , endmsm = endmsm
  , distribution = distribution
  , taxon_link = taxon_link
  , reference = reference
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
