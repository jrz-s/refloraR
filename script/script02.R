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
#' Importante: este script monta uma função para montar um base de dados
#' numa tabela data.frame ou tibble para poder exportar como excel.

# -------------------------------------------------------------------------
# Load packages

if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here, rvest, glue, chromote)

# -------------------------------------------------------------------------
# 'get_reflora' Function  

get_reflora <- function(genus, species, group = 6){
  
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
    "#CondicaoTaxonCP")
  
  # Open Chrome invisibly and navigate to the link
  b <- ChromoteSession$new()
  b$Page$navigate(link)
  b$Page$loadEventFired()  
  
  # Wait for scripts to load
  Sys.sleep(5)
  
  # Capture the rendered HTML
  html <- b$DOM$getDocument()
  node <- b$DOM$getOuterHTML(nodeId = html$root$nodeId)
  html_renderizado <- read_html(node$outerHTML)
  
  # Input manipulation
  
  genus <- paste0(genus %>% 
                    base::tolower() %>% 
                    stringr::str_sub(start = 1,end = 1) %>% 
                    base::toupper()
                  ,genus %>% 
                    base::tolower() %>% 
                    stringr::str_sub(start = 2,end = nchar(genus)))
  
  species <- species %>% 
    base::tolower()
  
   # Database constructions
  
  db <- tibble::tibble(
    sc_name = paste0(genus," ",species)
    , genus = genus
    , species = species
    , wfo = html_renderizado %>%
      html_elements("span + a") %>%
      html_attr("href")
    , cities = html_renderizado %>%
      html_elements("#linkCncFlora .svelte-kvvkhw") %>% 
      html_text()
    , rel_synms = html_renderizado %>%
      html_elements("#sinonimos-relevantes .content") %>%
      html_text2()
    , life_form = html_renderizado %>%
      html_elements(".forma-de-vida") %>%
      html_text2() %>% 
      stringr::str_remove("Forma de Vida\n")
    , substrate = html_renderizado %>%
      html_elements(".substrato") %>%
      html_text2() %>% 
      stringr::str_remove("Substrato\n")
    , ctrl_descrp = html_renderizado %>%
      html_elements("div:nth-child(12) div") %>%
      html_text2()
    , free_descrp_pt = html_renderizado %>%
      html_elements("#descricao-livre-pt p") %>%
      html_text2()
    # , free_descrp_en = html_renderizado %>%
    #   html_elements("#descricao-livre-en p") %>%
    #   html_text2()
    , public_comm = html_renderizado %>%
      html_elements("#comentario-publico-pt") %>%
      html_text2()
    , origin = html_renderizado %>%
      html_elements("div:nth-child(20) div") %>%
      html_text2()
    , endmsm = html_renderizado %>%
      html_elements("div:nth-child(22) div") %>%
      html_text2()
    , distribution = html_renderizado %>%
      html_elements(".text") %>%
      html_text2()
    , taxon_link = html_renderizado %>%
      html_elements("div:nth-child(26) a") %>%
      html_attr("href")
    , reference = html_renderizado %>%
      html_elements("div:nth-child(28) div") %>%
      html_text2() 
    , citation = html_renderizado %>%
      html_elements(".no-break-print .svelte-iqv7cw+ div") %>%
      html_text2() %>% 
      purrr::discard(~ .x == ""))
    
    return(db)
  
}

# -------------------------------------------------------------------------
# 'get_reflora_info' Function  

get_reflora_info <- function(genus, species, group = 6){
  
  sp <- tibble::tibble(
    genus = genus, species = species, group = group)
  
  db <- sp %>%
    purrr::pmap_dfr(~get_allometric(..1,..2,..3))
  
  return(db)
  
}

# -------------------------------------------------------------------------
# Input definition

genus <- "Cattleya"
#genus <- c("Cattleya", "Myracrodruon")
species <- "elongata"
#species <- c("elongata", "urundeuva")

# -------------------------------------------------------------------------
# Function testing

# get information on one species
get_reflora_info(genus = "Cattleya"
                 ,species = "elongata")

# get information on several species 
get_reflora_info(genus = c("Cattleya", "Myracrodruon")
                 ,species = c("elongata", "urundeuva"))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
