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
  
  group <- as.character(group)
  
  # link structure
  link <- glue::glue(
    "https://floradobrasil.jbrj.gov.br/consulta/",
    "?grupo={URLencode(group)}",
    "&familia=null",
    "&genero={URLencode(genus)}",
    "&especie={URLencode(species)}",
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
    
    , wfo = get_infoR(page = html_renderizado
                      ,selector = "span + a"
                      , text = F)
    
    , cities = get_infoR(page = html_renderizado
                         ,selector = "#linkCncFlora + a"
                         , text = F)
    
    , rel_synms = get_infoR(page = html_renderizado
                            ,selector = "#sinonimos-relevantes .content"
                            , text = T)
    
    , life_form = get_infoR(page = html_renderizado
                            ,selector = ".forma-de-vida"
                            ,text = T) %>% 
      stringr::str_remove("Forma de Vida ")
    
    , substrate = get_infoR(page = html_renderizado
                            ,selector = ".substrato"
                            ,text = T) %>% 
      stringr::str_remove("Substrato ")
    
    , ctrl_descrp = get_infoR(page = html_renderizado
                              ,selector = "div:nth-child(12) div"
                              ,text = T)
    
    , free_descrp_pt = get_infoR(page = html_renderizado
                                 ,selector = "#descricao-livre-pt p"
                                 ,text = T)
    
    , free_descrp_en = get_infoR(page = html_renderizado
                                 ,selector = "#descricao-livre-en p"
                                 ,text = T)
    
    , public_comm = get_infoR(page = html_renderizado
                              ,selector = "#comentario-publico-pt"
                              ,text = T)
    
    , origin = get_infoR(page = html_renderizado
                         ,selector = "div:nth-child(20) div"
                         ,text = T)
    
    , endmsm = get_infoR(page = html_renderizado
                         ,selector = "div:nth-child(22) div"
                         ,text = T)
    
    , distribution = get_infoR(page = html_renderizado
                               ,selector = ".text"
                               ,text = T)
    
    , taxon_link = get_infoR(page = html_renderizado
                             ,selector = "div:nth-child(26) a"
                             ,text = F)
    
    , reference = get_infoR(page = html_renderizado
                            ,selector = "div:nth-child(28) div"
                            ,text = T)
    
    , citation = get_infoR(page = html_renderizado
                           ,selector = ".no-break-print .svelte-iqv7cw+ div"
                           ,text = T) %>% 
      purrr::discard(~ .x == "  ")
    
  )
  
  return(db)
  
}

# -------------------------------------------------------------------------
# 'get_reflora_info' Function  

get_reflora_info <- function(genus, species){
  
  sp <- tibble::tibble(
    genus = genus, species = species)
  
  db <- sp %>%
    purrr::pmap_dfr(~get_reflora(..1,..2))
  
  return(db)
  
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
