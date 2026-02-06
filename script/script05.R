##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' 1. Filtrar a base de dados principal com a base de Vivian
#' 2. Preparar o script para obter por webscraping as informações 
#' da descrição controlada, livre e comentário públicos das espécies.
#' A base de dados 'orquidea_data.rda' é resultado do processo de webscraping.

# -------------------------------------------------------------------------
# Load packages

if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here, janitor)

# -------------------------------------------------------------------------
# Load 'get_reflora_info()' Function  

source(file = here::here("function","get_refloraR_info.R"))

# -------------------------------------------------------------------------

# Input definition

genero <- "Lippia"
especie <- "grata"
grupo <- "6"

identificar_link(genero = genero, especies = especie)

# -------------------------------------------------------------------------
# Protocol

identificar_link <- function(genero,especies, grupo = "6"){

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
utils::browseURL(link) #

}


# -------------------------------------------------------------------------
# Load database

# base de dados principal
pdb <- readRDS(file = here::here("database","393.417","CompleteBrazilianFlora.rds"))
head(pdb)

# -------------------------------------------------------------------------
# 1. Filtrar a base de dados principal

# 1.1 Carregar a base de dados da Vivi

db_caat <- readr::read_csv(file = here::here("database"
                                             ,"orquidea"
                                             ,'raw_data'
                                             ,"dados_gbif_Caatinga.csv")) %>% 
  tidyr::drop_na() %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(species = species %>% 
                  stringr::str_remove(pattern = paste0(genus," "))) %>% 
  dplyr::mutate(cond = ifelse(genus==species,TRUE,FALSE)) %>% 
  dplyr::filter(!cond == TRUE) %>% 
  dplyr::select(!cond) %>% 
  dplyr::mutate(sci_name = paste0(genus," ",species)) %>%
  dplyr::arrange(genus,species) %>% 
  dplyr::select(family,sci_name,genus,species,long,lat) %>% 
  dplyr::filter(!species %in% c("sp.","indet.")) %>% 
  dplyr::mutate(sci_name = recode(sci_name
                                  ,"Cattleya bahiensis" = "Hoffmannseggella bahiensis"
                                  ,"Coppensia flexuosum" = "Coppensia flexuosa"
                                  ,"Epidendrum avicule" = "Epidendrum avicula"
                                  ,"Epidendrum epidendroides" = "Epidendrum dendrobioides"
                                  ,"Epidendrum fruticosum" = "Epidendrum setiferum"
                                  ,"Epidendrum warrasii" = "Epidendrum warasii"
                                  #,verificar 'Habenaria longicorniculata' N = 231
                                  #,verificar 'Habenaria spanophytica' N = 248
                                  #, verificar 'Madisonia ianthina' N = 267
                                  #,verificar 'Maxillaria schlechteriana' N= 281
                                  #, verificar 'Peristylus whistler' N= 317
                                  #, verificar 'Stelis montserratii' N= 386
                                  #, verificar 'Stelis sclerophylla' N= 390
                                  ,"Hapalorchis lineata" = "Hapalorchis lineatus"
                                  )) %>% 
  dplyr::filter(!sci_name %in% c('Habenaria longicorniculata'
                                ,'Habenaria spanophytica'
                                ,'Madisonia ianthina'
                                ,'Maxillaria schlechteriana'
                                ,'Peristylus whistleri'
                                ,'Stelis montserratii'
                                ,'Stelis sclerophylla'))

# 1.2 Quantificamos o número de espécies únicas ocorrentes na Caatinga

pp <- db_caat %>% dplyr::select(sci_name) %>% 
  dplyr::group_by(sci_name) %>% 
  dplyr::count() %>% 
  dplyr::ungroup(sci_name)

# 1.3 Filtramos as espécies únicas na base de dados de reflora com as espécies únicas.

db_caat_principal <- pdb %>% # base de dados de reflora
  dplyr::filter(species %in% pp$sci_name) # processo de filtrar

# -------------------------------------------------------------------------
## código provisional para verificar se os nomes estão batendo

# db_caat_principal$species %>% unique %>% length() # verificar se o filtro funcionou
# db_caat$sci_name %>% unique %>% length()

# q1 <- db_caat_principal %>% 
#   dplyr::select("sci_name" = species) %>% 
#   tibble::as_tibble() %>% 
#   dplyr::mutate(principal = 1:nrow(.))
# 
# 
# q2 <- pp %>% 
#   dplyr::select(sci_name) %>% 
#   tibble::as_tibble() %>% 
#   dplyr::mutate(vivian = 1:nrow(.))
# 
# q2 %>% 
#   dplyr::left_join(q1,by = "sci_name") %>% 
#   dplyr::mutate(principal = ifelse(is.na(principal),"trocar",principal)) %>%
#   dplyr::filter(principal == 'trocar') %>% View()

# -------------------------------------------------------------------------
# Save tidy data

save(db_caat,file = here::here("database"
                               ,"orquidea"
                               ,"tidy_data"
                               ,"db_caat.rda"))

save(db_caat_principal,file = here::here("database"
                                         ,"orquidea"
                                         ,"tidy_data"
                                         ,"db_caat_principal.rda"))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# 2. Preparar o script para obter os dados via webscraping

# load database
load(file = here::here("database"
                       ,"orquidea"
                       ,"tidy_data"
                       ,"db_caat_principal.rda"))

# database manipulation
df <- db_caat_principal %>% 
  dplyr::select(genus, "species" = specificEpithet)

# input genus
genusp <- df$genus %>% unique

# list created
orquidea_list <- list()

# Loop

{
  
  for(i in 1:length(genusp)){
    
    # get data
    
    db_i <- df %>% 
      dplyr::filter(genus %in% genusp[i])
    
    orquidea_list[i] <- get_reflora_info(genus = db_i$genus
                                         ,species = db_i$species) %>% list()
    
  }
  
  names(orquidea_list) <- genusp
  
  # save data
  
  save(orquidea_list
       ,file = here::here('database'
                          ,'orquidea'
                          ,'tidy_data'
                          ,'orquidea_data.rda'))
  
  writexl::write_xlsx(x = orquidea_list
                      ,path = here::here('database'
                                         ,'orquidea'
                                         ,'tidy_data'
                                         ,'orquidea_data.xlsx'))
  
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
