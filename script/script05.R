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

genero <- "Coppensia"
especie <- "flexuosum"
grupo <- "6"

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

# -------------------------------------------------------------------------
# 1. Filtrar a base de dados principal

# filtrou a base de dados principal com os dados de vivi
db_caat_principal <- pdb %>% dplyr::filter(species %in% pp$sci_name)

db_caat_principal$species %>% unique %>% length()

db_caat$sci_name %>% unique %>% length()

# base de dados vivi

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


pp <- db_caat %>% dplyr::select(sci_name) %>% 
  dplyr::group_by(sci_name) %>% 
  dplyr::count() %>% 
  dplyr::ungroup(sci_name)


## código provisional para verificar se os nomes estão batendo

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
# 2. Preparar o script para obter os dados via webscraping

df <- db_caat_principal %>% 
  dplyr::select("sci_name" = species,genus, "species" = specificEpithet)


{

# get data

orquidea_data <- get_reflora_info(genus = df$genus
                               ,species = df$species)

# save data

save(orquidea_data
     ,file = here::here('database'
                        ,'orquidea'
                        ,'tidy_data'
                        ,'orquidea_data.rda'))

writexl::write_xlsx(x = orquidea_data
                    ,path = here::here('database'
                                       ,'orquidea'
                                       ,'tidy_data'
                                       ,'orquidea_data.xlsx'))

  }

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
