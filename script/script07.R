##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' vamos escolher as variáveis da base de dados do Reflora Brasil
#' para espacializar com pacote florabr

# -------------------------------------------------------------------------
# Load database

library(florabr)

# ficar de olho nesta função
florabr::get_synonym(data = pdb
                     ,species = 'Cattleya elongata')

# base de dados principal
pdb <- readRDS(file = here::here("database","393.417","CompleteBrazilianFlora.rds"))

colnames(pdb)

pdb %>% 
  dplyr::select(id
                ,taxonRank
                ,group
                ,subgroup
                ,species # nome cientifico completo
                ,acceptedName
                ,scientificName
                ,acceptedNameUsage
                ,parentNameUsage #consultar
                ,higherClassification
                ,kingdom
                ,phylum
                ,class
                ,order
                ,family
                ,genus
                ,specificEpithet
                ,infraspecificEpithet #subespecie
                ,scientificNameAuthorship
                ,taxonomicStatus # usar como filtro
                ,vernacularName
                ,lifeForm
                ,habitat
                ,vegetation
                ,origin
                ,endemism
                ,biome
                ,states
                ,countryCode
                ,references
                ) %>% tail
  
  
  unlist %>% as.vector %>% unique

# -------------------------------------------------------------------------

  # carregamos a base de dados vivi
  
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
  
  # identifcamos as especies unicas
  pp <- db_caat %>% dplyr::select(sci_name) %>% 
    dplyr::group_by(sci_name) %>% 
    dplyr::count() %>% 
    dplyr::ungroup(sci_name)
  
  # filtramos a base de dados principa com as espécies únicas
  
  db_caat_principal <- pdb %>% 
    dplyr::filter(species %in% pp$sci_name) %>% 
    dplyr::select(id
                  ,taxonRank
                  ,group
                  ,subgroup
                  #,species # nome cientifico completo
                  #,scientificName
                  ,acceptedName
                  ,acceptedNameUsage
                  #,parentNameUsage #consultar
                  #,higherClassification
                  ,kingdom
                  ,phylum
                  ,class
                  ,order
                  ,family
                  ,genus
                  ,specificEpithet
                  #,infraspecificEpithet #subespecie
                  ,scientificNameAuthorship
                  ,taxonomicStatus # usar como filtro
                  ,vernacularName
                  ,lifeForm
                  ,habitat
                  #,vegetation
                  ,origin
                  ,endemism
                  ,biome
                  ,states
                  ,countryCode
                  #,references
    ) %>% dplyr::filter(taxonomicStatus == "Accepted") 
  
  
  # -------------------------------------------------------------------------
  # Save tidy data
  
  save(db_caat_principal,file = here::here("database"
                                 ,"orquidea"
                                 ,"tidy_data"
                                 ,"db_completa.rda")) 
  
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  