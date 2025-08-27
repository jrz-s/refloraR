##################################################################
#' Costa, Vivian, Ecol.
#' Ecology | Undergraduate student
#' UFS
#' viviancosta507@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Neste script vamos a explorar os dados com clustering.
#' Vivi, lembre que este é um procedimento simples, ou seja,
#' compreenda isto como um processo exploratório. Beleza?

# -------------------------------------------------------------------------
# Load packages
if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here, janitor, vegan)

# -------------------------------------------------------------------------
# Load database Vivi

# base de dados principal
db <- readxl::read_xlsx(path = here::here("database"
                                          ,"orquidea"
                                          ,"tidy_data"
                                          ,"db_caat_habitat_dummy.xlsx")) %>% 
  dplyr::select(!c(lat,long)) # retiramos estas variáveis para criar a matrix

#' Obs. uma matriz não pode ter dados repetidos, viu

# -------------------------------------------------------------------------
# Exploramos a base de dados
dplyr::glimpse(db)
visdat::vis_dat(db)

# -------------------------------------------------------------------------
# Database manipulation (preparando a matrix)

# Aqui fazemos uma somatório de todas as espécies (ou seja, base de dados resumo)
# Aqui não precisamos espécies repetidas, só as espécies únicas.
# repare que a saída é um data.frame
 
db_data.frame <- db %>%
  dplyr::group_by(sci_name) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

# Aqui transformamos o data.frame em uma matrix mesmo.

db_matrix <- db_data.frame %>% 
  dplyr::mutate(sci_name = sci_name %>% 
                  stringr::str_replace_all(pattern = " ",replacement = "_")) %>% 
  tibble::column_to_rownames("sci_name") %>% 
  as.matrix()

# Obs. Como uma matrix não pode ter zeros, a gente deve retirá-los

# Retiramos colunas e linhas com somatória maior a zero 
db_matrix2 <- db_matrix[rowSums(db_matrix)>0,colSums(db_matrix)>0]

# Determinamos as distâncias
dis_orqui <- vegdist (db_matrix2, method = 'bray')  

# calculamos o dendrograma
dendro <- hclust(d = dis_orqui, method = "average")

#plotamos
plot(dendro,hang = -1,cex = 0.5)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
