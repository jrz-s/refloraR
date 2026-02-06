##################################################################
#' Costa, Vivian, Ecol.
#' Ecology | Undergraduate student
#' UFS
#' viviancosta507@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
# Nesse script estou criando uma grid com o shapefile da Caatinga;
# Adicionando os pontos de ocorrência das espécies;
# Analisando a riqueza em cada grid;
# Fazendo a matriz de presença e ausência;
# Juntando com os dados FRic e plotando.

# -------------------------------------------------------------------------
# Pacotes necessários
# install.packages("sf")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("ggplot2")
library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(viridis)
library(terra)
library(here)

# Ler shapefile da Caatinga
caatinga <- sf::st_read(here::here('shp','caatinga.shp'))
#caatinga <- st_read("shp/Municipios-Caatinga/Caatinga-Municipios.shp")
caatinga <- sf::st_transform(caatinga, crs = 4674)  # garantir CRS em graus
plot(caatinga)

# Criar grid de 0,5 grau
grid <- sf::st_make_grid(caatinga
                     ,cellsize = c(0.5, 0.5)
                     ,square = TRUE) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(cell_id = row_number())   # criar ID único para cada célula

plot(grid)

# Calcular interseção sem cortar a célula
# área total da célula
grid <- grid %>% mutate(area_total = as.numeric(st_area(.)))

# calcular a área de interseção com a Caatinga
intersec <- sf::st_intersection(grid, caatinga) %>%
  dplyr::mutate(area_intersec = as.numeric(st_area(.))) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(cell_id, area_intersec)

plot(intersec)

# juntar com a grid original
grid <- left_join(grid, intersec, by = "cell_id") %>%
  dplyr::mutate(area_intersec = ifelse(is.na(area_intersec)
                                       , 0
                                       , area_intersec)
                ,prop_area = area_intersec / area_total)


# Filtrar células com ≥ 50% da área dentro da Caatinga
grid_filtrada <- grid %>% filter(prop_area >= 0.5)

ggplot()+
  geom_sf(data = grid_filtrada)

# Ler pontos de ocorrência (Excel)
# A planilha precisa ter colunas "longitude" e "latitude"
# ocorrencias <- read_excel("database/db_caat_habitat_dummy.xlsx")

ocorrencias <- readxl::read_excel(path = here::here("database"
                                                    ,"orquidea"
                                                    ,"tidy_data"
                                                    ,"db_caat_habitat_dummy.xlsx"))

# Transformar em objeto sf
ocorrencias_sf <- sf::st_as_sf(ocorrencias
                           ,coords = c("long", "lat")
                           ,crs = 4674)

# Associar pontos à grid (saber em qual célula cada ponto caiu)
ocorrencias_com_grid <- sf::st_join(ocorrencias_sf
                                , grid_filtrada
                                , join = st_intersects)

# Riqueza e número de espécies distintas 
# Resumir por célula
ocorrencias_por_celula <- ocorrencias_com_grid %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(
    n_ocorrencias = n()                       # total de registros
    ,n_especies = n_distinct(sci_name))       # número de espécies distintas

# Juntar resultados com a grid
grid_filtrada <- grid_filtrada %>%
  dplyr::left_join(ocorrencias_por_celula, by = "cell_id") 

plot(grid_filtrada)

# Visualizar resultado (mapa com riqueza de espécies)
map_especies <- ggplot() +
  geom_sf(data = grid_filtrada, aes(fill = n_especies), color = "gray80") +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  geom_sf(data = grid_filtrada
          , aes(fill = n_especies)
          , color = "gray80") +
  #scale_fill_viridis_c(option = "brightgreen2", na.value = "white") + 
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  geom_sf(data = caatinga, fill = NA, color = "black") +
  #geom_sf(data = ocorrencias_sf, color = "red", size = 1) +
  theme_minimal() +
  labs(fill = "Nº de espécies")

# Visualizar resultado para número de ocorrências (registros)
map_especies <- ggplot() +
  
  geom_sf(data = grid_filtrada, aes(fill = n_ocorrencias), color = "gray80") +
  
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  
  geom_sf(data = grid_filtrada
          , aes(fill = n_ocorrencias)
          , color = "gray80") +
  
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  
  geom_sf(data = caatinga, fill = NA, color = "black") +
  
  theme_minimal() +
  
  labs(fill = "Nº de registros")

# Jutando FRic e plotando
FD.index <- read.csv(file = here::here("database"
                                       ,"orquidea"
                                       ,"tidy_data"
                                       ,"FD.incices.csv"))
#FD.index <- read.csv("FD.incices.csv")

library(dplyr)

grid_FRic <- grid_filtrada %>%
  left_join(FD.index, by = c("cell_id" = "Cell"))

map_FRic <- ggplot() +
  geom_sf(data = grid_FRic, aes(fill = FRic), color = "gray80") +
  scale_fill_viridis_c(option = "C", na.value = "white") +
  geom_sf(data = caatinga, fill = NA, color = "black", size = 0.6) +
  theme_minimal() +
  labs(fill = "FRic") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

library(grid)
library(gridExtra)

grid.arrange(map_especies, map_FRic, ncol = 2)

# Criar matriz de presença/ausência ---------------------------------------

# Organizando a coordenada da grid filtrada
grid_coords <- grid_filtrada %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(x = X, y = Y) %>%
  mutate(cell_id = grid_filtrada$cell_id)

# Selecionando as colunas necessárias e removendo duplicatas 
presenca <- ocorrencias_com_grid %>%
  st_drop_geometry() %>% 
  select(cell_id, sci_name) %>%
  distinct()

# Criando uma matriz de presença e ausência
matriz_pa <- presenca %>%
  mutate(presenca = 1) %>%
  tidyr::pivot_wider(
    names_from = sci_name,
    values_from = presenca,
    values_fill = list(presenca = 0)
  ) %>% arrange(cell_id) #ordenando por id da célula

# Juntar coordenadas à matriz de presença/ausência
matriz_pa <- matriz_pa %>%
  left_join(grid_coords, by = "cell_id") %>%
  relocate(x, y, .after = cell_id)  # coloca as coordenadas logo após o ID

# Visualizar as primeiras linhas
head(matriz_pa)

install.packages("writexl")
library(writexl)
write_xlsx(matriz_pa, "database/matriz_presenca_ausencia.xlsx")


plot(ocorrencias_por_celula$n_ocorrencias, ocorrencias_por_celula$n_especies)
