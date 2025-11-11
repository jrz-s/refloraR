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
# Analisando a riqueza em cada célula.

# -------------------------------------------------------------------------
# Pacotes necessários
install.packages("sf")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
library(sf)
library(dplyr)
library(readxl)
library(ggplot2)

# Ler shapefile da Caatinga
caatinga <- st_read("shp/caatinga.shp")
caatinga <- st_transform(caatinga, crs = 4674)  # garantir CRS em graus

# Criar grid de 0,5 grau
grid <- st_make_grid(caatinga,
                     cellsize = c(0.5, 0.5),
                     square = TRUE) %>%
  st_as_sf() %>%
  mutate(cell_id = row_number())   # criar ID único para cada célula

# Calcular interseção sem cortar a célula
# área total da célula
grid <- grid %>% mutate(area_total = as.numeric(st_area(.)))

# calcular a área de interseção com a Caatinga
intersec <- st_intersection(grid, caatinga) %>%
  mutate(area_intersec = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  select(cell_id, area_intersec)

# juntar com a grid original
grid <- left_join(grid, intersec, by = "cell_id") %>%
  mutate(area_intersec = ifelse(is.na(area_intersec), 0, area_intersec),
         prop_area = area_intersec / area_total)


# Filtrar células com ≥ 50% da área dentro da Caatinga
grid_filtrada <- grid %>% filter(prop_area >= 0.5)

ggplot()+
  geom_sf(data = grid_filtrada)

# Ler pontos de ocorrência (Excel)
# A planilha precisa ter colunas "longitude" e "latitude"
ocorrencias <- read_excel("database/db_caat_habitat_dummy.xlsx")

# Transformar em objeto sf
ocorrencias_sf <- st_as_sf(ocorrencias,
                           coords = c("long", "lat"),
                           crs = 4674)

# Associar pontos à grid (saber em qual célula cada ponto caiu)
ocorrencias_com_grid <- st_join(ocorrencias_sf, grid_filtrada, join = st_intersects)

# Riqueza e número de espécies distintas 
# Resumir por célula
ocorrencias_por_celula <- ocorrencias_com_grid %>%
  st_drop_geometry() %>%
  group_by(cell_id) %>%
  summarise(
    n_ocorrencias = n(),                   # total de registros
    n_especies = n_distinct(sci_name)       # número de espécies distintas
  )

# Juntar resultados com a grid
grid_filtrada <- grid_filtrada %>%
  left_join(ocorrencias_por_celula, by = "cell_id")


# Visualizar resultado (mapa com riqueza de espécies)
map_especies <- ggplot() +
  geom_sf(data = grid_filtrada, aes(fill = n_especies), color = "gray80") +
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  geom_sf(data = caatinga, fill = NA, color = "black") +
  #geom_sf(data = ocorrencias_sf, color = "red", size = 1) +
  theme_minimal() +
  labs(fill = "Nº de espécies")
