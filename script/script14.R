##################################################################
#' Costa, Vivian, Ecol.
#' Ecology | Undergraduate student
#' UFS
#' viviancosta507@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
# Nesse script foi criado um database com o nome das espécies e a quantidade 
# de ocorrência de cada uma 
# Foi feito um proportional area chart (gráfico de área proporcional)
# -------------------------------------------------------------------------

install.packages("readxl")
install.packages("sf")
library(readxl)
library(sf)

# Ler pontos de ocorrência (Excel)
# A planilha precisa ter colunas "longitude" e "latitude"
ocorrencias <- readxl::read_xlsx(path = here::here("database"
                                                   ,"orquidea"
                                                   ,"tidy_data"
                                                   ,"db_caat_habitat_dummy.xlsx"))

# Transformar em objeto sf
ocorrencias_sf <- st_as_sf(ocorrencias,
                           coords = c("long", "lat"),
                           crs = 4674)

#Criando o dataframe de ocorrência para cada espécie

n_spp <- aggregate(ocorrencias$sci_name, by = list(ocorrencias$sci_name), FUN = NROW)
names(n_spp) <- c("sci_name", "n_occs")
hist(n_spp$n_occs[n_spp$n_occs < 10], breaks = 10)

#Criando o proportional area chart

install.packages("dplyr")
install.packages("ggplot2")
install.packages("treemapify")
library(dplyr)
library(ggplot2)
library(treemapify)

# Agrupar por número exato de ocorrências
df_bins <- n_spp %>%
  group_by(n_occs) %>%
  summarise(num_species = n(), .groups = "drop") %>%
  arrange(n_occs)  # ordena pelo número de ocorrências

# Transformar em fator ordenado
df_bins$n_occs <- factor(df_bins$n_occs, levels = df_bins$n_occs)

# Gráfico treemap
ggplot(df_bins, aes(area = num_species,
                    fill = as.numeric(as.character(n_occs)),
                    label = paste0(n_occs, "\n", num_species, " = spp"))) +
                    #label = paste0(n_occs, " registros\n", num_species, " spp"))) +
  geom_treemap() +
  geom_treemap_text(colour = "black", place = "centre", grow = TRUE) +
  scale_fill_gradient(low = "#e6ffe6", high = "#66cc66") +
  labs(title = "Número de espécies por classe de ocorrências") +
  theme(legend.position = "none")

# salvar apenas em PDF
ggsave("treemap_verde.pdf", width = 12, height = 8, units = "in")


# -------------------------------------------------------------------------

#Outras opções de cores
scale_fill_gradient(low = "#e6f2ff", high = "#66b2ff")
scale_fill_gradient(low = "#f2f2f2", high = "#b3b3b3")