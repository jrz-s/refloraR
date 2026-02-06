##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Iniciar o processo de extração dos traits da base de dados de 
#' ReFloraBrasil

# -------------------------------------------------------------------------
# Load packages
if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(here, tidyverse, janitor)

# -------------------------------------------------------------------------
# Load database

db <- load(file = here::here('database'
                             ,'orquidea'
                             ,'tidy_data'
                             ,'db_webscrp_id_synm.rda')) %>% 
  get() %>% 
  dplyr::select(!synonym) %>% 
  unique() %>% 
  dplyr::select(id, ctrl_descrp, free_descrp_pt)

rm("db_webscrp_id")

# -------------------------------------------------------------------------
# Database manipulation

## Estratégia 1

### Padronizar o texto:

# tudo em minúsculas

db <- db %>% 
  dplyr::mutate(ctrl_descrp = tolower(ctrl_descrp)) # minúscula

db <- db[c(1,9,10,14,19,20,42,48,57),c(1,2)]

# writexl::write_xlsx(db
#                     ,here::here('database'
#                                 ,'orquidea'
#                                 ,'tidy_data'
#                                 ,'teste_morfo.xlsx'))

db[db$id==11180,colnames(db)=="ctrl_descrp"]

# -------------------------------------------------------------------------

validar_descricao <- function(texto) {
  
  if (is.na(texto) || str_trim(texto) == "")
    return("sem_descricao")
  
  texto <- str_to_lower(texto)
  
  # padrões que indicam metadado / referência
  padrao_metadado <- c(
    "disponível em", "acesso em", "flora e funga",
    "jardim botânico", "world checklist",
    "\\(sp\\)", "s\\.n\\.", "herbário"
  )
  
  if (str_detect(texto, paste(padrao_metadado, collapse = "|")))
    return("metadado")
  
  # palavras-chave morfológicas
  palavras_morf <- c(
    "folha", "caule", "rizoma", "raiz", "flor", "fruto",
    "infloresc", "labelo", "sépal", "pétala"
  )
  
  tem_morfologia <- str_detect(texto, paste(palavras_morf, collapse = "|"))
  
  # medidas (inclui × e intervalos)
  tem_medidas <- stringr::str_detect(
    texto,
    "\\d+\\s*[×x\\-–]\\s*\\d+|\\d+\\s*(mm|cm|m)"
  )
  
  if (!tem_morfologia & !tem_medidas)
    return("texto_nao_morfologico")
  
  if (tem_morfologia & !tem_medidas)
    return("morfologia_sem_medidas")
  
  return("ok")
}

# -------------------------------------------------------------------------

db1 <- db %>% 
  dplyr::mutate(status = map_chr(ctrl_descrp, validar_descricao))


# -------------------------------------------------------------------------

limpar_repeticoes <- function(texto) {
  texto |>
    str_split(";") |>
    unlist() |>
    unique() |>
    paste(collapse = "; ")
}


db1 <- db %>% 
  dplyr::mutate(status = map_chr(ctrl_descrp, validar_descricao))


db2 <- db |>
  mutate(ctrl_descrp = map_chr(ctrl_descrp, limpar_repeticoes))

dados_quant <- db1 |> filter(status == "ok")
dados_qual  <- db1 |> filter(status %in% c("ok", "morfologia_sem_medidas"))
























# trocar vírgula decimal por ponto

# expandir abreviações (compr. → comprimento)

### Separar por órgãos:
  
# caule

# folha

# inflorescência

# flor

# fruto

### Usar regex direcionado por órgão

# Ex.: só buscar “cm comprimento” dentro do bloco “folha”














