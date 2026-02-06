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
# -------------------------------------------------------------------------

text.test <- c("PT  Planta muito pequena, 3,25–6,76 mm compr. Raiz delgada, produzida ao
longo do rizoma. Rizoma 1,21–4,73 mm compr., delgado. Ramicaule 0,48–1,66 mm
compr., ascendente, 1 bainha. Folha 2,70–4,91 × 0,98–3,87 mm, prostrada,
crassa, orbicular a elíptica, ápice obtuso, mucronado, base arredondada.
Inflorescência 12,82–32,2 mm compr.; pedúnculo 11,33–27,04 mm compr., bráctea
abaixo do meio, 0,31–1,69 mm compr.; bráctea floral 0,62–1,24 mm compr.;
pedicelo e ovário 0,49 × 1,24 mm compr., filamento 0,53–1,52 mm compr.; sépalas
amarelo–esverdeadas, ocasionalmente vinosas, nervuras primárias vinosas, sépala
dorsal 3,33–5,67 × 0,59–1,16 mm, ereta, ovada, ápice obtuso, 3–nervada; sépalas
laterais 2,47–4,46 × 0,8–2,93 mm, ovadas, ápice obtuso, base profundamente
côncava, conadas até 2/3, 3–nervadas; pétalas coloridas como as sépalas,
2,47–4,77 × 0,36–0,81 mm, lanceoladas, ápice agudo; labelo vinoso com margens
amarelo–esverdeadas, 1,35–3,25 × 0,90–1,88 mm expandido, trilobado, base
côncava, lobo apical ovado a oblongo, ápice arredondado, lobos laterais próximo
ao meio, eretos, triangulares, o disco com par de calos mais abaixo regular
convergindo acima da base; coluna 1,36–2,40 mm compr., alada, antera ventral,
ápice cuculado, denticulado, pé bulboso.;")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# subfuncoes de A
normalizar_texto <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII")
}

remover_prefixos_editoriais <- function(x) {
  x |>
    stringr::str_remove("^\\s*(pt|pt\\.|pl|pt\\s*-)\\s+") 
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Passo A

segmentar_estruturas <- function(texto) {
  
  if (is.na(texto)) return(tibble())
  
  texto <- texto |>
    stringr::str_replace_all("\n", " ") |>
    stringr::str_squish()
  
  # fragmentação mínima (somente para leitura sequencial)
  partes <- texto |>
    stringr::str_split("[;\\.]") |>
    unlist() |>
    stringr::str_trim() |>
    stringr::str_remove("^,\\s*") |> #add 31.12.2025
    discard(~ .x == "") 
  
  estruturas_validas <- c(
    "planta", "raiz", "rizoma", "ramicaule", "folha",
    "infloresc", 
    "pedicelo", "filamento", # add 31.12.2025
    "fruto",
    "sepala", "petala", "labelo", "coluna"
  )
  
  detectar_estrutura <- function(x) {
    x_norm <- x |>
      normalizar_texto() |>
      remover_prefixos_editoriais()
    
    purrr::keep(
      estruturas_validas,
      ~ stringr::str_detect(x_norm, paste0("^", .x))
    ) |>
      first()
  }
  
  segmentos <- list()
  estrutura_atual <- NULL
  texto_atual <- NULL
  
  for (p in partes) {
    
    estrutura <- detectar_estrutura(p)
    
    if (!is.null(estrutura)) {
      # fecha segmento anterior
      if (!is.null(estrutura_atual)) {
        segmentos <- append(segmentos, list(
          tibble(
            estrutura = estrutura_atual,
            segmento = texto_atual
          )
        ))
      }
      # inicia novo
      estrutura_atual <- estrutura
      texto_atual <- p
      
    } else {
      # continuação obrigatória
      texto_atual <- paste(texto_atual, p, sep = "; ")
    }
  }
  
  # fecha último
  if (!is.null(estrutura_atual)) {
    segmentos <- append(segmentos, list(
      tibble(
        estrutura = estrutura_atual,
        segmento = texto_atual
      )
    ))
  }
  
  bind_rows(segmentos) |>
    mutate(segmento_id = row_number()) |>
    select(segmento_id, estrutura, segmento)
}

# -------------------------------------------------------------------------
#PASSO B1 — Resolver continuidade (fill)
passo_B1_fill <- function(df) {
  
  df |>
    tidyr::fill(estrutura, .direction = "down")
}

# -------------------------------------------------------------------------
# PASSO B2 — Estrutura principal correta

passo_B2_estrutura_principal <- function(df) {
  
  df |>
    mutate(
      estrutura_principal = case_when(
        estrutura %in% c("pedicelo", "filamento", # add 31.12.2025
          "sepala", "petala", "labelo", "coluna") ~ "flor",
        TRUE ~ estrutura
      )
    )
}

# -------------------------------------------------------------------------
# PASSO B3 — Identificar subestruturas
passo_B3_subestrutura <- function(df) {
  
  df |>
    mutate(
      txt = normalizar_texto(segmento),
      
      subestrutura = case_when(
        
        # Inflorescência
        estrutura_principal == "infloresc" & str_detect(txt, "peduncul") ~ "pedunculo",
        estrutura_principal == "infloresc" & str_detect(txt, "bractea")  ~ "bractea",
        
        # Flor
        estrutura_principal == "flor" & str_detect(txt, "pedicelo") ~ "pedicelo",
        estrutura_principal == "flor" & str_detect(txt, "ovario")   ~ "ovario",
        estrutura_principal == "flor" & str_detect(txt, "filamento") ~ "filamento",
        estrutura_principal == "flor" & str_detect(txt, "sepala")   ~ "sepala",
        estrutura_principal == "flor" & str_detect(txt, "petala")   ~ "petala",
        estrutura_principal == "flor" & str_detect(txt, "labelo")   ~ "labelo",
        estrutura_principal == "flor" & str_detect(txt, "coluna")   ~ "coluna",
        
        TRUE ~ NA_character_
      )
    ) |>
    select(-txt)
}

# -------------------------------------------------------------------------
# Passo B completo

passo_B_completo <- function(df) {
  
  df |>
    passo_B1_fill() |>
    passo_B2_estrutura_principal() |>
    passo_B3_subestrutura()
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Passo C1: Extração de atributos quantitativos

extrair_quantitativos_segmento <- function(texto) {
  
  if (is.na(texto)) {
    return(tibble(
      trait_type = character(),
      trait = character(),
      min = numeric(),
      max = numeric(),
      value = numeric(),
      unit = character()
    ))
  }
  
  texto <- texto |>
    stringr::str_replace_all(",", ".") |>
    stringr::str_replace_all("–|—", "-") |>
    stringr::str_replace_all(" × ", "-") # add. 31.12.2025
  
  padrao <- stringr::regex(
    "(?<min>\\d+(?:\\.\\d+)?)\\s*
     (?:-|x)?\\s*
     (?<max>\\d+(?:\\.\\d+)?)?\\s*
     (?<unit>mm|cm|m)",
    ignore_case = TRUE,
    comments = TRUE
  )
  
  stringr::str_match_all(texto, padrao)[[1]] |>
    as_tibble(.name_repair = "minimal") |>
    setNames(c("full", "min", "max", "unit")) |>
    mutate(
      min   = as.numeric(min),
      max   = as.numeric(max),
      value = dplyr::if_else(is.na(max), min, (min + max) / 2),
      trait_type = "quantitative",
      trait = NA_character_
    ) |>
    select(trait_type, trait, min, max, value, unit)
}

# -------------------------------------------------------------------------
# Passo C2: Extração de atributos qualitativos

# dicionario base: extensível

dic_qualitativo <- list(
  
  ## morfologia
  forma = c("orbicular", "eliptica", "lanceolada", "ovada", "linear"),
  apice = c("agudo", "obtuso", "acuminado", "arredondado", "mucronado"),
  base  = c("cuneada", "arredondada", "cordada", "decorrente"),
  
  ## superfície
  indumento = c("glabro", "pubescente", "piloso"),
  
  ## orientação
  posicao = c("ereta", "prostrada", "ascendente"),
  
  ## textura
  consistencia = c("crassa", "membranacea"),
  
  ## cor
  cor = c(
    "amarelo-esverdead[ao]s?",
    "verde",
    "vinos[ao]s?",
    "purpure[ao]s?"
  )
)


extrair_qualitativos_segmento <- function(texto) {
  
  if (is.na(texto)) {
    return(tibble(
      trait_type = character(),
      trait = character(),
      min = numeric(),
      max = numeric(),
      value = character(),
      unit = character()
    ))
  }
  
  texto <- texto |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII")
  
  purrr::imap_dfr(dic_qualitativo, function(termos, categoria) {
    
    encontrados <- stringr::str_extract_all(texto, termos) |>
      unlist() |>
      unique()
    
    if (length(encontrados) == 0) return(NULL)
    
    tibble(
      trait_type = "qualitative",
      trait = categoria,
      min = NA_real_,
      max = NA_real_,
      value = encontrados,
      unit = NA_character_
    )
  })
}


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

db_segmentado <- segmentar_estruturas(text.test) |>
  passo_B_completo()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

traits_quant <- db_segmentado |>
  dplyr::mutate(dados = purrr::map(segmento, extrair_quantitativos_segmento)) |>
  tidyr::unnest(dados) |>
  dplyr::select(any_of(c(
    "segmento_id",
    "estrutura",
    "segmento",
    "estrutura_principal",
    "subestrutura"
  )), everything())

traits_qual <- db_segmentado |>
  dplyr::mutate(dados = purrr::map(segmento, extrair_qualitativos_segmento)) |>
  tidyr::unnest(dados) |>
  dplyr::select(any_of(c(
    "segmento_id",
    "estrutura",
    "segmento",
    "estrutura_principal",
    "subestrutura"
  )), everything())

# -------------------------------------------------------------------------

cols_traits <- c(
  "segmento_id",
  "estrutura",
  "segmento",
  "estrutura_principal",
  "subestrutura",
  "trait_type",
  "trait",
  "min",
  "max",
  "value",
  "unit"
)

# -------------------------------------------------------------------------

traits_all <- bind_rows(traits_quant %>% 
                          dplyr::mutate(value = as.character(value))
                        , traits_qual) |>
  dplyr::select(all_of(cols_traits)) |>
  dplyr::arrange(
    segmento_id,
    estrutura_principal,
    subestrutura,
    trait_type,
    trait
  )

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
