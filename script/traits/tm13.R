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
# subfuncoes de A
normalizar_texto <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII")
}

remover_prefixos_editoriais <- function(x) {
  x %>%
    stringr::str_remove("^\\s*(pt|pt\\.|pl|pt\\s*-)\\s+") 
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Passo A

segmentar_estruturas <- function(texto) {
  
  if (is.na(texto)) return(tibble())
  
  texto <- texto %>%
    stringr::str_replace_all("\n", " ") %>%
    stringr::str_squish()
  
  # fragmentação mínima (somente para leitura sequencial)
  partes <- texto %>%
    stringr::str_split("[;\\.]") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_remove("^,\\s*") %>% #add 31.12.2025
    discard(~ .x == "") 
  
  estruturas_validas <- c(
    "planta", "raiz", "rizoma", "ramicaule", "folha",
    "infloresc", 
    "pedicelo", "filamento", # add 31.12.2025
    "fruto",
    "sepala", "petala", "labelo", "coluna"
    ,"flor" # add 02.01.2026
    ,"caule" # add 02.01.2026
  )
  
  detectar_estrutura <- function(x) {
    x_norm <- x %>%
      normalizar_texto() %>%
      remover_prefixos_editoriais()
    
    purrr::keep(
      estruturas_validas,
      ~ stringr::str_detect(x_norm, paste0("^", .x))
    ) %>%
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
  
  bind_rows(segmentos) %>%
    mutate(segmento_id = row_number()) %>%
    select(segmento_id, estrutura, segmento)
}

# -------------------------------------------------------------------------
#PASSO B1 — Resolver continuidade (fill)
passo_B1_fill <- function(df) {
  
  df %>%
    tidyr::fill(estrutura, .direction = "down")
}

# -------------------------------------------------------------------------
# PASSO B2 — Estrutura principal correta

passo_B2_estrutura_principal <- function(df) {
  
  df %>%
    mutate(
      estrutura_principal = case_when(
        estrutura %in% c("pedicelo", "filamento", # add 31.12.2025
                         "sepala", "petala", "labelo"
                         ,"calice" # add 02.01.2026
                         ,"textura segmento" # add 02.01.2026
                         ,"sexualidade" # add 02.01.2026
                         , "coluna") ~ "flor",
        TRUE ~ estrutura
      )
    )
}

# -------------------------------------------------------------------------
# PASSO B3 — Identificar subestruturas
passo_B3_subestrutura <- function(df) {
  
  df %>%
    dplyr::mutate(
      txt = normalizar_texto(segmento),
      
      subestrutura = dplyr::case_when(
        
        # ---------------- Inflorescência ----------------
        estrutura_principal == "infloresc" &
          stringr::str_detect(txt, "peduncul") ~ "pedunculo",
        
        estrutura_principal == "infloresc" &
          stringr::str_detect(txt, "bracte") ~ "bractea",
        
        estrutura_principal == "infloresc" &
          stringr::str_detect(txt, "numero.*flor|flor.*numero") ~ "numero_flores",
        
        estrutura_principal == "infloresc" &
          stringr::str_detect(txt, "racemos|panicul|espig") ~ "tipo_inflorescencia",
        
        # ---------------- Flor ----------------
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "pedicel") ~ "pedicelo",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "ovari") ~ "ovario",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "filament") ~ "filamento",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "sepala") ~ "sepala",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "petala") ~ "petala",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "labelo") ~ "labelo",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "calice") ~ "calice",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "corola") ~ "corola",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "textura.*segment") ~ "segmento",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "sexualidad") ~ "sexualidade",
        
        # ---------------- Coluna ----------------
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "coluna") ~ "coluna",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "antera") ~ "antera",
        
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "polinia") ~ "polinia",
        
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-txt)
}

# -------------------------------------------------------------------------
# Passo B completo

passo_B_completo <- function(df) {
  
  df %>%
    passo_B1_fill() %>%
    passo_B2_estrutura_principal() %>%
    passo_B3_subestrutura()
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

normalizar_dimensoes_2d <- function(texto) {
  
  padrao_2d <- stringr::regex(
    "(\\d+(?:\\.\\d+)?)\\s*-\\s*(\\d+(?:\\.\\d+)?)\\s*[x×]\\s*
     (\\d+(?:\\.\\d+)?)\\s*-\\s*(\\d+(?:\\.\\d+)?)(\\s*(mm|cm|m))",
    comments = TRUE,
    ignore_case = TRUE
  )
  
  stringr::str_replace_all(
    texto,
    padrao_2d,
    function(m) {
      
      nums <- stringr::str_match(
        m,
        padrao_2d
      )
      
      m1 <- mean(as.numeric(nums[2:3]))
      m2 <- mean(as.numeric(nums[4:5]))
      unit <- nums[6]
      
      paste0(
        round(m1, 2),
        "-",
        round(m2, 2),
        unit
      )
    }
  )
}

# -------------------------------------------------------------------------

# Passo C1: Extração de atributos quantitativos

extrair_quantitativos_segmento <- function(texto) {
  
  if (is.na(texto)) {
    return(dplyr::tibble())
  }
  
  texto <- texto %>%
    stringr::str_replace_all(",", ".") %>%
    stringr::str_replace_all("–|—", "-") %>%
    normalizar_dimensoes_2d() %>%
    stringr::str_replace_all("\\s*×\\s*", "-")
  
  padrao <- stringr::regex(
    "(?<min>\\d+(?:\\.\\d+)?)\\s*
     (?:-|x)?\\s*
     (?<max>\\d+(?:\\.\\d+)?)?\\s*
     (?<unit>mm|cm|m)",
    ignore_case = TRUE,
    comments = TRUE
  )
  
  res <- stringr::str_match_all(texto, padrao)[[1]]
  
  if (nrow(res) == 0) return(dplyr::tibble())
  
  res %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    rlang::set_names(c("full", "min", "max", "unit")) %>%
    dplyr::mutate(
      min = as.numeric(min),
      max = as.numeric(max),
      value = dplyr::if_else(is.na(max), min, (min + max) / 2),
      trait_type = "quantitative",
      trait = NA_character_
    ) %>%
    dplyr::select(trait_type, trait, min, max, value, unit)
}

# -------------------------------------------------------------------------
# Passo C2: Extração de atributos qualitativos

# -------------------------------------------------------------------------
# dicionario base: extensível

dic_qualitativo <- list(
  
  ## forma
  forma = c(
    "orbicular",
    "eliptic[ao]s?",
    "lanceolad[ao]s?",
    "oval[- ]lanceolad[ao]s?",
    "oblanceolad[ao]s?",
    "ovad[ao]s?",
    "lineares?"
  ),
  
  ## ápice
  apice = c(
    "agud[ao]s?",
    "obtus[ao]s?",
    "acuminad[ao]s?",
    "arredondad[ao]s?",
    "mucronad[ao]s?"
  ),
  
  ## base
  base = c(
    "atenuad[ao]s?",
    "cunead[ao]s?",
    "cordad[ao]s?",
    "decorrent[es]?"
  ),
  
  ## superfície
  indumento = c(
    "glabr[ao]s?",
    "pubescent[es]?",
    "pilos[ao]s?"
  ),
  
  ## posição
  posicao = c(
    "eret[ao]s?",
    "suberet[ao]s?",
    "prostrad[ao]s?",
    "ascendent[es]?",
    "patent[es]?",
    "recurvad[ao]s?"
  ),
  
  ## textura / consistência
  consistencia = c(
    "crass[ao]s?",
    "membranace[ao]s?"
  ),
  
  ## arranjo / número
  arranjo = c(
    "trimer[ao]s?",
    "multiflor[ao]s?"
  ),
  
  ## sexualidade 
  sexualidade = c(
    "dioic[ao]s?",
    "monoic[ao]s?",
    "poligam[ao]s?",
    "hermafrodit[ao]s?",
    "estaminad[ao]s?",
    "pistilad[ao]s?"
  ),
  
  ## cor
  cor = c(
    "verde(s)?",
    "amarelo[- ]esverdead[ao]s?",
    "vinos[ao]s?",
    "purpure[ao]s?"
  )
)

# -------------------------------------------------------------------------

extrair_qualitativos_segmento <- function(texto) {
  
  if (is.na(texto)) {
    return(dplyr::tibble())
  }
  
  texto <- texto %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII")
  
  purrr::imap_dfr(dic_qualitativo, function(termos, categoria) {
    
    encontrados <- purrr::map(
      termos,
      ~ stringr::str_extract_all(texto, stringr::regex(.x))
    ) %>%
      unlist() %>%
      unique()
    
    if (length(encontrados) == 0) return(NULL)
    
    dplyr::tibble(
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

db_segmentado <- segmentar_estruturas(text.test2) %>%
  passo_B_completo()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

traits_quant <- db_segmentado %>%
  dplyr::mutate(dados = purrr::map(segmento, extrair_quantitativos_segmento)) %>%
  tidyr::unnest(dados) %>%
  dplyr::select(any_of(c(
    "segmento_id",
    "estrutura",
    "segmento",
    "estrutura_principal",
    "subestrutura"
  )), everything())

traits_qual <- db_segmentado  %>% 
  dplyr::mutate(dados = purrr::map(segmento, extrair_qualitativos_segmento)) %>%
  tidyr::unnest(dados) %>%
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
                        , traits_qual) %>%
  dplyr::select(all_of(cols_traits)) %>%
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
