##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Organização das funções para a extração dos traits.
#' Bloco B.

# -------------------------------------------------------------------------
# Load packages
if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(here, tidyverse, janitor)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Fase 1: Validar descrição -----------------------------------------------

validar_descricao <- function(texto) {
  
  if (is.na(texto) || str_trim(texto) == "")
    return("nao_extrair")
  
  texto <- str_to_lower(texto)
  
  # ------------------------------------------------------------------
  # 1. Metadados / referências → NÃO extrair
  # ------------------------------------------------------------------
  
  padrao_metadado <- c(
    "disponível em", "acesso em", "flora e funga",
    "jardim botânico", "world checklist",
    "\\(sp\\)", "s\\.n\\.", "herbário"
  )
  
  if (str_detect(texto, paste(padrao_metadado, collapse = "|")))
    return("nao_extrair")
  
  # ------------------------------------------------------------------
  # 2. Status biogeográfico simples → NÃO extrair
  # ------------------------------------------------------------------
  
  if (str_detect(texto, "^nativa$|endêmica|exótica|introduzida"))
    return("nao_extrair")
  
  # ------------------------------------------------------------------
  # 3. Morfologia e medidas
  # ------------------------------------------------------------------
  
  palavras_morf <- c(
    "raiz", "caule", "rizoma", "ramicaule", "pseudobulbo",
    "folha", "infloresc", "flor", "fruto",
    "labelo", "sépal", "pétala", "coluna", "polínia"
  )
  
  tem_morfologia <- str_detect(texto, paste(palavras_morf, collapse = "|"))
  
  tem_medidas <- str_detect(
    texto,
    "\\d+\\s*(mm|cm|m)|\\d+\\s*[x\\-–]\\s*\\d+"
  )
  
  tem_estrutura <- str_detect(
    texto,
    "raiz:|caule:|folha:|inflorescência:|flor:|fruto:"
  )
  
  # ------------------------------------------------------------------
  # 4. Gatekeeper lógico
  # ------------------------------------------------------------------
  
  if (tem_morfologia || tem_medidas || tem_estrutura)
    return("extrair")
  
  "nao_extrair"
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Fase 2: Extrair traits --------------------------------------------------

# -------------------------------------------------------------------------
# Step A ------------------------------------------------------------------

segmentar_estruturas <- function(texto) {
  
  # function limpar_repeticoes  # add 19/01/2026
  limpar_repeticoes <- function(x) {
    x %>%
      # remover prefixos editoriais ANTES de tudo
      stringr::str_replace_all(
        regex("(^|[;\\n])\\s*(pt|pt\\.|pl)\\b\\s+", ignore_case = TRUE),
        "\\1"
      ) %>%
      
      # normalizar separadores fortes
      stringr::str_replace_all("\\s*;\\s*", " |SEP| ") %>%
      
      # dividir em blocos candidatos
      stringr::str_split("\\s*\\|SEP\\|\\s*") %>%
      
      purrr::map_chr(function(blocos) {
        
        # normalização mínima apenas para comparação
        blocos_norm <- blocos %>%
          stringr::str_replace_all("\\s+", " ") %>%
          stringr::str_trim()
        
        # manter apenas a primeira ocorrência
        blocos_unicos <- blocos[!duplicated(blocos_norm)]
        
        # recompor o texto final
        blocos_unicos %>%
          stringr::str_trim() %>%
          paste(collapse = "; ")
      })
  }
  
  # start
  
  if (is.na(texto)) return(tibble())
  
  texto <- texto %>%
    limpar_repeticoes() %>% # add 19/01/2026
    stringr::str_replace_all("compr.","comprimento") %>% # add 12/01/2025
    stringr::str_replace_all("\\(cm\\)","cm") %>% # add 12/01/2025
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
      "planta"
    , "raiz"
    , "rizoma"
    , "ramicaule"
    , "folha"
    , "infloresc" 
    , "pedicelo"
    , "filamento" # add 31.12.2025
    , "fruto"
    , "sepala"
    , "petala"
    #, "labelo" 
    #, "coluna" # add 02.01.2026 # retirei porque a coluna faz parte da flor
    , "flor" # add 02.01.2026
    , "caule" # add 02.01.2026
  )
  
  # subfunctions 
  normalizar_texto <- function(x) {
    x %>%
      stringr::str_to_lower() %>%
      stringi::stri_trans_general("Latin-ASCII")
  }
  
  remover_prefixos_editoriais <- function(x) {
    x %>%
      stringr::str_remove("^\\s*(pt|pt\\.|pl|pt\\s*-)\\s+") 
  }
  
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
# -------------------------------------------------------------------------
# Step B ------------------------------------------------------------------

# -------------------------------------------------------------------------
# Step B1 -----------------------------------------------------------------

passo_B1_fill <- function(df) {
  
  df %>%
    tidyr::fill(estrutura, .direction = "down")
}

# -------------------------------------------------------------------------
# Step B2 -----------------------------------------------------------------

passo_B2_estrutura_principal <- function(df) {
  
  df %>%
    dplyr::mutate(
      estrutura_principal = case_when(
        estrutura %in% c(  "pedicelo"
                         , "filamento" # add 31.12.2025
                         , "sepala"
                         , "petala"
                         , "labelo"
                         , "calo" # add 14.01.2026
                         , "calice" # add 02.01.2026
                         , "textura segmento" # add 02.01.2026
                         , "sexualidade" # add 02.01.2026
                         , "coluna") ~ "flor",
        TRUE ~ estrutura
      )
    ) %>% # add 09.01.2026
    dplyr::mutate(
      estrutura_principal = case_when(
        estrutura %in% c("placentação") ~ "fruto", # dar uma olhada para generalizar
        TRUE ~ estrutura
      )
    )
}

# -------------------------------------------------------------------------
# Step B3 -----------------------------------------------------------------

passo_B3_subestrutura <- function(df) {
  
  #subfunctions
  normalizar_texto <- function(x) {
    x %>%
      stringr::str_to_lower() %>%
      stringi::stri_trans_general("Latin-ASCII")
  }
  
  df %>%
    dplyr::mutate(
      txt = normalizar_texto(segmento),
      
      subestrutura = dplyr::case_when(
        
        # --------------------- caule --------------------
        estrutura_principal == "caule" &
          stringr::str_detect(txt, "rizoma") ~ "rizoma", # add 12/01/2025
        
        estrutura_principal == "caule" &
          stringr::str_detect(txt, "pseudobulbo") ~ "pseudobulbo", # add 12/01/2025
        
        # ---------------- Inflorescência ----------------
        estrutura_principal == "infloresc" &
          stringr::str_detect(txt, "peduncul") ~ "pedunculo",
        
        estrutura_principal == "infloresc" &
          stringr::str_detect(txt, "bracte") ~ "bractea",
        
        # estrutura_principal == "infloresc" &
        #   stringr::str_detect(txt, "numero.*flor|flor.*numero") ~ "numero_flores", # add 09.01.2026
        
        # estrutura_principal == "infloresc" &
        #   stringr::str_detect(txt, "racemos|panicul|espig") ~ "tipo_inflorescencia", # add 09.01.2026 (talvez só retirar 'racemosa')
        
        # ---------------- Fruto ----------------
        estrutura_principal == "fruto" &
          stringr::str_detect(txt, "placenta") ~ "placenta", # add 09.01.2026
        
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
          stringr::str_detect(txt, "calo") ~ "calo", # add 14.01.2026
        
        # estrutura_principal == "flor" &
        #   stringr::str_detect(txt, "sexualidad") ~ "sexualidade", # add 09.01.2026
        
        # ---------------- Coluna ----------------
        estrutura_principal == "flor" &
          stringr::str_detect(txt, "coluna") ~ "coluna", # add 09.01.2026
        
        # estrutura_principal == "flor" &
        #   stringr::str_detect(txt, "coluna") ~ "coluna",
        
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
# Step B4 (complete) ------------------------------------------------------

passo_B_completo <- function(df) {
  
  df %>%
    passo_B1_fill() %>%
    passo_B2_estrutura_principal() %>%
    passo_B3_subestrutura()
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Step C ------------------------------------------------------------------

# -------------------------------------------------------------------------
# Step C1 -----------------------------------------------------------------

# -------------------------------------------------------------------------
# Quantitatives Traits

# new version 2 (funciona)
extrair_quantitativos_segmento <- function(texto) {
  
  if (is.na(texto)) {
    return(dplyr::tibble())
  }
  
  texto_original <- texto
  
  texto <- texto %>%
    stringr::str_replace_all(",", ".") %>%
    stringr::str_replace_all("–|—", "-") %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII")
  
  # ---------------------------------------------------------------
  # 1) CONTAGENS (somente se NÃO houver unidade métrica)
  
  possui_unidade <- grepl("\\b(mm|cm|m)\\b", texto)
  
  extrair_contagens <- function(texto) {
    
    padrao <- stringr::regex(
      "(?:numero(?:\\s+de)?\\s*)?(\\d+(?:/\\d+)*)",
      ignore_case = TRUE
    )
    
    m <- stringr::str_match_all(texto, padrao)[[1]]
    
    if (nrow(m) == 0) {
      return(dplyr::tibble())
    }
    
    valores <- m[, 2] %>%
      stringr::str_split("/") %>%
      unlist() %>%
      as.numeric()
    
    dplyr::tibble(
      trait_type = "quantitative",
      trait = "count",
      min = min(valores),
      max = max(valores),
      value = mean(valores),
      unit = NA_character_
    )
  }
  
  tib_count <- if (!possui_unidade) {
    extrair_contagens(texto)
  } else {
    dplyr::tibble()
  }
  
  # ---------------------------------------------------------------
  # 2) DIMENSÕES (somente se houver unidade)
  
  padrao_dim <- stringr::regex(
    "(\\d+(?:\\.\\d+)?)\\s*
     (?:-|ate)?\\s*
     (\\d+(?:\\.\\d+)?)?\\s*
     (?:comprimento|largura|altura)?\\s*
     (mm|cm|m)",
    ignore_case = TRUE,
    comments = TRUE
  )
  
  m_dim <- stringr::str_match_all(texto, padrao_dim)[[1]]
  
  tib_dim <- if (nrow(m_dim) == 0) {
    dplyr::tibble()
  } else {
    m_dim %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      rlang::set_names(c("full", "min", "max", "unit")) %>%
      dplyr::mutate(
        min = as.numeric(min),
        max = as.numeric(max),
        value = dplyr::if_else(is.na(max), min, (min + max) / 2),
        trait_type = "quantitative",
        trait = "dimension"
      ) %>%
      dplyr::select(trait_type, trait, min, max, value, unit)
  }
  
  # ---------------------------------------------------------------
  dplyr::bind_rows(tib_dim, tib_count)
}

# -------------------------------------------------------------------------
# Step C2 -----------------------------------------------------------------

# -------------------------------------------------------------------------
# Botanical Dictionary (Expandable)

dic_qualitativo <- list(
  
  ## comprimento # add 09/01/2025
  comprimento = c(
    "muito pequen[ao]s?|pequen[ao]s?",
    "muito grandes?|grandes?"
  ),
  
  ## forma
  forma = c(
    "orbicular",
    "eliptic[ao]s?",
    "lanceolad[ao]s?",
    "oval[- ]lanceolad[ao]s?",
    "oblanceolad[ao]s?",
    "ovad[ao]s?",
    "lineares?",
    "delgad[ao]s?", # add 09/01/2025
    "alad[ao]s?", # add 09/01/2025
    "ramificad[ao]s?", # add 09/01/2025
    "fusiforme", # add 09/01/2025
    "racemosas?", # add 09/01/2025
    "desenvolvida", # add 09/01/2025
    "elipsoide", # add 09/01/2025
    "rizomatosas?", # add 12/01/2025
    "claviforme", # add 12/01/2025
    "^longo",  # add 14/01/2025
    "trilobado", # add 13/01/2025
    "sem folhas?", # add 13/01/2025
    "consp[ií]cuas?", # add 14/01/2025
    "oblongos?", # add 14/01/2025
    #"\\b(proximal)\\b.*?\\b(lisa)\\b|\\b(distal)\\b.*?\\b(escamosa)\\b", # add 14/01/2025
    "\\b(proximal)\\b.*?\\b(lisa)\\b|\\b(distal)\\b.*?\\b(escamosa)\\b" # add 14/01/2025
    
  ),
  
  ## ápice
  apice = c(
    "agud[ao]s?",
    "obtus[ao]s?",
    "acuminad[ao]s?",
    "arredondad[ao]s?",
    "mucronad[ao]s?",
    "cuculad[ao]s?", # add 09/01/2025
    "denticulad[ao]s?", # add 09/01/2025
    "lateral|laterais" # add 14/01/2025
    
  ),
  
  ## base
  base = c(
    "atenuad[ao]s?",
    "cunead[ao]s?",
    "cordad[ao]s?",
    "decorrent[es]?",
    #pé
    "bulbosos?", # add 09/01/2025 
    "s[eé]ssil|s[eé]sseis" # add 14/01/2025 
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
    "recurvad[ao]s?",
    #"antera-ventral", # add 09/01/2025
    "axial", # add 09/01/2025
    "aéreos?|aereos?", # add 09/01/2025
    "basal", # add 09/01/2025
    "ventral", # add 09/01/2025
    "terminal", # add 09/01/2025
    "proximal & lisa", # add 14/01/2025
    "distal escamosa" # add 14/01/2025
    
  ),
  
  ## textura / consistência
  consistencia = c(
    "crass[ao]s?",
    "membranace[ao]s?"
  ),
  
  ## arranjo / número
  arranjo = c(
    "trimer[ao]s?",
    "multiflor[ao]s?",
    "espataceo",
    "sem folhas" # add 13/01/2025
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
    "purpure[ao]s?",
    "rosa escuro", # add 13.01.2026
    "rosa claro",  # add 13.01.2026
    "lil[aá]s|lila",  # add 13.01.2026
    "rosa escuro", # add 13.01.2026
    "purp[uú]re[oa]s?", # add 13.01.2026
    "branc[ao]s?" # add 14.01.2026
  )
)

# -------------------------------------------------------------------------
# Qualitative Traits

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
# Step D (final)-----------------------------------------------------------

extrair_traits_segmento <- function(texto) {
  
  qt <- extrair_quantitativos_segmento(texto)
  ql <- extrair_qualitativos_segmento(texto)
  
  # converter value apenas se existir
  if ("value" %in% names(qt)) {
    qt <- qt %>%
      dplyr::mutate(value = as.character(value))
  }
  
  dplyr::bind_rows(qt, ql)
}

# -------------------------------------------------------------------------
# Função wrapper: aplica TODO o pipeline a UM texto
# (A -> B -> C -> D)
# -------------------------------------------------------------------------

pipeline_extracao_traits <- function(texto) {
  
  segmentar_estruturas(texto) %>%
    passo_B_completo() %>%
    dplyr::mutate(
      dados = map(segmento, extrair_traits_segmento)
    ) %>%
    tidyr::unnest(dados, keep_empty = TRUE)
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
