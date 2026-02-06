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
# Função do Passo A

segmentar_estruturas <- function(texto) {
  
  if (is.na(texto)) return(tibble())
  
  texto <- texto |> 
    str_replace_all("\n", " ") |> 
    str_squish()
  
  # separa por ; ou por ponto seguido de maiúscula
  segmentos <- texto |>
    str_split(";(?![^()]*\\))|\\.\\s+(?=[A-ZÁÉÍÓÚ])") |>
    unlist() |>
    str_trim()
  
  tibble(
    segmento_id = seq_along(segmentos),
    segmento = segmentos
  ) |> 
    filter(segmento != "")
}


# -------------------------------------------------------------------------
# SubFunção 1 do Passo B

normalizar_texto <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII")
}

# -------------------------------------------------------------------------
# SubFunção 2 do Passo B

remover_prefixos_editoriais <- function(x) {
  x |>
    stringr::str_remove("^\\s*(pt|pt\\.|pl|pt\\s*-)\\s+") 
}

# -------------------------------------------------------------------------
# Função do Passo B

detectar_estrutura <- function(texto) {
  
  txt <- texto |>
    normalizar_texto() |>
    remover_prefixos_editoriais()
  
  dplyr::case_when(
    str_detect(txt, "^planta\\b")                         ~ "planta",
    str_detect(txt, "^raiz\\b")                           ~ "raiz",
    str_detect(txt, "^rizoma\\b")                         ~ "rizoma",
    str_detect(txt, "^ramicaule\\b")                      ~ "ramicaule",
    str_detect(txt, "^folha\\b")                          ~ "folha",
    
    # Inflorescência e partes
    str_detect(txt, "infloresc")                          ~ "inflorescencia",
    str_detect(txt, "peduncul|bractea|pedicelo|ovario")   ~ "inflorescencia",
    
    # Flor
    str_detect(txt, "sepala|petala")                      ~ "sepala",
    str_detect(txt, "labelo")                             ~ "labelo",
    str_detect(txt, "coluna")                             ~ "coluna",
    
    TRUE ~ "indefinido"
  )
}

# -------------------------------------------------------------------------
# Função do Passo C

detectar_subestrutura <- function(texto, estrutura) {
  
  if (is.na(texto)) return(NA_character_)
  
  txt <- texto |> normalizar_texto()
  
  if (estrutura != "inflorescencia") {
    return(NA_character_)
  }
  
  dplyr::case_when(
    str_detect(txt, "peduncul")              ~ "pedunculo",
    str_detect(txt, "bractea")               ~ "bractea",
    str_detect(txt, "pedicelo")              ~ "pedicelo",
    str_detect(txt, "ovario")                ~ "ovario",
    str_detect(txt, "filamento")             ~ "filamento",
    TRUE                                     ~ NA_character_
  )
}

# -------------------------------------------------------------------------

# Passo A
segmentar_estruturas(text.test) |>
  # Passo B
  dplyr::mutate(
    estrutura_raw = map_chr(segmento, detectar_estrutura)) |>
  tidyr::fill(estrutura_raw, .direction = "down") |>
  # Passo C
  dplyr::mutate(
    estrutura = estrutura_raw,
    subestrutura = map2_chr(segmento, estrutura, detectar_subestrutura))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Aqui utiliza 'normalizar_texto', 'remover_prefixos_editoriais'

segmentar_estruturas <- function(texto) {
  
  if (is.na(texto)) return(tibble())
  
  texto <- texto |>
    str_replace_all("\n", " ") |>
    str_squish()
  
  # lista explícita de estruturas principais
  estruturas <- c(
    "planta",
    "raiz",
    "rizoma",
    "ramicaule",
    "folha",
    "inflorescencia",
    "sepal",
    "petala",
    "labelo",
    "coluna"
  )
  
  padrao_inicio <- paste0(
    "\\.\\s+(?=(",
    paste(estruturas, collapse = "|"),
    ")\\b)"
  )
  
  segmentos <- texto |>
    normalizar_texto() |>
    remover_prefixos_editoriais() |>
    str_split(padrao_inicio) |>
    unlist() |>
    str_trim()
  
  tibble(
    segmento_id = seq_along(segmentos),
    segmento = segmentos
  ) |>
    filter(segmento != "")
}


# -------------------------------------------------------------------------

# Passo A — estruturas principais
segmentar_estruturas(text.test) |>
  
  # Passo B — detectar estrutura
  mutate(
    estrutura_raw = map_chr(segmento, detectar_estrutura)
  ) |>
  fill(estrutura_raw, .direction = "down") |>
  
  # Passo C — subestruturas (AGORA funciona!)
  mutate(
    estrutura = estrutura_raw,
    subestrutura = map2_chr(segmento, estrutura, detectar_subestrutura)
  )


# -------------------------------------------------------------------------

segmentar_subestruturas <- function(segmento, estrutura) {
  
  if (estrutura != "inflorescencia") {
    return(tibble(
      sub_id = 1,
      sub_texto = segmento
    ))
  }
  
  subsegmentos <- segmento |>
    str_split(";") |>
    unlist() |>
    str_trim() |>
    discard(~ .x == "")
  
  tibble(
    sub_id = seq_along(subsegmentos),
    sub_texto = subsegmentos
  )
}

# -------------------------------------------------------------------------

detectar_subestrutura <- function(texto) {
  
  txt <- texto |> normalizar_texto()
  
  dplyr::case_when(
    str_detect(txt, "^peduncul")        ~ "pedunculo",
    str_detect(txt, "^bractea floral")  ~ "bractea_floral",
    str_detect(txt, "^bractea")         ~ "bractea",
    str_detect(txt, "^pedicelo")        ~ "pedicelo",
    str_detect(txt, "ovario")           ~ "ovario",
    str_detect(txt, "^filamento")       ~ "filamento",
    TRUE                                ~ NA_character_
  )
}


# -------------------------------------------------------------------------


segmentar_estruturas(text.test) |>
  mutate(
    estrutura = map_chr(segmento, detectar_estrutura)
  ) |>
  tidyr::fill(estrutura, .direction = "down") |>
  
  # Passo A2
  rowwise() |>
  mutate(
    sub_tbl = list(segmentar_subestruturas(segmento, estrutura))
  ) |>
  unnest(sub_tbl) |>
  
  # Passo C real
  mutate(
    subestrutura = if_else(
      estrutura == "inflorescencia",
      map_chr(sub_texto, detectar_subestrutura),
      NA_character_
    )
  ) 
