
# -------------------------------------------------------------------------

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

segmentar_estruturas(text.test)

# -------------------------------------------------------------------------

detectar_estrutura <- function(segmento) {
  
  estruturas <- c(
    planta = "planta",
    raiz = "raiz",
    rizoma = "rizoma",
    ramicaule = "ramicaule",
    folha = "folha",
    inflorescencia = "infloresc",
    sepala = "sépala",
    petala = "pétala",
    labelo = "labelo",
    coluna = "coluna",
    fruto = "fruto"
  )
  
  for (est in names(estruturas)) {
    if (str_detect(segmento, estruturas[[est]]))
      return(est)
  }
  
  return("indefinido")
}

# -------------------------------------------------------------------------

segmentar_estruturas(text.test) |>
  mutate(estrutura = map_chr(segmento, detectar_estrutura))

# -------------------------------------------------------------------------

normalizar_segmento <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[áàâã]", "a") |>
    stringr::str_replace_all("[éê]", "e") |>
    stringr::str_replace_all("[í]", "i") |>
    stringr::str_replace_all("[óôõ]", "o") |>
    stringr::str_replace_all("[ú]", "u") |>
    stringr::str_replace_all("\\bpt\\b", "") |>
    stringr::str_squish()
}

# -------------------------------------------------------------------------


detectar_estrutura <- function(segmento) {
  
  seg <- normalizar_segmento(segmento)
  
  estruturas <- list(
    planta          = "\\bplanta\\b",
    raiz            = "\\braiz(es)?\\b",
    rizoma          = "\\brizoma(s)?\\b",
    ramicaule       = "\\bramicaule(s)?\\b",
    folha           = "\\bfolha(s)?\\b",
    inflorescencia  = "\\binfloresc",
    sepala          = "\\bsepala(s)?\\b",
    petala          = "\\bpetala(s)?\\b",
    labelo          = "\\blabelo(s)?\\b",
    coluna          = "\\bcoluna(s)?\\b",
    fruto           = "\\bfruto(s)?\\b"
  )
  
  for (est in names(estruturas)) {
    if (stringr::str_detect(seg, estruturas[[est]]))
      return(est)
  }
  
  return("indefinido")
}

# -------------------------------------------------------------------------

segmentar_estruturas(text.test) |>
  dplyr::mutate(estrutura = map_chr(segmento, detectar_estrutura)) %>% 
  dplyr::mutate(
    estrutura = if_else(
      estrutura == "indefinido",
      lag(estrutura),
      estrutura
    )
  )


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Passo A — Normalizar texto antes de detectar

normalizar_texto <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII")
}

# Passo A — Normalizar texto antes de detectar

detectar_estrutura <- function(texto) {
  
  txt <- normalizar_texto(texto)
  
  dplyr::case_when(
    str_detect(txt, "^planta\\b")                        ~ "planta",
    str_detect(txt, "^raiz\\b")                          ~ "raiz",
    str_detect(txt, "^rizoma\\b")                        ~ "rizoma",
    str_detect(txt, "^ramicaule\\b")                     ~ "ramicaule",
    str_detect(txt, "^folha\\b")                         ~ "folha",
    
    # Inflorescência e partes
    str_detect(txt, "infloresc")                         ~ "inflorescencia",
    str_detect(txt, "peduncul|bractea|pedicelo|ovario")  ~ "inflorescencia",
    
    # Flor
    str_detect(txt, "sepala|petala")                     ~ "sepala",
    str_detect(txt, "labelo")                            ~ "labelo",
    str_detect(txt, "coluna")                            ~ "coluna",
    
    TRUE ~ "indefinido"
  )
}

# Passo C — Herdar corretamente a última estrutura válida

segmentar_estruturas(text.test) |>
  dplyr::mutate(estrutura_raw = map_chr(segmento, detectar_estrutura)) |>
  tidyr::fill(estrutura_raw, .direction = "down") |>
  dplyr::mutate(estrutura = estrutura_raw)


# -------------------------------------------------------------------------

remover_prefixos_editoriais <- function(x) {
  x |>
    stringr::str_remove("^\\s*(pt|pt\\.|pl|pt\\s*-)\\s+") 
}

# -------------------------------------------------------------------------

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

segmentar_estruturas(text.test) |>
  dplyr::mutate(estrutura_raw = map_chr(segmento, detectar_estrutura)) |>
  tidyr::fill(estrutura_raw, .direction = "down") |>
  dplyr::mutate(estrutura = estrutura_raw)

# agora precisamos extrair as subesturturas das linhas 7,8 e9 que
# pertencem à inflorescencia

# -------------------------------------------------------------------------

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

segmentar_estruturas(text.test) |>
  dplyr::mutate(
    estrutura_raw = map_chr(segmento, detectar_estrutura)
  ) |>
  tidyr::fill(estrutura_raw, .direction = "down") |>
  dplyr::mutate(
    estrutura = estrutura_raw,
    subestrutura = map2_chr(segmento, estrutura, detectar_subestrutura)
  )

# -------------------------------------------------------------------------

expandir_subestruturas <- function(texto, estrutura) {
  
  if (is.na(texto)) return(tibble(segmento = NA_character_))
  
  txt <- texto |> normalizar_texto()
  
  # só faz sentido para inflorescência (por enquanto)
  if (estrutura != "inflorescencia") {
    return(tibble(segmento = texto))
  }
  
  # padrões de início de subestruturas
  padrao_subestruturas <- c(
    "peduncul",
    "bractea",
    "pedicelo",
    "ovario",
    "filamento"
  )
  
  # cria lookahead: divide antes de cada subestrutura
  padrao_split <- paste0("(?=\\b(", paste(padrao_subestruturas, collapse = "|"), ")\\b)")
  
  partes <- stringr::str_split(txt, padrao_split)[[1]] |>
    stringr::str_trim()
  
  tibble(segmento = partes)
}

# -------------------------------------------------------------------------

res_expandido <- res |>
  dplyr::mutate(
    segmento_expandido = purrr::map2(segmento, estrutura, expandir_subestruturas)
  ) |>
  tidyr::unnest(segmento_expandido) |>
  dplyr::select(-segmento) |>
  dplyr::rename(segmento = segmento_expandido)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
