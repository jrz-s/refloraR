##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################



# -------------------------------------------------------------------------

extrair_quantitativos <- function(texto) {
  
  if (is.na(texto)) return(tibble())
  
  padrao <- "(?<trait>comprimento|largura|altura|diâmetro)[^0-9]*?(?<min>[0-9]+\\.?[0-9]*)\\s*(?:-|–|a)?\\s*(?<max>[0-9]+\\.?[0-9]*)?\\s*(?<unit>mm|cm|m)"
  
  stringr::str_match_all(texto, padrao)[[1]] |>
    as_tibble(.name_repair = "minimal") |>
    setNames(c("full", "trait", "min", "max", "unit")) |>
    mutate(
      min  = as.numeric(min),
      max  = as.numeric(max),
      value = ifelse(is.na(max), min, (min + max) / 2)
    ) |>
    select(trait, min, max, value, unit)
}


# -------------------------------------------------------------------------

extrair_qualitativos <- function(texto) {
  
  if (is.na(texto)) return(tibble())
  
  traits <- c(
    "forma_foliar" = "lanceolada|ovada|elíptica|linear",
    "margem"       = "inteira|serrada|crenada",
    "ápice"        = "agudo|acuminado|obtuso",
    "indumento"    = "glabro|pubescente|piloso",
    "inflorescencia" = "espiga|panícula|racemo"
  )
  
  map_dfr(names(traits), function(tr) {
    valor <- stringr::str_extract(texto, traits[[tr]])
    if (!is.na(valor)) tibble(trait = tr, value = valor) else NULL
  })
}

# -------------------------------------------------------------------------
# Qualidade por especie

gerar_log <- function(texto, quant, qual) {
  
  tibble(
    descricao_vazia = is.na(texto),
    n_quantitativos = nrow(quant),
    n_qualitativos  = nrow(qual),
    status = case_when(
      is.na(texto) ~ "sem_descricao",
      nrow(quant) + nrow(qual) == 0 ~ "descricao_nao_funcional",
      TRUE ~ "ok"
    )
  )
}

# -------------------------------------------------------------------------

# FUNÇÃO-MESTRE extract_traits()

extract_traits <- function(df, id_col, text_col, status_col) {
  
  df |>
    rowwise() |>
    mutate(
      
      quant = list(
        if ({{ status_col }} == "ok")
          extrair_quantitativos({{ text_col }})
        else
          tibble()
      ),
      
      qual = list(
        if ({{ status_col }} %in% c("ok", "morfologia_sem_medidas"))
          extrair_qualitativos({{ text_col }})
        else
          tibble()
      ),
      
      log = list(
        gerar_log({{ text_col }}, quant, qual)
      )
      
    ) |>
    ungroup() |>
    mutate(
      traits = map2(
        quant, qual,
        ~ bind_rows(
          .x |> mutate(type = "quantitative"),
          .y |> mutate(type = "qualitative")
        )
      )
    ) |>
    select({{ id_col }}, traits, log) |>
    list(
      traits = unnest(., traits),
      log    = unnest(., log)
    )
}

# -------------------------------------------------------------------------

extract_traits <- function(df, id_col, text_col) {
  
  df |>
    rowwise() |>
    mutate(
      quant = list(extrair_quantitativos({{ text_col }})),
      qual  = list(extrair_qualitativos({{ text_col }})),
      log   = list(gerar_log({{ text_col }}, quant, qual))
    ) |>
    ungroup() |>
    mutate(
      traits = map2(
        quant, qual,
        ~ bind_rows(
          .x |> mutate(type = "quantitative"),
          .y |> mutate(type = "qualitative")
        )
      )
    ) |>
    select({{ id_col }}, traits, log) |>
    list(
      traits = unnest(., traits),
      log    = unnest(., log)
    )
}


# -------------------------------------------------------------------------

res <- extract_traits(
  df         = db_clean
  ,id_col     = id
  ,text_col   = ctrl_descrp
  ,status_col = status
)

traits_final <- res$traits
log_qualidade <- res$log


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------




