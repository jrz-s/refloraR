
# step 1
db_quant <- db_clean |> 
  dplyr::filter(status == "ok")

#step 2: Padronizar o output da função quantitativa

extrair_quantitativos <- function(texto) {
  
  if (is.na(texto)) return(tibble())
  
  padrao <- "(?<trait>comprimento|largura|altura|diâmetro)[^0-9]*?(?<min>[0-9]+\\.?[0-9]*)\\s*(?:-|–|a)?\\s*(?<max>[0-9]+\\.?[0-9]*)?\\s*(?<unit>mm|cm|m)"
  
  res <- stringr::str_match_all(texto, padrao)[[1]]
  
  if (nrow(res) == 0) return(tibble())
  
  as_tibble(res, .name_repair = "minimal") |>
    setNames(c("full", "trait", "min", "max", "unit")) |>
    mutate(
      min = as.numeric(min),
      max = as.numeric(max),
      value_num = ifelse(is.na(max), min, (min + max) / 2)
    ) |>
    select(trait, value_num, unit)
}

# step 3: Aplicar a função quantitativa linha a linha

traits_quant <- db_quant |> 
  dplyr::select(id, ctrl_descrp) |> 
  dplyr::mutate(
    traits = purrr::map(ctrl_descrp, extrair_quantitativos)
  ) |> 
  tidyr::unnest(traits) |> 
  dplyr::mutate(
    trait_type = "quantitative",
    source = "text_morphology"
  )

# Problemas observados
# ao momento de padronizar, a estrutura das orações precisa ser melhor entendida
# isto porque ainda não conseguimos extrair e o valor e reconhecer a onde ele pertence
# ou seja, à estrutura na qual ele faz parte. 



# step 4: Diagnóstico da cobertura quantitativa

traits_quant |> 
  count(id)








