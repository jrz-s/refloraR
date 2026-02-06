##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Extração de traits com uso de funções.

# -------------------------------------------------------------------------
# Load packages

# if(!require("pacman")){
#   install.packages("pacman")
#   library(pacman)
# }

pacman::p_load(here, tidyverse, janitor, writexl)

# -------------------------------------------------------------------------
# Load Functions

source(here::here('script','traits','ftextmining.R'))

# -------------------------------------------------------------------------
# Load database (Text)

# -------------------------------------------------------------------------
text.test1 <- c("PT  Planta muito pequena, 3,25–6,76 mm compr. Raiz delgada, produzida ao
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
ápice cuculado, denticulado, pé bulboso.")

text.test2 <- c("Raiz: tipo ramificada(s). Caule: tipo aéreo; forma fusiforme(s); disposição ereto(s). Folha: consistência membranácea(s); formato plana(s) plicada(s) com limbo patente(s) a(s) recurvado(s) oblanceolada(s) a(s) oval-lanceolada(s) com a(s) base atenuada(s) e o ápice(s) agudo(s) ou acuminado(s). Inflorescência: posição basal(ais) da base do caule(s); tipo racemosa(s) ereta(s) a(s) subereta(s); número de flor(es) multiflora(s). Flor: segmento(s) trímero(s) cálice(s) e corola vistoso(s) e desenvolvido(s); textura segmento(s) membranáceo(s); sexualidade dioica(s) estaminada(s); coluna(s) desenvolvida(s); antera(s) ventral(ais) a(s) terminal(ais); polínia(s) elipsoide. Fruto: placentação axial(ais). ")

text.test3 <- c("Caule: planta(s) rizomatosa(s); número de entrenó(s) do rizoma(s) 3; compr. do pseudobulbo 10 até 30 compr. (cm); número entrenó(s) pseudobulbo 2; forma do pseudobulbo claviforme(s)/achatado(s) lateralmente. Folha: número 1; forma lanceolada(s)/elíptico(s) lanceolada(s). Inflorescência: inflorescência(s) em pseudobulbo diferenciado sem folha(s) não; bráctea(s) espataceo(s) dupla(s); número de flor(es) 1/2/3/4/5. Flor: cor das pétala(s) e sépala(s) rosa escuro/rosa claro/lilás; forma do labelo(s) levemente trilobado(s)/istmo curto(s) entre lobo(s) mediano(s) e base dos lobo(s) lateral(ais) ou lobo(s) mediano(s) séssil(eis); cor do lobo(s) mediano(s) do labelo(s) rosa escuro/purpúreo; cor dos lobo(s) lateral(ais) do labelo(s) rosa claro/lilás; lobo(s) lateral(ais) do labelo(s) compr. e disposição longo(s) envolvendo a(s) coluna(s); número de polínia(s) 4. ")

text.test4 <- c("Folha: tipo regular(es) e conspícua(s); número 1 - 3; posição decurrente(s) ou levemente patente(s)/fortemente patente(s); forma estreitamente lanceolada(s); ápice(s) agudo(s). Flor: cor rosa a(s) lilás; cor da porção distal(ais) do calo do labelo branco; labelo formato fortemente trilobado(s); labelo lobo(s) lateral(ais) forma estreitamente obovado(s); labelo ápice(s) lobo(s) lateral(ais) livre(s); labelo lobo(s) lateral(ais) ápice(s) arredondado(s); labelo lobo mediano base séssil(eis)/unguiculado(s); labelo lobo mediano forma oblongo(s)/ovado(s)/deltoide(s); calo ornamentação metade proximal(ais) lisa(s) metade distal(ais) escamosa(s). ")

text.test5 <- c("van den Berg, C. Brassavola  in Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Disponível em:  <https://floradobrasil.jbrj.gov.br/FB606260>. Acesso em:  08 ago. 2025")

text.test6 <- c("Nativa")

# -------------------------------------------------------------------------
# Get text segments

db_segmentado <- segmentar_estruturas(text.test2) %>%
  passo_B_completo()

# -------------------------------------------------------------------------
# Get traits

traits_all <- db_segmentado %>%
  
  dplyr::mutate(
    dados = purrr::map(segmento, extrair_traits_segmento)
  ) %>%
  
  tidyr::unnest(dados, keep_empty = TRUE) %>%
  
  dplyr::select(
    segmento_id,
    estrutura,
    segmento,
    estrutura_principal,
    subestrutura,
    trait_type,
    trait,
    min,
    max,
    value,
    unit
  ) %>%
  
  dplyr::arrange(
    segmento_id,
    estrutura_principal,
    subestrutura,
    trait_type,
    trait
  ) #%>% dplyr::filter(trait_type %>% is.na())

# writexl::write_xlsx(traits_all,path = here::here("teste3.xlsx"))

## Tarefa dia 14.01.2026
#' trabalhar no script 2 pois nele a gente limpa qualquer base de dados que repita informações.
#' Lembrando que agora tenho um script das funções que estou modificando ('ftextmining.R')
#' script 9 e 13 funcionam de boa, porém com imperfeições"

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
