#' @importFrom magrittr %>%
#' @title Consecuencias Ambientales de Falla para Transformadores de 13.2 kV
#' @description Esta función calcula las consecuencias ambientales de falla
#' para todos los tipos de transformadores. (cf. sección 7.5, página 84, CNAIM, 2021).
#' Las consecuencias ambientales de falla se utilizan en
#' la derivación de las consecuencias de falla, ver \code{\link{cof}}().
#' @param tipo_transformador Cadena de texto. Tipos de transformadores.
#' Opciones: \code{tipo_transformador = c("Transformador 13.2kV")}.
#' @param kva Numérico. La capacidad nominal de un transformador.
#' Un valor de \code{"Default"} resultará en un
#' factor ambiental de tamaño de 1 (cf. tabla 230, página 187, CNAIM, 2021).
#' @param distancia_agua Numérico. Especificar la proximidad a un curso de agua en metros.
#' Un valor de \code{"Default"} resultará en un factor de proximidad de 1. Por lo tanto,
#' se asume que la proximidad a un curso de agua está entre 80m y 120m
#' (cf. tabla 231, página 188, CNAIM, 2021).
#' @param acotado Cadena de texto. Opciones: \code{acotado = c("Sí", "No", "Default")}.
#' Un valor de \code{"Default"} resultará en un factor de contención de 1.
#' @param gb_ref_given parámetro opcional para usar valores de referencia personalizados
#' @return Numérico. Costo financiero de falla para un transformador de 13.2 kV.
#' @source Metodología Común de Índices de Activos de Red (CNAIM),
#' Salud y Criticidad - Versión 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Consecuencias ambientales de falla para un transformador de 13.2 kV
#' e_cof_transformador_13_2kv(tipo_transformador = "Transformador 13.2kV",
#'                            kva = 750,
#'                            distancia_agua = 100,
#'                            acotado = "Sí")

e_cof_transformador_13_2kv <- function(tipo_transformador = "Transformador 13.2kV",
                                       kva = "Default",
                                       distancia_agua = "Default",
                                       acotado = "Default",
                                       gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` = `Lower` = NULL
  # due to NSE notes in R CMD check

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  # Get category ------------------------------------------------------------
  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tipo_transformador) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure <-
    gb_ref_taken$reference_costs_of_failure

    reference_costs_of_failure_tf <- dplyr::filter(reference_costs_of_failure,
                                                   `Asset Register Category` ==
                                                     tipo_transformador)

  # Reference environmental cost of failure ---------------------------------
  ecost <- reference_costs_of_failure_tf$`Environmental - (GBP)`

  # Type environmetal factor table 221 --------------------------------------
  type_environmetal_factors <- 1 # Default for assets not in table 221

  # Size environmetal factor table 222 --------------------------------------
  if (tipo_transformador == "132kV Transformer (GM)") {
    size_enviromental_factor_tf <- dplyr::filter(
      gb_ref_taken$size_enviromental_factor,
      `Asset Register Category` ==
        tipo_transformador)

    size_enviromental_factor_tf <-
      size_enviromental_factor_tf %>% dplyr::filter(!is.na(Lower))

  } else {

    size_enviromental_factor_tf <- dplyr::filter(
      gb_ref_taken$size_enviromental_factor,
      `Asset Register Category` ==
        tipo_transformador)
  }

  for (n in 1:nrow(size_enviromental_factor_tf)){
    if (kva == 'Default'){
      size_environmental_factor <- 1
      break
    } else if (rated_capacity >= as.numeric(
      size_enviromental_factor_tf$Lower[n]) &
      kva < as.numeric(
        size_enviromental_factor_tf$Upper[n])){
      size_environmental_factor <-
        size_enviromental_factor_tf$`Size Environmental Factor`[n]
      break
    }
  }

  # Location environmetal factor table 222 ----------------------------------
  location_environ_al_factor <- gb_ref_taken$location_environ_al_factor

  location_environ_al_factor_tf <- dplyr::filter(location_environ_al_factor,
                                                 `Asset Register Category` ==
                                                   asset_category)

  # Bunded "Yes", "No", "Default" ?
  if (acotado == "Default") {
    bunding_factor <- 1
  } else if (acotado == "Sí") {
    bunding_factor <-
      location_environ_al_factor_tf$`Bunding Factor: Bunded`
  } else if (acotado == "No") {
    bunding_factor <-
      location_environ_al_factor_tf$`Bunding Factor: Not bunded`
  }

  # Proximity to water.
  if(distancia_agua == "Default") {
    prox_factor <- 1
  } else if (distancia_agua >= 40 && distancia_agua < 80) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Close to Water Course (between 40m and 80m)`
  } else if (distancia_agua >= 80 && distancia_agua < 120) {
    prox_factor <- location_environ_al_factor_tf$
    `Proximity Factor: Moderately Close to Water Course (between 80m and 120m)`
  } else if (distancia_agua > 120) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Not Close to Water Course (>120m) or No Oil`
  } else if (distancia_agua < 40) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Very Close to Water Course (<40m)`
  }

  # Location environmental factor
  location_environmental_factor <- prox_factor * bunding_factor

  # Environmental consequences factor ---------------------------------------
  environmental_consequences_factor <- (type_environmetal_factors *
                                        size_environmental_factor *
                                        location_environmental_factor)

  # Environmental consequences ----------------------------------------------
  environmental_cof <- environmental_consequences_factor * ecost
  return(environmental_cof)
}