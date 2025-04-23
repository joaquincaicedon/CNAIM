#' @title Costo ambiental de falla para grupos electrógenos diésel
#' @description This function calculates environmental consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Environmental consequences
#' of failure is used in the derivation of consequences of failure see 
#' \code{\link{cof}}().
#' @param tipo_generador Texto. El tipo de categoría de activo.
#' Opciones: \code{tipo_generador = c("Grupo Electrógeno Diésel 13.8kV")}.
#' @param distancia_agua Numérico. Especifique la proximidad a un curso de agua 
#' en metros. Una configuración de \code{"Default"} resultará en un factor de 
#' proximidad de 1. Por lo tanto, asuma que la proximidad a un curso de agua 
#' está entre 80m y 120m (cf. tabla 231, página 188, CNAIM, 2021).
#' @param acotado Texto. Opciones: \code{acotado = c("Sí", "No", "Default")}.
#' Una configuración de \code{"Default"} resultará en un factor de acotamiento de 1.
#' @param MVA Numérico. La clasificación MVA KVA para el grupo electrógeno diésel.
#' @param nivel_tensión Texto. Nivel de tensión del grupo electrógeno diésel.
#' @param gb_ref_given Parámetro opcional para usar valores de referencia personalizados.
#' @return Numérico. Consecuencias ambientales de falla para grupos electrógenos diésel.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' ambiental_cof_grupo_electrogeno_13_8kv(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
#'                                        distancia_agua = 95,
#'                                        acotado = "Sí",
#'                                        MVA = 15,
#'                                        nivel_tensión = "13.8kV")

ambiental_cof_grupo_electrogeno_13_8kv <- function(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
                                                   distancia_agua,
                                                   acotado,
                                                   MVA = NULL,
                                                   nivel_tensión = NULL,
                                                   gb_ref_given = NULL){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type environment factor` = `Size` = `Lower` = `Upper` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tipo_generador) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_GE <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` == tipo_generador)

  # Costo financiero de referencia de falla ---------------------------------
  ecost <- reference_costs_of_failure_GE$`Environmental - (GBP)`

  # Factor de tipo ambiental -------------------------------------
  type_environmental_factor <- 1

  # Factor de tamaño ambiental -------------------------------------
  size_environmental_factor_df <- gb_ref_taken$size_enviromental_factor

  size_environmental_factor_df <- size_environmental_factor_df %>%
    dplyr::filter(`Asset Register Category` == tipo_generador)

  if(!is.null(nivel_tensión)){
    size_environmental_factor_df <- size_environmental_factor_df %>%
      dplyr::filter(`Size` == nivel_tensión)
  }

  if(!is.null(MVA)){
    size_environmental_factor_df <- size_environmental_factor_df %>%
      dplyr::filter(`Lower` <= MVA,
                    `Upper` > MVA)
  }

  size_environmental_factor <- size_environmental_factor_df$`Size Environmental Factor`[1]

  # Tabla de factores ambientales de ubicación (tabla 222) ------------------
  location_environ_al_factor <- gb_ref_taken$location_environ_al_factor

  location_environ_al_factor_GE <- dplyr::filter(location_environ_al_factor,
                                                 `Asset Register Category` == asset_category)

  if(nrow(location_environ_al_factor_GE) == 0){
    location_environmental_factor <- 1
  } else {
    # Acotado "Sí", "No", "Default" ?
    if (acotado == "Default") {
      bunding_factor <- 1
    } else if (acotado == "Sí") {
      bunding_factor <-
        location_environ_al_factor_GE$`Bunding Factor: Bunded`
    } else if (acotado == "No") {
      bunding_factor <-
        location_environ_al_factor_GE$`Bunding Factor: Not bunded`
    }

    # Proximidad al agua.
    if(distancia_agua == "Default") {
      prox_factor <- 1
    } else if (distancia_agua >= 40 && distancia_agua < 80) {
      prox_factor <- location_environ_al_factor_GE$
        `Proximity Factor: Close to Water Course (between 40m and 80m)`
    } else if (distancia_agua >= 80 && distancia_agua < 120) {
      prox_factor <- location_environ_al_factor_GE$
        `Proximity Factor: Moderately Close to Water Course (between 80m and 120m)`
    } else if (distancia_agua > 120) {
      prox_factor <- location_environ_al_factor_GE$
        `Proximity Factor: Not Close to Water Course (>120m) or No Oil`
    } else if (distancia_agua < 40) {
      prox_factor <- location_environ_al_factor_GE$
        `Proximity Factor: Very Close to Water Course (<40m)`
    }

    # Factor ambiental de ubicación
    location_environmental_factor <- prox_factor * bunding_factor
  }

  environmental_consequences_factor <- (type_environmental_factor *
                                        size_environmental_factor *
                                        location_environmental_factor)

  # Consecuencias ambientales ----------------------------------------------
  environmental_cof <- environmental_consequences_factor * ecost
  return(environmental_cof)
}