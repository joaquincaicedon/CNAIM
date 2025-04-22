#' @title Costo financiero de falla para grupos electrógenos diésel de 13.8 kV
#' @description Esta función calcula las consecuencias financieras de falla
#' (cf. sección 7.3, página 79, CNAIM, 2021). Las consecuencias financieras
#' de falla se utilizan en la derivación de las consecuencias de falla, ver
#' \code{\link{cof}}().
#' @param tipo_generador Texto. El tipo de categoría de activo.
#' Opciones: \code{tipo_generador = c("Grupo Electrógeno Diésel 13.8kV")}.
#' @param acceso Texto. Criterios del factor financiero de acceso para el
#' grupo electrógeno (cf. tabla 221, página 180, CNAIM modificada, 2021).
#' @param nivel_tensión Texto. Nivel de tensión del grupo electrógeno.
#' @param MVA Numérico. Potencia aparente nominal del grupo electrógeno.
#' @param gb_ref_given parámetro opcional para usar valores de referencia 
#' personalizados.
#' @return Numérico. Consecuencias financieras de falla para grupos electrógenos.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' Y adaptación de CNAIM para considerar grupos electrógenos diésel.
#' @export
#' @examples
#' financiero_cof_grupo_electrogeno_13_8kv(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
#'                                         nivel_tensión = "13.8kV",
#'                                         MVA = 15,
#'                                         acceso = "Tipo A")

financiero_cof_grupo_electrogeno_13_8kv <- function(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
                                                    nivel_tensión = NULL,
                                                    MVA = NULL,
                                                    acceso,
                                                    gb_ref_given = NULL){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type Financial Factor Criteria` = `Lower` = `Upper` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tipo_generador) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                 tipo_generador)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`
 
  # Type financial factor ---------------------------------------------------
  type_financial_factors <- gb_ref_taken$type_financial_factors

  type_financial_factors_GE <- dplyr::filter(type_financial_factors,
                                             `Asset Register Category` == tipo_generador)

  if(!is.null(nivel_tensión)){
    type_financial_factors_GE <- type_financial_factors_GE %>%
      dplyr::filter(`Type Financial Factor Criteria` == nivel_tensión)
  }

  if(!is.null(MVA)){
    type_financial_factors_GE <- type_financial_factors_GE %>%
      dplyr::filter(`Lower` <= MVA,
                    `Upper` > MVA)
  }

  type_financial_factor <- type_financial_factors_GE$`Type Financial Factor`[1]
  
  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref_taken$access_factor_swg_tf_asset

  access_financial_factors_GE <- dplyr::filter(access_financial_factors,
                                               `Asset Category` == asset_category)

  if (acceso == "Tipo A") {
    access_financial_factor <- access_financial_factors_GE$
                               `Access Factor: Type A Criteria - Normal Access ( & Default Value)`
  }
  else if (acceso == "Tipo B") {
    access_financial_factor <- access_financial_factors_GE$
                               `Access Factor: Type B Criteria - Constrained Access or Confined Working Space`
  }  else if (acceso == "Tipo C") {
    access_financial_factor <- access_financial_factors_GE$
                               `Access Factor: Type C Criteria - Underground substation`
  }

  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_financial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost)
}