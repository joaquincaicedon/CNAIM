#' @title Consecuencias de Seguridad por Falla para Transformadores de 13.2 kV
#' @description Esta función calcula las consecuencias de seguridad por fallo
#' para transformadores de 13.2 kV
#' (cf. sección 7.4, página 80, CNAIM, 2021). Las consecuencias de seguridad por falla
#' se utilizan en la derivación de las consecuencias de falla, ver \code{\link{cof}}().
#' @param tipo_riesgo Cadena de texto. Riesgo que el activo presenta al
#' público por sus características y situación particular. Opciones:
#' \code{tipo_riesgo = c("Bajo", "Medio", "Alto", "Default")}
#' (cf. tabla 225A, página 183, CNAIM, 2021).
#' Una configuración de \code{"Default"} equivale a una configuración de \code{"Medio"}.
#' @param riesgo_ubicacion Cadena de texto. Proximidad a áreas que pueden afectar su
#' probabilidad de intrusión o interferencia. Opciones:
#' \code{riesgo_ubicacion = c("Bajo", "Medio", "Alto", "Default")}
#' (cf. tabla 225A, página 183, CNAIM, 2021).
#' Una configuración de \code{"Default"} equivale a una configuración de \code{"Medio"}.
#' @param tipo_transformador Cadena de texto.
#' Opciones: \code{tipo_transformador = c("Transformador 13.2kV")}
#' @param gb_ref_given Parámetro opcional para usar valores de referencia personalizados
#' @return Numérico. Consecuencias de seguridad por falla para
#' interruptores, transformadores y líneas aéreas.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Consecuencias de seguridad por falla para un transformador de 13.2 kV
#' s_cof_transformador_13_2kv(tipo_riesgo = "Default", riesgo_ubicacion = "Default",
#'                            tipo_transformador = "Transformador 13.2kV")

s_cof_transformador_13_2kv <- function(tipo_riesgo = "Default",
                                       riesgo_ubicacion = "Default",
                                       tipo_transformador = "Transformador 13.2kV",
                                       gb_ref_given = NULL) {

  `Asset Register Category` = NULL
  # due to NSE notes in R CMD check

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  # Get category ------------------------------------------------------------
  reference_costs_of_failure_tf <-
    dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tipo_transformador)

  # Reference safety cost of failure ----------------------------------------
  scost <- reference_costs_of_failure_tf$`Safety - (GBP)`


  #  Safety Consequence factor ----------------------------------------------
  factor_consec_seg_transformador <- gb_ref_taken$factor_consec_seg_generador

  if (riesgo_ubicacion == "Default") riesgo_ubicacion <- "Medio (Default)"
  if (riesgo_ubicacion == "Medio") riesgo_ubicacion <- "Medio (Default)"
  if (tipo_riesgo == "Default") tipo_riesgo <- "Medio"

  row_no <- which(factor_consec_seg_transformador$
  `Safety Consequence Factor - Switchgear, Transformers & Overhead Lines...2` ==
    riesgo_ubicacion)

  col_no <- grep(tipo_riesgo, colnames(factor_consec_seg_transformador))

  safety_consequence_factor <- factor_consec_seg_transformador[row_no, col_no]

  # Safety consequence of failure -------------------------------------------
  safety_cof <- safety_consequence_factor * scost

  return(safety_cof)
}