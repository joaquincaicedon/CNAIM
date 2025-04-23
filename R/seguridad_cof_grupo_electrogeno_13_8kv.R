#' @title Costo de seguridad por falla para grupo electrógeno diésel
#' @description Esta función calcula las consecuencias de seguridad por falla
#' (cf. sección 7.3, página 79, CNAIM, 2021). Las consecuencias de seguridad
#' por falla se utilizan en la derivación de las consecuencias de falla, ver 
#' \code{\link{cof}}().
#' @param tipo_generador Texto. El tipo de categoría de activo.
#' Opciones: \code{tipo_generador = c("Grupo Electrógeno Diésel 13.8kV")}.
#' @param riesgo_ubicación Texto. Tipo de criterio de factor de riesgo de ubicación 
#' para grupo electrógeno diésel #' (cf. sección D.2.2, página 183, CNAIM, 2021).
#' Opciones: \code{riesgo_ubicación = c("Bajo", "Medio", "Alto")}.
#' La configuración predeterminada es \code{riesgo_ubicación = "Medio"}.
#' @param riesgo_tipo Texto. Tipo de criterio de calificación de riesgo 
#' para grupo electrógeno diésel (cf. sección D.2.2, página 183, CNAIM, 2021).
#' Opciones: \code{riesgo_tipo = c("Bajo", "Medio", "Alto")}.
#' La configuración predeterminada es \code{riesgo_tipo = "Medio"}.
#' @param gb_ref_given Parámetro opcional para usar valores de referencia personalizados.
#' @return Numérico. Consecuencias de seguridad por falla para grupos electrógenos diésel
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' Y adaptación de CNAIM para considerar grupos electrógenos diésel.
#' @export
#' @examples
#' seguridad_cof_grupo_electrogeno_13_8kv(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
#'                                        riesgo_ubicación = "Default",
#'                                        riesgo_tipo = "Default")

seguridad_cof_grupo_electrogeno_13_8kv <- function(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
                                                   riesgo_ubicación,
                                                   riesgo_tipo,
                                                   gb_ref_given = NULL){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

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

  # Reference financial cost of failure -------------------------------------
  scost <- reference_costs_of_failure_GE$`Safety - (GBP)`

  if (riesgo_ubicación == "Default") riesgo_ubicación <- "Medio (Default)"
  if (riesgo_ubicación == "Medio") riesgo_ubicación <- "Medio (Default)"
  if (riesgo_tipo == "Default") riesgo_tipo <- "Medio (Default)"
  if (riesgo_tipo == "Medio") riesgo_tipo <- "Medio (Default)"

  factor_consec_seg_generador <- gb_ref_taken$factor_consec_seg_generador

  row_no <- which(factor_consec_seg_generador$
                  `Factor de consecuencia de seguridad - Generadores...2` ==
                  riesgo_ubicación)

  col_no <- grep(riesgo_tipo, colnames(factor_consec_seg_generador))

  safety_consequence_factor <- factor_consec_seg_generador[row_no, col_no]

  # Safety consequence of failure -------------------------------------------
  safety_cof <- safety_consequence_factor * scost

  return(safety_cof)
}