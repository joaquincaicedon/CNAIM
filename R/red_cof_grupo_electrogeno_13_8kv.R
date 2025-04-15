#' @title Costo de Red por Falla para Grupos Electrógenos Diésel
#' @description Esta función calcula el costo de red por falla para
#' todas las categorías de activos, excluyendo los transformadores EHV y 132kV.
#' (cf. sección 7.6, página 87, CNAIM, 2021). El costo de red por falla
#' se utiliza en la derivación de las consecuencias de falla, ver \code{\link{cof}}().
#' @param tf_asset_category tipo_generador Texto. El tipo de categoría de activo.
#' Opciones: \code{tipo_generador = c("Grupo Electrógeno Diésel 13.8kV")}.
#' @param demanda_mva Numérico. La demanda actual en el activo
#' @param red_segura Booleano. Si el activo está en una red segura o no
#' @param gb_ref_given parámetro opcional para usar valores de referencia personalizados.
#' @return Numérico. Costo de falla operativa (red).
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' Y adaptación de CNAIM para considerar grupos electrógenos diésel.
#' @export
#' @examples
#' red_cof_grupo_electrogeno_13_8kv(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
#'                                  demanda_mva = 12)
red_cof_grupo_electrogeno_13_8kv <- function(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
                                             demanda_mva,
                                             red_segura = T,
                                             gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL
  
  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  reference_costs_of_failure_GE <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` == tipo_generador)
  
  # Costo financiero de referencia por falla -------------------------------------
  ncost <- reference_costs_of_failure_GE$`Network Performance - (GBP)`
  
  # Factor de carga ---------------------------------------------------------


  ref_nw_perf_cost_fail_ehv_df <- gb_ref_taken$ref_nw_perf_cost_of_fail_ehv
  ref_nw_perf_cost_fail_ehv_single_row_df <- dplyr::filter(ref_nw_perf_cost_fail_ehv_df,
                                                           `Asset Category` == tipo_generador)

  load_factor <- demanda_mva/ref_nw_perf_cost_fail_ehv_single_row_df$`Maximum Demand Used To Derive Reference Cost (MVA)`
  
  # Factor de tipo de red -----------------------------------
  network_type_factor <- 1

  if(!red_segura){
    network_type_factor <- 2.5
  }

  # Factor de consecuencia de rendimiento de red ----------------------------
  network_performance_consequence_factor <- load_factor * network_type_factor

  # Costo de falla por rendimiento de red -------------------------------------
  network_cof <- network_performance_consequence_factor * ncost

  return(network_cof)

}