#' @title Consecuencias de falla para un grupo electrógeno diésel de 13.8 kV
#' @description Esta función calcula las consecuencias de falla
#' para un grupo electrógeno diésel de 13.8 kV (cf. sección 7, página 75, CNAIM, 2021) 
#' y modificación de CNAIM para incluir grupos electrógenos diésel.
#' @inheritParams financiero_cof_grupo_electrogeno_13_8kv
#' @inheritParams seguridad_cof_grupo_electrogeno_13_8kv
#' @inheritParams ambiental_cof_grupo_electrogeno_13_8kv
#' @inheritParams red_cof_grupo_electrogeno_13_8kv
#' @param gb_ref_given parámetro opcional para usar valores de referencia personalizados
#' @return Numérico. Consecuencias de falla para un grupo electrógeno diésel de 13.8 kV.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' Y adaptación de CNAIM para considerar grupos electrógenos diésel.
#' @export
#' @examples
#' # Consecuencias de falla para un grupo electrógeno diésel de 13.8 kV
#' cof_grupo_electrogeno_13_8kv(kva = 500, type = "Type C",
#'                              type_risk = "High", location_risk = "High",
#'                              prox_water = 50, bunded = "No",
#'                              no_customers = 500, kva_per_customer = 1)


cof_grupo_electrogeno_13_8kv <- function(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
                                         MVA,
                                         acceso,
                                         riesgo_tipo,
                                         riesgo_ubicación,
                                         distancias_agua,
                                         contención,
                                         carga_actual,
                                         red_segura,
                                         gb_ref_given = NULL) {

  financiero <- f_cof_transformer_11kv(kva, type, gb_ref_given)

  seguridad <- s_cof_swg_tf_ohl(type_risk, location_risk,
                             asset_type_scf = tipo_generador, gb_ref_given)

  ambiental <-  e_cof_tf(asset_type_tf = tipo_generador,
                             rated_capacity = kva,
                             prox_water, bunded,
                             gb_ref_given)

  red <-
    n_cof_excl_ehv_132kv_tf(asset_type_ncf = "6.6/11kV Transformer (GM)",
                            no_customers, kva_per_customer, gb_ref_given)

  return(financiero + seguridad + ambiental + red)
}
