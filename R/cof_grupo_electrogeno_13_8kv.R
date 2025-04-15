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
                                         nivel_tensión,
                                         MVA,
                                         acceso,
                                         riesgo_ubicación,
                                         riesgo_tipo,
                                         distancia_agua,
                                         acotado,
                                         carga_actual,
                                         red_segura,
                                         gb_ref_given = NULL) {

  financiero <- financiero_cof_grupo_electrogeno_13_8kv(tipo_generador = tipo_generador,
                                                        type_financial_factor_size = nivel_tensión,
                                                        type_financial_factor_kva_mva = MVA,
                                                        access_factor_criteria = acceso)

  seguridad <- seguridad_cof_transformador_34kv(tf_asset_category = tipo_generador,
                                                location_risk = riesgo_ubicación,
                                                type_risk = riesgo_tipo)

  ambiental <- ambiental_cof_transformador_34kv(tf_asset_category = tipo_generador,
                                                prox_water = distancia_agua,
                                                bunded = acotado,
                                                size_kva_mva = MVA)

  red <- red_cof_transformador_34kv(tf_asset_category = tipo_generador,
                                    actual_load_mva = carga_actual,
                                    secure = red_segura)

  CoF <- financiero + seguridad + ambiental + red
  
  cat("CoF financiero:", financiero,
      "; CoF seguridad:", seguridad,
      "; CoF ambiental:", ambiental,
      "; COF red:", red,
      "y COF total:", CoF)

  return(CoF)
}
