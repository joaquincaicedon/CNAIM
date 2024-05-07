#' @title Consequences of Failure for a 6.6/11 kV transformer
#' @description This function calculates consequences of failure
#' for a 6.6/11 kV transformer (cf.section 7, page 75, CNAIM, 2021).
#' @inheritParams f_cof_transformer_11kv
#' @inheritParams s_cof_swg_tf_ohl
#' @inheritParams e_cof_tf
#' @inheritParams n_cof_excl_ehv_132kv_tf
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Consequences of failure for a 6.6/11 kV transformer.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Consequences of failure for a 6.6/11 kV transformer
#' cof_transformer_11kv(kva = 500, type = "Type C",
#'                      type_risk = "High", location_risk = "High",
#'                      prox_water = 50, bunded = "No",
#'                      no_customers = 500, kva_per_customer = 1)


cof_transformador_34kv <- function(tipo_TP, Sn, Rtransfor, acceso, riesgo_loc, riesgo_tipo,
                                   dist_agua, acotado, carga_actual, red_segura,
                                    gb_ref_given = NULL) {

  finance <- financiero_cof_transformador_34kv(tf_asset_category = "tipo_TP",
                                               type_financial_factor_size = "Rtransfor",
                                               type_financial_factor_kva_mva = Sn,
                                               access_factor_criteria = "acceso")


  safety <- seguridad_cof_transformador_34kv(tf_asset_category = "tipo_TP", location_risk = "riesgo_loc",type_risk = "riesgo_tipo")



  environmental <-  ambiental_cof_transformador_34kv(tf_asset_category = "tipo_TP", prox_water = dist_agua, bunded = "acotado", size_kva_mva = MVA)

  network <- red_cof_transformador_34kv(tf_asset_category = "tipo_TP",
                                        actual_load_mva = carga_actual, secure = "red_segura")


  return(finance + safety + environmental + network)
}
