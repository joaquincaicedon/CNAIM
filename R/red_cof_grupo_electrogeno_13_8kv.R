#' @title Network cost of Failure for Transformers
#' @description This function calculates network cost of failure for
#' all asset categories exclusive the assets EHV and 132kV transformers.
#' (cf. section 7.6, page 87, CNAIM, 2021). Network cost of failure
#' is used in the derivation of consequences of failure see \code{\link{cof}}().
#' @param tf_asset_category String The type of Transformer asset category
#' Options: \code{tf_asset_category = c("6.6/11kV Transformer (GM)",
#' "20kV Transformer (GM)", "33kV Transformer (GM)", "66kV Transformer (GM) "
#' "132kV Transformer (GM) ")}.
#' @param actual_load_mva Numeric. The actual load on the asset
#' @param secure Boolean If the asset is in a secure network or not
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Network cost of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' network_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
#' actual_load_mva = 15)
red_cof_grupo_electrogeno_13_8kv <- function(tf_asset_category,
                                             actual_load_mva,
                                             secure = T,
                                             gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tf_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`
  cat("costo referencia red:", ncost)
  # Load factor ---------------------------------------------------------
  if(tf_asset_category == "132kV Transformer (GM)")
    load_factor_asset_category <- "132kV Transformer"
  if(tf_asset_category == "66kV Transformer (GM)")
    load_factor_asset_category <- "66kV Transformer"
  if(tf_asset_category == "33kV Transformer (GM)")
    load_factor_asset_category <- "33kV Transformer (GM)"
  if(tf_asset_category == "Transformador 34kV (GM)")
    load_factor_asset_category <- "Transformador 34kV (GM)"


  ref_nw_perf_cost_fail_ehv_df <- gb_ref_taken$ref_nw_perf_cost_of_fail_ehv
  ref_nw_perf_cost_fail_ehv_single_row_df <- dplyr::filter(ref_nw_perf_cost_fail_ehv_df,
                                                           `Asset Category` ==
                                                             load_factor_asset_category)

  load_factor <- actual_load_mva/ref_nw_perf_cost_fail_ehv_single_row_df$`Maximum Demand Used To Derive Reference Cost (MVA)`
  cat("factor de carga:", load_factor)
  # Network type factor -----------------------------------
  network_type_factor <- 1

  if(!secure){
    network_type_factor <- 2.5
  }

  cat("tipo de factor de red:", network_type_factor)
  # Network perfomance consequence factor -----------------------------------

  network_performance_consequence_factor <- load_factor *
    network_type_factor

  # Network performance cost of failure -------------------------------------
  network_cof <- network_performance_consequence_factor * ncost

  return(network_cof)

}
