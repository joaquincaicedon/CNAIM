#' @title Network cost of Failure for 30/10kV and 60/10kV Transformers
#' @description This function calculates network cost of failure for
#' Outputted in COL.
#' @param tf_asset_category String The type of Tower
#' Options: \code{tf_asset_category = c("30kV Transformer (GM)",
#' "60kV Transformer (GM)")}.
#' @param actual_load_mva Numeric. The actual load on the asset
#' @param secure Boolean If the asset is in a secure network or not
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Network cost of failure.
#' @export
#' @examples
#' red_cof_transformer_30_60kv(tf_asset_category = "30kV Transformer (GM)",
#' actual_load_mva = 15)
red_cof_transformador_34kv<- function(tf_asset_category,
                                           actual_load_mva,
                                           secure = T,
                                           gb_ref_given = NULL) {

  GBP_to_COL <- 1
  if (tf_asset_category == "30kV Transformer (GM)" ) {
    tf_asset_category <- "33kV Transformer (GM)"
  } else if (tf_asset_category == "60kV Transformer (GM)" ) {
    tf_asset_category <- "66kV Transformer (GM)"
  } else {
    tf_asset_category <- NULL
    warning("Wrong input, please go to CNAIM.io for more documentation")
  }

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tf_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Load factor ---------------------------------------------------------
  if(tf_asset_category == "132kV Transformer (GM)")
    load_factor_asset_category <- "132kV Transformer"
  if(tf_asset_category == "66kV Transformer (GM)")
    load_factor_asset_category <- "66kV Transformer"
  if(tf_asset_category == "33kV Transformer (GM)")
    load_factor_asset_category <- "33kV Transformer (GM)"


  ref_nw_perf_cost_fail_ehv_df <- gb_ref_taken$ref_nw_perf_cost_of_fail_ehv
  ref_nw_perf_cost_fail_ehv_single_row_df <- dplyr::filter(ref_nw_perf_cost_fail_ehv_df,
                                                           `Asset Category` ==
                                                             load_factor_asset_category)

  load_factor <- actual_load_mva/ref_nw_perf_cost_fail_ehv_single_row_df$`Maximum Demand Used To Derive Reference Cost (MVA)`

  # Network type factor -----------------------------------
  network_type_factor <- 1

  if(!secure){
    network_type_factor <- 2.5
  }

  # Network perfomance consequence factor -----------------------------------

  network_performance_consequence_factor <- load_factor *
    network_type_factor

  # Network performance cost of failure -------------------------------------
  network_cof <- network_performance_consequence_factor * ncost

  return(network_cof * GBP_to_COL)

}
