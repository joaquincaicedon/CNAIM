#' @title Safety cost of Failure for 30/10kv and 60/10kv Transformer
#' @description This function calculates safety consequences of failure
#' Outputted in COL.
#' @param tf_asset_category String The type of Transformer
#' Options: \code{tf_asset_category = c("30kV Transformer (GM)",
#' "60kV Transformer (GM)")}.
#' @param location_risk String Type Financial factor criteria for Transformer
#' (cf. section D1.2.1, page 178, CNAIM, 2021).
#' Options: \code{location_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{location_risk = "Medium"}.
#' @param type_risk String. Asses Financial factor criteria for Transformer
#' setting (cf. table 221, page 180, CNAIM, 2021).
#' Options: \code{type_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{type_risk = "Medium"}.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Safety consequences of failure for Transformers
#' @export
#' @examples
#' seguridad_cof_transformador_34kv(tf_asset_category = "30kV Transformer (GM)",
#' location_risk = "Default",
#' type_risk = "Default")
seguridad_cof_transformador_34kv <- function(tf_asset_category,
                                           location_risk,
                                           type_risk,
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

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tf_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tf_asset_category)

  # Reference financial cost of failure -------------------------------------
  scost <- reference_costs_of_failure_tf$`Safety - (GBP)`

  if (location_risk == "Default") location_risk <- "Medium (Default)"
  if (location_risk == "Medium") location_risk <- "Medium (Default)"
  if (type_risk == "Default") type_risk <- "Medium"

  safety_conseq_factor_sg_tf_oh <- gb_ref_taken$safety_conseq_factor_sg_tf_oh

  row_no <- which(safety_conseq_factor_sg_tf_oh$
                    `Safety Consequence Factor - Switchgear, Transformers & Overhead Lines...2` ==
                    location_risk)

  col_no <- grep(type_risk, colnames(safety_conseq_factor_sg_tf_oh))

  safety_consequence_factor <- safety_conseq_factor_sg_tf_oh[row_no, col_no]

  # Safety consequence of failure -------------------------------------------
  safety_cof <- safety_consequence_factor * scost

  return(safety_cof * GBP_to_COL)
}
