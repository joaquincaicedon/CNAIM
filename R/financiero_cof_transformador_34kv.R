#' @title Financial cost of Failure for 30/10 kV and 60/10 kV Transformers
#' @description This function calculates financial consequences of failure
#' Outputted in COL.
#' @param tf_asset_category String The type of Transformer asset category
#' Options: \code{tf_asset_category = c("30kV Transformer (GM)",
#' "60kV Transformer (GM)")}.
#' @param access_factor_criteria String. Asses Financial factor criteria for Transformer
#' setting (cf. table 221, page 180, CNAIM, 2021).
#' Options: \code{access_factor_criteria = c("Type A", "Type B", "Type C")}.
#' @param type_financial_factor_kva_mva Numeric The type financial factor kVA MVA for Transformer
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Financial consequences of failure for Transformer
#' @export
#' @examples
#' financiero_cof_transformer_34kv(tf_asset_category = "30kV Transformer (GM)",
#' type_financial_factor_kva_mva = 20,
#' access_factor_criteria = "Type A")
financiero_cof_transformador_34kv <- function(tf_asset_category,
                                              type_financial_factor_kva_mva = NULL,
                                              access_factor_criteria,
                                              gb_ref_given = NULL) {

  GBP_to_COL <- 1
  if (tf_asset_category == "30kV Transformer (GM)" ) {
    tf_asset_category <- "33kV Transformer (GM)"
    type_financial_factor_size <- "33/11 or 6.6kV, CMR equivalent"
  } else if (tf_asset_category == "60kV Transformer (GM)" ) {
    tf_asset_category <- "66kV Transformer (GM)"
    type_financial_factor_size <- "66/11 or 6.6kV, CMR equivalent"
  } else {
    tf_asset_category <- NULL
    warning("Wrong input, please go to CNAIM.io for more documentation")
  }


  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type Financial Factor Criteria` = `Lower` = `Upper` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tf_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tf_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factors <- gb_ref_taken$type_financial_factors

  type_financial_factors_tf <- dplyr::filter(type_financial_factors,
                                             `Asset Register Category` == tf_asset_category)

  if(!is.null(type_financial_factor_size)){
    type_financial_factors_tf <- type_financial_factors_tf %>%
      dplyr::filter(`Type Financial Factor Criteria` == type_financial_factor_size)
  }


  if(!is.null(type_financial_factor_kva_mva)){
    type_financial_factors_tf <- type_financial_factors_tf %>%
      dplyr::filter(`Lower` <= type_financial_factor_kva_mva,
                    `Upper` > type_financial_factor_kva_mva)

  }

  type_financial_factor <- type_financial_factors_tf$`Type Financial Factor`[1]

  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref_taken$access_factor_swg_tf_asset

  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                               `Asset Category` ==
                                                 asset_category)

  if (access_factor_criteria == "Type A") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type A Criteria - Normal Access ( & Default Value)`
  }
  else if (access_factor_criteria == "Type B") {
    access_finacial_factor <-
      access_financial_factors_tf$`Access Factor: Type B Criteria - Constrained Access or Confined Working Space`
  }  else if (access_factor_criteria == "Type C") {
    access_finacial_factor <-
      access_financial_factors_tf$`Access Factor: Type C Criteria - Underground substation`
  }


  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_finacial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost * GBP_to_COL)
}
