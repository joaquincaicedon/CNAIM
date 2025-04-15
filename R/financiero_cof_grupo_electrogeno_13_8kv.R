#' @title Financial cost of Failure for Transformers
#' @description This function calculates financial consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Financial consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param tf_asset_category String The type of Transformer asset category
#' Options: \code{tf_asset_category = c("6.6/11kV Transformer (GM)",
#' "20kV Transformer (GM)", "33kV Transformer (GM)", "66kV Transformer (GM) "
#' "132kV Transformer (GM) ")}.
#' @param access_factor_criteria String. Asses Financial factor criteria for Transformer
#' setting (cf. table 221, page 180, CNAIM, 2021).
#' @param type_financial_factor_size String The type financial factor size for Transformer
#' @param type_financial_factor_kva_mva Numeric The type financial factor kVA MVA for Transformer
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Financial consequences of failure for Transformer
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' financial_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
#' type_financial_factor_size = "33/20kV, CMR equivalent",
#' type_financial_factor_kva_mva = 20,
#' access_factor_criteria = "Type A")
financiero_cof_grupo_electrogeno_13_8kv <- function(tf_asset_category,
                                                    type_financial_factor_size = NULL,
                                                    type_financial_factor_kva_mva = NULL,
                                                    access_factor_criteria,
                                                    gb_ref_given = NULL){
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
  print(fcost)
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


  print(type_financial_factor)
  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref_taken$access_factor_swg_tf_asset

  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                               `Asset Category` ==
                                                 asset_category)

  if (access_factor_criteria == "Type A") {
    access_financial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type A Criteria - Normal Access ( & Default Value)`
  }
  else if (access_factor_criteria == "Type B") {
    access_financial_factor <-
      access_financial_factors_tf$`Access Factor: Type B Criteria - Constrained Access or Confined Working Space`
  }  else if (access_factor_criteria == "Type C") {
    access_financial_factor <-
      access_financial_factors_tf$`Access Factor: Type C Criteria - Underground substation`
  }

print(access_financial_factor)
  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_financial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost)
}

