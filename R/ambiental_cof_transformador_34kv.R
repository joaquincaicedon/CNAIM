#' @title Environmental cost of Failure for 30/10kV and 60/10kV Transformers
#' @description This function calculates environmental consequences of failure
#' Outputted in COL.
#' @param tf_asset_category String The type of Transformer
#' Options: \code{tf_asset_category = c("30kV Transformer (GM)",
#' "60kV Transformer (GM)")}.
#' @param prox_water Numeric. Specify the proximity to a water course in meters.
#' A setting of \code{"Default"} will result in a proximity factor of 1. Thus
#' assume the proximity to a water course is between 80m and 120m
#' @param bunded String. Options: \code{bunded = c("Yes", "No", "Default")}.
#' A setting of \code{"Default"} will result in a bunding factor of 1.
#' @param size_kva_mva Numeric The MVA KVA rating for the transformer
#' @param gb_ref_given optional parameter to use custom reference values
#' @export
#' @examples
#' ambiental_cof_transformer_34kv(tf_asset_category = "30kV Transformer (GM)",
#' prox_water = 95, bunded = "Yes", size_kva_mva = 20)
ambiental_cof_transformador_34kv <- function(tf_asset_category,
                                                  prox_water, bunded,
                                                  size_kva_mva = NULL,
                                                  gb_ref_given = NULL) {

  GBP_to_COL <- 1
  if (tf_asset_category == "30kV Transformer (GM)" ) {
    tf_asset_category <- "33kV Transformer (GM)"
    size_conversion <- "33/11 or 6.6kV"
  } else if (tf_asset_category == "60kV Transformer (GM)" ) {
    tf_asset_category <- "66kV Transformer (GM)"
    size_conversion <- "66/11 or 6.6kV"
  } else {
    size_conversion <- NULL
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
    `Type environment factor` = `Size` = `Lower` = `Upper` = NULL

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tf_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tf_asset_category)

  # Reference financial cost of failure -------------------------------------
  ecost <- reference_costs_of_failure_tf$`Environmental - (GBP)`

  # Type env factor -------------------------------------
  type_environmental_factor <- 1

  # Size env factor -------------------------------------
  size_environmental_factor_df <- gb_ref_taken$size_enviromental_factor

  size_environmental_factor_df <- size_environmental_factor_df %>%
    dplyr::filter(`Asset Register Category` == tf_asset_category)

  if(!is.null(size_conversion)){
    size_environmental_factor_df <- size_environmental_factor_df %>%
      dplyr::filter(`Size` == size_conversion)
  }

  if(!is.null(size_kva_mva)){
    size_environmental_factor_df <- size_environmental_factor_df %>%
      dplyr::filter(`Lower` <= size_kva_mva,
                    `Upper` > size_kva_mva)
  }

  size_environmental_factor <- size_environmental_factor_df$`Size Environmental Factor`[1]

  # Location environmetal factor table 222 ----------------------------------
  location_environ_al_factor <- gb_ref_taken$location_environ_al_factor

  location_environ_al_factor_tf <- dplyr::filter(location_environ_al_factor,
                                                 `Asset Register Category` ==
                                                   asset_category)

  if(nrow(location_environ_al_factor_tf) == 0){
    location_environmental_factor <- 1
  } else {
    # Bunded "Yes", "No", "Default" ?
    if (bunded == "Default") {
      bunding_factor <- 1
    } else if (bunded == "Yes") {
      bunding_factor <-
        location_environ_al_factor_tf$`Bunding Factor: Bunded`
    } else if (bunded == "No") {
      bunding_factor <-
        location_environ_al_factor_tf$`Bunding Factor: Not bunded`
    }

    # Proximity to water.
    if(prox_water == "Default") {
      prox_factor <- 1
    } else if (prox_water >= 40 && prox_water < 80) {
      prox_factor <- location_environ_al_factor_tf$
        `Proximity Factor: Close to Water Course (between 40m and 80m)`
    } else if (prox_water >= 80 && prox_water < 120) {
      prox_factor <- location_environ_al_factor_tf$
        `Proximity Factor: Moderately Close to Water Course (between 80m and 120m)`
    } else if (prox_water > 120) {
      prox_factor <- location_environ_al_factor_tf$
        `Proximity Factor: Not Close to Water Course (>120m) or No Oil`
    } else if (prox_water < 40) {
      prox_factor <- location_environ_al_factor_tf$
        `Proximity Factor: Very Close to Water Course (<40m)`
    }

    # Location environmental factor
    location_environmental_factor <- prox_factor * bunding_factor
  }


  environmental_consequences_factor <- (type_environmental_factor *
                                          size_environmental_factor *
                                          location_environmental_factor)

  # Environmental consequences ----------------------------------------------
  environmental_cof <- environmental_consequences_factor * ecost
  return(environmental_cof * GBP_to_COL)
}
