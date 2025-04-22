#' @importFrom magrittr %>%
#' @title Probabilidad Actual de Falla para Transformadores de 13.2kV
#' @description Esta función calcula la probabilidad
#' anual actual de falla para transformadores de 13.2kV.
#' La función es una curva cúbica basada en los
#' primeros tres términos de la serie de Taylor para una
#' función exponencial. Para más información sobre la
#' función de probabilidad de falla, consulte la sección 6
#' en la página 34 de CNAIM (2021).
#' @param tipo_transformador String. Se refiere al tipo de transformador de alta
#' tensión para el cual se realiza el cálculo. Opciones: \code{tipo_transformador =
#' c("6.6/11kV Transformer (GM)", "20kV Transformer (GM)", "Transformador 13.2kV")}.
#' La configuración predeterminada es \code{tipo_transformador = "Transformador 13.2kV"}.
#' @inheritParams duty_factor_transformer_11_20kv
#' @inheritParams location_factor
#' @inheritParams current_health
#' @param edad Numérico. La edad actual en años.
#' @param descarga_parcial Texto. Indica el nivel de descarga parcial.
#' Opciones: \code{descarga_parcial = c("Baja", "Media", "Alta (No Confirmada)",
#' "Alta (Confirmada)", "Default")}. Ver página 153, tabla 171 en CNAIM (2021).
#' @param acidez_aceite Acidez del aceite. Indica el nivel de acidez del aceite
#' en el transformador. Ver página 162, tabla 202 en CNAIM (2021).
#' @inheritParams oil_test_modifier
#' Ver página 162, tabla 204 en CNAIM (2021).
#' @param lectura_temperatura Texto. Indica la criticidad.
#' Opciones: \code{lectura_temperatura = c("Normal", "Moderadamente Alta",
#' "Muy Alta", "Default")}. Ver página 153, tabla 172 en CNAIM (2021).
#' @param condición_observada Texto. Indica la condición observada del
#' transformador. Opciones para \code{observed_condition}:
#' \code{condición_observada = c("Sin deterioro", "Deterioro específico/menor",
#' "Ligeramente deteriorado", "Cierto deterioro", "Deterioro sustancial", "Default")}.
#' Ver página 130, tabla 81 en CNAIM (2021).
#' @param humedad Numérico. La humedad dada en (ppm). Ver página 162, tabla 203 en CNAIM (2021).
#' @param resistencia_dieléctrica Numérico. La resistencia dieléctrica dada en (kV).
#' Ver página 162, tabla 205 en CNAIM (2021).
#' @param índice_corrosión Entero.
#' Especifica la categoría del índice de corrosión, 1-5.
#' @param gb_ref_given Parámetro opcional para usar valores de referencia personalizados.
#' @return DataFrame Probabilidad actual de falla
#' anual por kilómetro junto con el puntaje de salud actual.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Probabilidad actual de falla para un transformador de 13.2 kV
#' pof_transformador_13_2kv(tipo_transformador = "Transformador 13.2kV",
#'                          utilización_pct = "Default",
#'                          emplazamiento = "Default",
#'                          altitud_m = "Default",
#'                          distancia_costa_km = "Default",
#'                          índice_corrosión = "Default",
#'                          edad = 10,
#'                          descarga_parcial = "Default",
#'                          lectura_temperatura = "Default",
#'                          condición_observada = "Default",
#'                          factor_confiabilidad = "Default",
#'                          humedad = "Default",
#'                          acidez_aceite = "Default",
#'                          resistencia_dieléctrica = "Default")

pof_transformador_13_2kv <- function(tipo_transformador = "Transformador 13.2kV",
                                     utilización_pct = "Default",
                                     emplazamiento = "Default",
                                     altitud_m = "Default",
                                     distancia_costa_km = "Default",
                                     índice_corrosión = "Default",
                                     edad,
                                     descarga_parcial = "Default",
                                     lectura_temperatura = "Default",
                                     condición_observada = "Default",
                                     factor_confiabilidad = "Default",
                                     humedad = "Default",
                                     acidez_aceite = "Default",
                                     resistencia_dieléctrica = "Default",
                                     gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` =
    `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
    `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
  # due to NSE notes in R CMD check
  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }
  
  # Ref. table Categorisation of Assets and Generic Terms for Assets  --
  asset_type <- tipo_transformador

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == asset_type) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  generic_term_1 <- gb_ref_taken$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...1`) %>% dplyr::pull()

  generic_term_2 <- gb_ref_taken$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...2`) %>% dplyr::pull()

  # Normal expected life for 13.2 kV transformer ------------------------------
  normal_expected_life <- gb_ref_taken$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == asset_type) %>%
    dplyr::pull()

  # Constants C and K for PoF function --------------------------------------
  k <- gb_ref_taken$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    asset_category) %>% dplyr::select(`K-Value (%)`) %>%
    dplyr::pull()/100

  c <- gb_ref_taken$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    asset_category) %>% dplyr::select(`C-Value`) %>% dplyr::pull()

  # Duty factor -------------------------------------------------------------
  duty_factor_tf_11kv <- duty_factor_transformer_11_20kv(utilización_pct)

  # Location factor ----------------------------------------------------
  location_factor_transformer <- location_factor(emplazamiento,
                                                 altitud_m,
                                                 distancia_costa_km,
                                                 índice_corrosión,
                                                 asset_type)

  # Expected life for 13.2 kV transformer ------------------------------
  expected_life_years <- expected_life(normal_expected_life,
                                       duty_factor_tf_11kv,
                                       location_factor_transformer)

  # b1 (Initial Ageing Rate) ------------------------------------------------
  b1 <- beta_1(expected_life_years)

  # Initial health score ----------------------------------------------------
  initial_health_score <- initial_health(b1, edad)
browser()
  ## NOTE
  # Typically, the Health Score Collar is 0.5 and
  # Health Score Cap is 10, implying no overriding
  # of the Health Score. However, in some instances
  # these parameters are set to other values in the
  # Health Score Modifier calibration tables.
  # These overriding values are shown in Table 35 to Table 202
  # and Table 207 in Appendix B.

  # Measured condition inputs ---------------------------------------------
  mcm_mmi_cal_df <-
    gb_ref_taken$measured_cond_modifier_mmi_cal

  mcm_mmi_cal_df <-
    mcm_mmi_cal_df[which(mcm_mmi_cal_df$`Asset Category` == asset_category), ]

  factor_divider_1 <-
    as.numeric(mcm_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Factor Divider 1`)

  factor_divider_2 <-
    as.numeric(mcm_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Factor Divider 2`)

  max_no_combined_factors <-
    as.numeric(mcm_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`
    )

  # Partial discharge -------------------------------------------------------
  mci_hv_tf_partial_discharge <-
    gb_ref_taken$mci_hv_tf_partial_discharge

  ci_factor_partial_discharge <-
    mci_hv_tf_partial_discharge$`Condition Input Factor`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial)]

  ci_cap_partial_discharge <-
    mci_hv_tf_partial_discharge$`Condition Input Cap`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial)]

  ci_collar_partial_discharge <-
    mci_hv_tf_partial_discharge$`Condition Input Collar`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial)]

  # Oil test modifier -------------------------------------------------------
  oil_test_mod <- oil_test_modifier(humedad,
                                    acidez_aceite,
                                    resistencia_dieléctrica)

  # Temperature readings ----------------------------------------------------
  mci_hv_tf_temp_readings <-
    gb_ref_taken$mci_hv_tf_temp_readings

  ci_factor_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Factor`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        lectura_temperatura)]

  ci_cap_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Cap`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        lectura_temperatura)]

  ci_collar_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Collar`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        lectura_temperatura)]

  # measured condition factor -----------------------------------------------
  factors <- c(ci_factor_partial_discharge,
               oil_test_mod$oil_condition_factor,
               ci_factor_temp_reading)

  measured_condition_factor <- mmi(factors,
                                   factor_divider_1,
                                   factor_divider_2,
                                   max_no_combined_factors)

  # Measured condition cap --------------------------------------------------
  caps <- c(ci_cap_partial_discharge,
            oil_test_mod$oil_condition_cap,
            ci_cap_temp_reading)
  measured_condition_cap <- min(caps)

  # Measured condition collar -----------------------------------------------
  collars <- c(ci_collar_partial_discharge,
               oil_test_mod$oil_condition_collar,
               ci_collar_temp_reading)
  measured_condition_collar <- max(collars)

  # Measured condition modifier ---------------------------------------------
  measured_condition_modifier <- data.frame(measured_condition_factor,
                                            measured_condition_cap,
                                            measured_condition_collar)

  # Observed condition inputs ---------------------------------------------
  oci_mmi_cal_df <-
    gb_ref_taken$observed_cond_modifier_mmi_cal

  oci_mmi_cal_df <-
    oci_mmi_cal_df[which(oci_mmi_cal_df$`Asset Category` == asset_category), ]

  factor_divider_1 <-
    as.numeric(oci_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Factor Divider 1`)

  factor_divider_2 <-
    as.numeric(oci_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Factor Divider 2`)

  max_no_combined_factors <-
    as.numeric(oci_mmi_cal_df$
                 `Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`
    )

  oci_hv_tf_tf_ext_cond_df <-
    gb_ref_taken$oci_hv_tf_tf_ext_cond

  ci_factor_ext_cond <-
    oci_hv_tf_tf_ext_cond_df$`Condition Input Factor`[which(
      oci_hv_tf_tf_ext_cond_df$`Condition Criteria: Observed Condition` ==
        condición_observada)]

  ci_cap_ext_cond <-
    oci_hv_tf_tf_ext_cond_df$`Condition Input Cap`[which(
      oci_hv_tf_tf_ext_cond_df$`Condition Criteria: Observed Condition` ==
        condición_observada)]

  ci_collar_ext_cond <-
    oci_hv_tf_tf_ext_cond_df$`Condition Input Collar`[which(
      oci_hv_tf_tf_ext_cond_df$`Condition Criteria: Observed Condition` ==
        condición_observada)]

  # Observed condition factor -----------------------------------------------
  observed_condition_factor <- mmi(factors = ci_factor_ext_cond,
                                   factor_divider_1,
                                   factor_divider_2,
                                   max_no_combined_factors)

  # Observed condition cap ---------------------------------------------
  observed_condition_cap <- ci_cap_ext_cond

  # Observed condition collar ---------------------------------------------
  observed_condition_collar <- ci_collar_ext_cond

  # Observed condition modifier ---------------------------------------------
  observed_condition_modifier <- data.frame(observed_condition_factor,
                                            observed_condition_cap,
                                            observed_condition_collar)

  # Health score factor ---------------------------------------------------
  health_score_factor <-
    health_score_excl_ehv_132kv_tf(observed_condition_factor,
                                   measured_condition_factor)

  # Health score cap --------------------------------------------------------
  health_score_cap <- min(observed_condition_cap, measured_condition_cap)

  # Health score collar -----------------------------------------------------
  health_score_collar <-  max(observed_condition_collar,
                              measured_condition_collar)

  # Health score modifier ---------------------------------------------------
  health_score_modifier <- data.frame(health_score_factor,
                                      health_score_cap,
                                      health_score_collar)

  # Current health score ----------------------------------------------------
  current_health_score <-
    current_health(initial_health_score,
                   health_score_modifier$health_score_factor,
                   health_score_modifier$health_score_cap,
                   health_score_modifier$health_score_collar,
                   reliability_factor = factor_confiabilidad)

  # Probability of failure for the 6.6/11kV and 20kV transformer today -----------------
  probability_of_failure <- k *
    (1 + (c * current_health_score) +
       (((c * current_health_score)^2) / factorial(2)) +
       (((c * current_health_score)^3) / factorial(3)))

  return(data.frame(pof = probability_of_failure, chs = current_health_score))
}