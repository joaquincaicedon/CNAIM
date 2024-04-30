#' @importFrom magrittr %>%
#' @title Future Probability of Failure for 33/10kV and 66/10kV Transformers
#' @description This function calculates the future
#' annual probability of failure for 33/10kV and 66/10kV transformers.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 34 in CNAIM (2021).
#' @inheritParams pof_transformer_33_66kv
#' @param año_final_simulacion Numeric. The last year of simulating probability
#' of failure. Default is 100.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return DataFrame. Future probability of failure
#' along with future health score
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Future probability of failure for a 66/10kV transformer
#' pof_future_transformer_33_66kv(tipo_transformador = "66kV Transformer (GM)",
#' año_de_fabricación = 1980,
#' utilización = "Default",
#' nro_taps = "Default",
#' emplazamiento = "Default",
#' altura_m = "Default",
#' distancia_costa_km = "Default",
#' categoria_indice_corrosion = "Default",
#' edad_TP = 43,
#' edad_CT = 43,
#' descarga_parcial_TP = "Default",
#' descarga_parcial_CT = "Default",
#' Temperatura_lectura = "Default",
#' tanque_principal = "Default",
#' ventiladores_radiador = "Default",
#' pasatapas = "Default",
#' quiosco = "Default",
#' Caja_cables = "Default",
#' externo_tap = "Default",
#' interno_tap = "Default",
#' Condicion_mecanismo = "Default",
#' Contactos_derivador = "Default",
#' Trenzas_derivador = "Default",
#' humedad = "Default",
#' acidez = "Default",
#' Rigidez_dielectrica = "Default",
#' hydrogen = "Default",
#' metano = "Default",
#' etileno = "Default",
#' etano = "Default",
#' acetileno = "Default",
#' hydrogen_pre = "Default",
#' metano_pre = "Default",
#' etileno_pre = "Default",
#' etano_pre = "Default",
#' acetileno_pre = "Default",
#' furfuraldehído = "Default",
#' Factor_confiabilidad = "Default",
#' año_final_simulacion = 100)
pof_futuro_transformador_34_5kv <- function(tipo_transformador = "66kV Transformer (GM)",
                                           año_de_fabricación,
                                           utilización = "Default",
                                           nro_taps = "Default",
                                           emplazamiento = "Default",
                                           altura_m = "Default",
                                           distancia_costa_km = "Default",
                                           categoria_indice_corrosion = "Default",
                                           edad_TP,
                                           edad_CT,
                                           descarga_parcial_TP = "Default",
                                           descarga_parcial_CT = "Default",
                                           Temperatura_lectura = "Default",
                                           tanque_principal = "Default",
                                           ventiladores_radiador = "Default",
                                           pasatapas = "Default",
                                           quiosco = "Default",
                                           Caja_cables = "Default",
                                           externo_tap = "Default",
                                           interno_tap = "Default",
                                           Condicion_mecanismo = "Default",
                                           Contactos_derivador = "Default",
                                           Trenzas_derivador = "Default",
                                           humedad = "Default",
                                           acidez = "Default",
                                           Rigidez_dielectrica = "Default",
                                           hidrógeno = "Default",
                                           metano = "Default",
                                           etileno = "Default",
                                           etano = "Default",
                                           acetileno = "Default",
                                           hidrógeno_pre = "Default",
                                           metano_pre = "Default",
                                           etileno_pre = "Default",
                                           etano_pre = "Default",
                                           acetileno_pre = "Default",
                                           furfuraldehído = "Default",
                                           Factor_confiabilidad = "Default",
                                           año_final_simulacion = 100,
                                           gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` =
    `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
    `K-Value (%)` = `C-Value` = `Asset Register  Category` =
    `Asset Category` = `Sub-division` = NULL
  # due to NSE notes in R CMD check
  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  # Ref. table Categorisation of Assets and Generic Terms for Assets  --

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tipo_transformador) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  generic_term_1 <- gb_ref_taken$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...1`) %>% dplyr::pull()

  generic_term_2 <- gb_ref_taken$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...2`) %>% dplyr::pull()

  # Normal expected life for transformer -----------------------------

  if (año_de_fabricación < 1980) {
    sub_division <- "Transformer - Pre 1980"
  } else {
    sub_division <- "Transformer - Post 1980"

  }

  normal_expected_life_tf <- gb_ref_taken$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == tipo_transformador & `Sub-division` ==
                    sub_division) %>%
    dplyr::pull()

  # Normal expected life for tapchanger -----------------------------

  normal_expected_life_tc <- gb_ref_taken$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == tipo_transformador & `Sub-division` ==
                    "Tapchanger") %>%
    dplyr::pull()

  # Constants C and K for PoF function --------------------------------------
  k <- gb_ref_taken$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    'EHV Transformer/ 132kV Transformer') %>% dplyr::select(`K-Value (%)`) %>%
    dplyr::pull()/100

  c <- gb_ref_taken$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    'EHV Transformer/ 132kV Transformer') %>% dplyr::select(`C-Value`) %>% dplyr::pull()

  # Duty factor -------------------------------------------------------------
  duty_factor_tf_11kv <- duty_factor_transformer_33_66kv(utilización,
                                                         nro_taps)
  duty_factor_tf <-
    duty_factor_tf_11kv$duty_factor[which(duty_factor_tf_11kv$category ==
                                            "transformer")]
  duty_factor_tc <-
    duty_factor_tf_11kv$duty_factor[which(duty_factor_tf_11kv$category ==
                                            "tapchanger")]


  # Location factor ----------------------------------------------------
  location_factor_transformer <- location_factor(emplazamiento,
                                                 altura_m,
                                                 distancia_costa_km,
                                                 categoria_indice_corrosion,
                                                 asset_type = tipo_transformador)

  # Expected life for transformer ------------------------------
  expected_life_years_tf <- expected_life(normal_expected_life =
                                            normal_expected_life_tf,
                                          duty_factor_tf,
                                          location_factor_transformer)

  # Expected life for tapchanger ------------------------------
  expected_life_years_tc <- expected_life(normal_expected_life =
                                            normal_expected_life_tc,
                                          duty_factor_tc,
                                          location_factor_transformer)

  # b1 (Initial Ageing Rate) ------------------------------------------------
  b1_tf <- beta_1(expected_life_years_tf)
  b1_tc <- beta_1(expected_life_years_tc)

  # Initial health score ----------------------------------------------------
  initial_health_score_tf <- initial_health(b1_tf, edad_TP)
  initial_health_score_tc <- initial_health(b1_tc, edad_CT)

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
    mcm_mmi_cal_df[which(mcm_mmi_cal_df$`Asset Category` == "EHV Transformer (GM)"), ]


  factor_divider_1_tf <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(mcm_mmi_cal_df$Subcomponent == "Main Transformer")
    ])

  factor_divider_1_tc <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(mcm_mmi_cal_df$Subcomponent == "Tapchanger")
    ])


  factor_divider_2_tf <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(mcm_mmi_cal_df$Subcomponent == "Main Transformer")
    ])

  factor_divider_2_tc <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(mcm_mmi_cal_df$Subcomponent == "Tapchanger")
    ])

  max_no_combined_factors_tf <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(mcm_mmi_cal_df$Subcomponent == "Main Transformer")
    ])

  max_no_combined_factors_tc <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(mcm_mmi_cal_df$Subcomponent == "Tapchanger")
    ])


  # Partial discharge transformer ----------------------------------------------
  mci_hv_tf_partial_discharge <-
    gb_ref_taken$mci_ehv_tf_main_tf_prtl_dis

  ci_factor_descarga_parcial_TP <-
    mci_hv_tf_partial_discharge$`Condition Input Factor`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial_TP)]

  ci_cap_descarga_parcial_TP <-
    mci_hv_tf_partial_discharge$`Condition Input Cap`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial_TP)]

  ci_collar_descarga_parcial_TP <-
    mci_hv_tf_partial_discharge$`Condition Input Collar`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial_TP)]


  # Partial discharge tapchanger ------------------------------------------------
  mci_hv_tf_partial_discharge_tc <-
    gb_ref_taken$mci_ehv_tf_tapchngr_prtl_dis

  ci_factor_descarga_parcial_CT <-
    mci_hv_tf_partial_discharge_tc$`Condition Input Factor`[which(
      mci_hv_tf_partial_discharge_tc$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial_CT)]

  ci_cap_descarga_parcial_CT <-
    mci_hv_tf_partial_discharge_tc$`Condition Input Cap`[which(
      mci_hv_tf_partial_discharge_tc$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial_CT)]

  ci_collar_descarga_parcial_CT <-
    mci_hv_tf_partial_discharge_tc$`Condition Input Collar`[which(
      mci_hv_tf_partial_discharge_tc$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial_CT)]


  # Temperature readings ----------------------------------------------------
  mci_hv_tf_temp_readings <-
    gb_ref_taken$mci_ehv_tf_temp_readings

  ci_factor_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Factor`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        Temperatura_lectura)]

  ci_cap_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Cap`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        Temperatura_lectura)]

  ci_collar_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Collar`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        Temperatura_lectura)]

  # measured condition factor -----------------------------------------------
  factors_tf <- c(ci_factor_descarga_parcial_TP,
                  ci_factor_temp_reading)

  measured_condition_factor_tf <- mmi(factors_tf,
                                      factor_divider_1_tf,
                                      factor_divider_2_tf,
                                      max_no_combined_factors_tf)


  measured_condition_factor_tc <- mmi(ci_factor_descarga_parcial_CT,
                                      factor_divider_1_tc,
                                      factor_divider_2_tc,
                                      max_no_combined_factors_tc)

  # Measured condition cap --------------------------------------------------
  caps_tf <- c(ci_cap_descarga_parcial_TP,
               ci_cap_temp_reading)
  measured_condition_cap_tf <- min(caps_tf)


  measured_condition_cap_tc <- ci_cap_descarga_parcial_CT

  # Measured condition collar -----------------------------------------------
  collars_tf <- c(ci_collar_descarga_parcial_TP,
                  ci_collar_temp_reading)
  measured_condition_collar_tf <- max(collars_tf)

  measured_condition_collar_tc <- ci_collar_descarga_parcial_CT

  # Measured condition modifier ---------------------------------------------
  measured_condition_modifier_tf <- data.frame(measured_condition_factor_tf,
                                               measured_condition_cap_tf,
                                               measured_condition_collar_tf)

  measured_condition_modifier_tc <- data.frame(measured_condition_factor_tc,
                                               measured_condition_cap_tc,
                                               measured_condition_collar_tc)

  # Observed condition inputs ---------------------------------------------
  oci_mmi_cal_df <-
    gb_ref_taken$observed_cond_modifier_mmi_cal %>%
    dplyr::filter(`Asset Category` == "EHV Transformer (GM)")

  factor_divider_1_tf_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])

  factor_divider_1_tc_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Tapchanger") ])

  factor_divider_2_tf_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])

  factor_divider_2_tc_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Tapchanger") ])


  max_no_combined_factors_tf_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Main Transformer") ])

  max_no_combined_factors_tc_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(oci_mmi_cal_df$Subcomponent ==
              "Tapchanger") ])


  # Transformer -------------------------------------------------------------

  # Main tank condition
  oci_ehv_tf_main_tank_cond <-
    gb_ref_taken$oci_ehv_tf_main_tank_cond

  Oi_collar_tanque_principal <-
    oci_ehv_tf_main_tank_cond$`Condition Input Collar`[which(
      oci_ehv_tf_main_tank_cond$`Condition Criteria: Observed Condition` ==
        tanque_principal)]

  Oi_cap_tanque_principal <-
    oci_ehv_tf_main_tank_cond$`Condition Input Cap`[which(
      oci_ehv_tf_main_tank_cond$`Condition Criteria: Observed Condition` ==
        tanque_principal)]

  Oi_factor_tanque_principal <-
    oci_ehv_tf_main_tank_cond$`Condition Input Factor`[which(
      oci_ehv_tf_main_tank_cond$`Condition Criteria: Observed Condition` ==
        tanque_principal)]

  # Coolers/Radiator condition

  oci_ehv_tf_cooler_radiatr_cond <-
    gb_ref_taken$oci_ehv_tf_cooler_radiatr_cond

  Oi_collar_ventiladores_radiador <-
    oci_ehv_tf_cooler_radiatr_cond$`Condition Input Collar`[which(
      oci_ehv_tf_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        ventiladores_radiador)]

  Oi_cap_ventiladores_radiador <-
    oci_ehv_tf_cooler_radiatr_cond$`Condition Input Cap`[which(
      oci_ehv_tf_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        ventiladores_radiador)]

  Oi_factor_ventiladores_radiador <-
    oci_ehv_tf_cooler_radiatr_cond$`Condition Input Factor`[which(
      oci_ehv_tf_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        ventiladores_radiador)]


  # pasatapas

  oci_ehv_tf_bushings_cond <-
    gb_ref_taken$oci_ehv_tf_bushings_cond

  Oi_collar_pasatapas <-
    oci_ehv_tf_bushings_cond$`Condition Input Collar`[which(
      oci_ehv_tf_bushings_cond$`Condition Criteria: Observed Condition` ==
        pasatapas)]

  Oi_cap_pasatapas <-
    oci_ehv_tf_bushings_cond$`Condition Input Cap`[which(
      oci_ehv_tf_bushings_cond$`Condition Criteria: Observed Condition` ==
        pasatapas)]

  Oi_factor_pasatapas <-
    oci_ehv_tf_bushings_cond$`Condition Input Factor`[which(
      oci_ehv_tf_bushings_cond$`Condition Criteria: Observed Condition` ==
        pasatapas)]

  # quiosco

  oci_ehv_tf_kiosk_cond <-
    gb_ref_taken$oci_ehv_tf_kiosk_cond

  Oi_collar_quiosco <-
    oci_ehv_tf_kiosk_cond$`Condition Input Collar`[which(
      oci_ehv_tf_kiosk_cond$`Condition Criteria: Observed Condition` ==
        quiosco)]

  Oi_cap_quiosco <-
    oci_ehv_tf_kiosk_cond$`Condition Input Cap`[which(
      oci_ehv_tf_kiosk_cond$`Condition Criteria: Observed Condition` ==
        quiosco)]

  Oi_factor_quiosco <-
    oci_ehv_tf_kiosk_cond$`Condition Input Factor`[which(
      oci_ehv_tf_kiosk_cond$`Condition Criteria: Observed Condition` ==
        quiosco)]


  # Cable box
  oci_ehv_tf_cable_boxes_cond <-
    gb_ref_taken$oci_ehv_tf_cable_boxes_cond

  Oi_collar_Caja_cables <-
    oci_ehv_tf_cable_boxes_cond$`Condition Input Collar`[which(
      oci_ehv_tf_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        Caja_cables)]

  Oi_cap_Caja_cables <-
    oci_ehv_tf_cable_boxes_cond$`Condition Input Cap`[which(
      oci_ehv_tf_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        Caja_cables)]

  Oi_factor_Caja_cables <-
    oci_ehv_tf_cable_boxes_cond$`Condition Input Factor`[which(
      oci_ehv_tf_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        Caja_cables)]


  # Tapchanger --------------------------------------------------------------

  # External condition
  oci_ehv_tf_tapchanger_ext_cond <-
    gb_ref_taken$oci_ehv_tf_tapchanger_ext_cond

  Oi_collar_externo_tap <-
    oci_ehv_tf_tapchanger_ext_cond$`Condition Input Collar`[which(
      oci_ehv_tf_tapchanger_ext_cond$`Condition Criteria: Observed Condition` ==
        externo_tap)]

  Oi_cap_externo_tap <-
    oci_ehv_tf_tapchanger_ext_cond$`Condition Input Cap`[which(
      oci_ehv_tf_tapchanger_ext_cond$`Condition Criteria: Observed Condition` ==
        externo_tap)]

  Oi_factor_externo_tap <-
    oci_ehv_tf_tapchanger_ext_cond$`Condition Input Factor`[which(
      oci_ehv_tf_tapchanger_ext_cond$`Condition Criteria: Observed Condition` ==
        externo_tap)]


  # Internal condition
  oci_ehv_tf_int_cond <-
    gb_ref_taken$oci_ehv_tf_int_cond

  Oi_collar_interno_tap <-
    oci_ehv_tf_int_cond$`Condition Input Collar`[which(
      oci_ehv_tf_int_cond$`Condition Criteria: Observed Condition` ==
        interno_tap)]

  Oi_cap_interno_tap <-
    oci_ehv_tf_int_cond$`Condition Input Cap`[which(
      oci_ehv_tf_int_cond$`Condition Criteria: Observed Condition` ==
        interno_tap)]

  Oi_factor_interno_tap <-
    oci_ehv_tf_int_cond$`Condition Input Factor`[which(
      oci_ehv_tf_int_cond$`Condition Criteria: Observed Condition` ==
        interno_tap)]

  # Drive mechanism
  oci_ehv_tf_drive_mechnism_cond <-
    gb_ref_taken$oci_ehv_tf_drive_mechnism_cond

  Oi_collar_Condicion_mecanismo <-
    oci_ehv_tf_drive_mechnism_cond$`Condition Input Collar`[which(
      oci_ehv_tf_drive_mechnism_cond$`Condition Criteria: Observed Condition` ==
        Condicion_mecanismo)]

  Oi_cap_Condicion_mecanismo <-
    oci_ehv_tf_drive_mechnism_cond$`Condition Input Cap`[which(
      oci_ehv_tf_drive_mechnism_cond$`Condition Criteria: Observed Condition` ==
        Condicion_mecanismo)]

  Oi_factor_Condicion_mecanismo <-
    oci_ehv_tf_drive_mechnism_cond$`Condition Input Factor`[which(
      oci_ehv_tf_drive_mechnism_cond$`Condition Criteria: Observed Condition` ==
        Condicion_mecanismo)]

  # Selecter diverter contacts
  oci_ehv_tf_cond_select_divrter_cst <-
    gb_ref_taken$oci_ehv_tf_cond_select_div_cts

  Oi_collar_Contactos_derivador <-
    oci_ehv_tf_cond_select_divrter_cst$`Condition Input Collar`[which(
      oci_ehv_tf_cond_select_divrter_cst$`Condition Criteria: Observed Condition` ==
        Contactos_derivador)]

  Oi_cap_Contactos_derivador <-
    oci_ehv_tf_cond_select_divrter_cst$`Condition Input Cap`[which(
      oci_ehv_tf_cond_select_divrter_cst$`Condition Criteria: Observed Condition` ==
        Contactos_derivador)]

  Oi_factor_Contactos_derivador <-
    oci_ehv_tf_cond_select_divrter_cst$`Condition Input Factor`[which(
      oci_ehv_tf_cond_select_divrter_cst$`Condition Criteria: Observed Condition` ==
        Contactos_derivador)]


  # Selecter diverter braids
  oci_ehv_tf_cond_select_divrter_brd <-
    gb_ref_taken$oci_ehv_tf_cond_select_div_brd

  Oi_collar_Trenzas_derivador <-
    oci_ehv_tf_cond_select_divrter_brd$`Condition Input Collar`[which(
      oci_ehv_tf_cond_select_divrter_brd$`Condition Criteria: Observed Condition` ==
        Trenzas_derivador)]

  Oi_cap_Trenzas_derivador <-
    oci_ehv_tf_cond_select_divrter_brd$`Condition Input Cap`[which(
      oci_ehv_tf_cond_select_divrter_brd$`Condition Criteria: Observed Condition` ==
        Trenzas_derivador)]

  Oi_factor_Trenzas_derivador <-
    oci_ehv_tf_cond_select_divrter_brd$`Condition Input Factor`[which(
      oci_ehv_tf_cond_select_divrter_brd$`Condition Criteria: Observed Condition` ==
        Trenzas_derivador)]


  # Observed condition factor --------------------------------------

  # Transformer
  factors_tf_obs <- c(Oi_factor_tanque_principal,
                      Oi_factor_ventiladores_radiador,
                      Oi_factor_pasatapas,
                      Oi_factor_quiosco,
                      Oi_factor_Caja_cables)

  observed_condition_factor_tf <- mmi(factors_tf_obs,
                                      factor_divider_1_tf_obs,
                                      factor_divider_2_tf_obs,
                                      max_no_combined_factors_tf_obs)


  # Tapchanger

  factors_tc_obs <- c(Oi_factor_externo_tap,
                      Oi_factor_interno_tap,
                      Oi_factor_Condicion_mecanismo,
                      Oi_factor_Contactos_derivador,
                      Oi_factor_Trenzas_derivador)

  observed_condition_factor_tc <- mmi(factors_tc_obs,
                                      factor_divider_1_tc_obs,
                                      factor_divider_2_tc_obs,
                                      max_no_combined_factors_tc_obs)



  # Observed condition cap -----------------------------------------

  # Transformer
  caps_tf_obs <- c(Oi_cap_tanque_principal,
                   Oi_cap_ventiladores_radiador,
                   Oi_cap_pasatapas,
                   Oi_cap_quiosco,
                   Oi_cap_Caja_cables)

  observed_condition_cap_tf <- min(caps_tf_obs)

  # Tapchanger

  caps_tc_obs <- c(Oi_cap_externo_tap,
                   Oi_cap_interno_tap,
                   Oi_cap_Condicion_mecanismo,
                   Oi_cap_Contactos_derivador,
                   Oi_cap_Trenzas_derivador)

  observed_condition_cap_tc <- min(caps_tc_obs)


  # Observed condition collar ---------------------------------------

  # Transformer
  collars_tf_obs <- c(Oi_collar_tanque_principal,
                      Oi_collar_ventiladores_radiador,
                      Oi_collar_pasatapas,
                      Oi_collar_quiosco,
                      Oi_collar_Caja_cables)

  observed_condition_collar_tf <- max(collars_tf_obs)

  # Tapchanger

  collars_tc_obs <- c(Oi_collar_externo_tap,
                      Oi_collar_interno_tap,
                      Oi_collar_Condicion_mecanismo,
                      Oi_collar_Contactos_derivador,
                      Oi_collar_Trenzas_derivador)

  observed_condition_collar_tc <- max(collars_tc_obs)


  # Observed condition modifier ---------------------------------------------

  # Transformer
  observed_condition_modifier_tf <- data.frame(observed_condition_factor_tf,
                                               observed_condition_cap_tf,
                                               observed_condition_collar_tf)

  # Tapchanger
  observed_condition_modifier_tc <- data.frame(observed_condition_factor_tc,
                                               observed_condition_cap_tc,
                                               observed_condition_collar_tc)


  # Oil test modifier -------------------------------------------------------
  oil_test_mod <- oil_test_modifier(humedad,
                                    acidez,
                                    Rigidez_dielectrica)

  # DGA test modifier -------------------------------------------------------
  dga_test_mod <- dga_test_modifier(hydrogen,
                                    metano,
                                    etileno,
                                    etano,
                                    acetileno,
                                    hydrogen_pre,
                                    metano_pre,
                                    etileno_pre,
                                    etano_pre,
                                    acetileno_pre)
  # FFA test modifier -------------------------------------------------------
  ffa_test_mod <- ffa_test_modifier(furfuraldehído)

  # Health score factor ---------------------------------------------------

  health_score_factor_for_tf <-  gb_ref_taken$health_score_factor_for_tf
  health_score_factor_tapchanger <-  gb_ref_taken$health_score_factor_tapchanger


  # Transformer

  factor_divider_1_tf_health <-
    health_score_factor_for_tf$`Parameters for Combination Using MMI Technique - Factor Divider 1`

  factor_divider_2_tf_health <-
    health_score_factor_for_tf$`Parameters for Combination Using MMI Technique - Factor Divider 2`

  max_no_combined_factors_tf_health <-
    health_score_factor_for_tf$`Parameters for Combination Using MMI Technique - Max. No. of Condition Factors`

  # Tapchanger
  factor_divider_1_tc_health <-
    health_score_factor_tapchanger$`Parameters for Combination Using MMI Technique - Factor Divider 1`

  factor_divider_2_tc_health <-
    health_score_factor_tapchanger$`Parameters for Combination Using MMI Technique - Factor Divider 2`

  max_no_combined_factors_tc_health <-
    health_score_factor_tapchanger$`Parameters for Combination Using MMI Technique - Max. No. of Condition Factors`


  # Health score modifier -----------------------------------------------------

  # Transformer
  obs_tf_factor <- observed_condition_modifier_tf$observed_condition_factor_tf
  mea_tf_factor <- measured_condition_modifier_tf$measured_condition_factor_tf
  oil_factor <- oil_test_mod$oil_condition_factor
  dga_factor <- dga_test_mod$dga_test_factor
  ffa_factor <- ffa_test_mod$ffa_test_factor

  factors_tf_health <- c(obs_tf_factor,
                         mea_tf_factor,
                         oil_factor,
                         dga_factor,
                         ffa_factor)

  health_score_factor_tf <- mmi(factors_tf_health,
                                factor_divider_1_tf_health,
                                factor_divider_2_tf_health,
                                max_no_combined_factors_tf_health)

  # tapchanger
  obs_tc_factor <- observed_condition_modifier_tc$observed_condition_factor_tc
  mea_tc_factor <- measured_condition_modifier_tc$measured_condition_factor_tc


  factors_tc_health <- c(obs_tc_factor,
                         mea_tc_factor,
                         oil_factor)


  health_score_factor_tc <- mmi(factors_tc_health,
                                factor_divider_1_tc_health,
                                factor_divider_2_tc_health,
                                max_no_combined_factors_tc_health)

  # Health score cap --------------------------------------------------------

  # Transformer
  health_score_cap_tf <- min(observed_condition_modifier_tf$observed_condition_cap_tf,
                             measured_condition_modifier_tf$measured_condition_cap_tf,
                             oil_test_mod$oil_condition_cap,
                             dga_test_mod$dga_test_cap,
                             ffa_test_mod$ffa_test_cap)

  # Tapchanger
  health_score_cap_tc <- min(observed_condition_modifier_tc$observed_condition_cap_tc,
                             measured_condition_modifier_tc$measured_condition_cap_tc,
                             oil_test_mod$oil_condition_cap)


  # Health score collar -----------------------------------------------------
  # Transformer
  health_score_collar_tf <- max(observed_condition_modifier_tf$observed_condition_collar_tf,
                                measured_condition_modifier_tf$measured_condition_collar_tf,
                                oil_test_mod$oil_condition_collar,
                                dga_test_mod$dga_test_collar,
                                ffa_test_mod$ffa_test_collar)

  # Tapchanger
  health_score_collar_tc <- max(observed_condition_modifier_tc$observed_condition_collar_tc,
                                measured_condition_modifier_tc$measured_condition_collar_tc,
                                oil_test_mod$oil_condition_collar)

  # Health score modifier ---------------------------------------------------

  # transformer
  health_score_modifier_tf <- data.frame(health_score_factor_tf,
                                         health_score_cap_tf,
                                         health_score_collar_tf)
  # Tapchanger
  health_score_modifier_tc <- data.frame(health_score_factor_tc,
                                         health_score_cap_tc,
                                         health_score_collar_tc)

  # Current health score ----------------------------------------------------

  # Transformer

  current_health_score <-
    max(current_health(initial_health_score_tf,
                       health_score_modifier_tf$health_score_factor_tf,
                       health_score_modifier_tf$health_score_cap_tf,
                       health_score_modifier_tf$health_score_collar,
                       reliability_factor = Factor_confiabilidad),
        current_health(initial_health_score_tf,
                       health_score_modifier_tc$health_score_factor_tc,
                       health_score_modifier_tc$health_score_cap_tc,
                       health_score_modifier_tc$health_score_collar_tc,
                       reliability_factor = Factor_confiabilidad))

  # Probability of failure for the 6.6/11 kV transformer today -----------------
  probability_of_failure <- k *
    (1 + (c * current_health_score) +
       (((c * current_health_score)^2) / factorial(2)) +
       (((c * current_health_score)^3) / factorial(3)))

  # Future probability of failure -------------------------------------------
  # the Health Score of a new asset
  H_new <- 0.5
  # the Health Score of the asset when it reaches its Expected Life

  # Transformer
  current_health_score_tf <-
    current_health(initial_health_score_tf,
                   health_score_modifier_tf$health_score_factor_tf,
                   health_score_modifier_tf$health_score_cap_tf,
                   health_score_modifier_tf$health_score_collar,
                   reliability_factor = Factor_confiabilidad)

  # Tapchanger
  current_health_score_tc <-
    current_health(initial_health_score_tf,
                   health_score_modifier_tc$health_score_factor_tc,
                   health_score_modifier_tc$health_score_cap_tc,
                   health_score_modifier_tc$health_score_collar_tc,
                   reliability_factor = Factor_confiabilidad)


  b2_tf <- beta_2(current_health_score_tf, age = edad_TP)
  b2_tc <- beta_2(current_health_score_tc, age = edad_CT)

  # Transformer
  if (b2_tf > 2*b1_tf){
    b2_tf <- b1_tf * 2
  } else if (current_health_score_tf == 0.5){
    b2_tf <- b1_tf
  }

  if (current_health_score_tf < 2) {
    ageing_reduction_factor_tf <- 1
  } else if (current_health_score_tf <= 5.5) {
    ageing_reduction_factor_tf <- ((current_health_score_tf - 2)/7) + 1
  } else {
    ageing_reduction_factor_tf <- 1.5
  }

  # Tapchanger
  if (b2_tc > 2*b1_tc){
    b2_tc <- b1_tc*2
  } else if (current_health_score_tc == 0.5){
    b2_tc <- b1_tc
  }

  if (current_health_score_tc < 2) {
    ageing_reduction_factor_tc <- 1
  } else if (current_health_score_tc <= 5.5) {
    ageing_reduction_factor_tc <- ((current_health_score_tc - 2)/7) + 1
  } else {
    ageing_reduction_factor_tc <- 1.5
  }


  # Dynamic bit -------------------------------------------------------------
  pof_year <- list()
  future_health_score_list <- list()
  year <- seq(from=0,to=año_final_simulacion,by=1)

   for (y in 1:length(year)){
    t <- year[y]

      future_health_Score_tf <- current_health_score_tf*exp((b2_tf/ageing_reduction_factor_tf) * t)
      future_health_Score_tc <- current_health_score_tc*exp((b2_tc/ageing_reduction_factor_tc) * t)
      H <- max(future_health_Score_tf, future_health_Score_tc)

    future_health_score_limit <- 15
    if (H > future_health_score_limit){
      H <- future_health_score_limit
    } else if (H < 4) {
      H <- 4
    }

    future_health_score_list[[paste(y)]] <- max(future_health_Score_tf, future_health_Score_tc)
    pof_year[[paste(y)]] <- k * (1 + (c * H) +
                                   (((c * H)^2) / factorial(2)) +
                                   (((c * H)^3) / factorial(3)))
  }

  pof_future <- data.frame(
    year=year,
    PoF=as.numeric(unlist(pof_year)),
    future_health_score = as.numeric(unlist(future_health_score_list)))
  pof_future$age <- NA
  pof_future$age[1] <- edad_TP

    for(i in 2:nrow(pof_future)) {

  pof_future$age[i] <- edad_TP + i -1

    }

  return(pof_future)
}
