#' @importFrom magrittr %>%
#' @title Probabilidad de falla actual para transformadores de 34,5/13,8 kV
#' @description Esta función calcula la probabilidad anual actual de falla para
#'  transformadores de 33/10 kV y 66/10 kV.  La función es una curva cúbica que
#'  se basa en los tres primeros términos de la serie de Taylor para una función
#'  exponencial. Para más información sobre la probabilidad de falla, ver la
#'  sección 6 en  la página 34 en CNAIM (2021).
#' @param tipo_transformador Texto. Texto que hace referencia a la categoría específica
#'  de activos. Véase la página 17, tabla 1 en CNAIM (2021).
#' Options:
#' \code{tipo_transformador =
#' c("33kV Transformer (GM)", "Transformador 34kV (GM)", "66kV Transformer (GM)")}. The default setting is
#' \code{tipo_transformador = "66kV Transformer (GM)"}
#' @param año_de_fabricación Númerico. La vida útil normal esperada depende
#' del año de fabricación, véase la página 107 del cuadro 20 de CNAIM (2021).
#' @inheritParams duty_factor_transformer_33_66kv
#' @inheritParams location_factor
#' @inheritParams current_health
#' @param edad_TP Numeric. The current age in years
#' of the transformer.
#' @param edad_CT Numeric. The current age in years
#' of the tapchanger
#' @param descarga_parcial_TP String. Indicating the
#' level of partial discharge in the transformer.
#' Options:
#' \code{descarga_parcial_TP = c("Baja", "Media", "Alta (No Confirmada)",
#'  "Alta (Confirmada)", "Default")}. See page 154, table 173 in CNAIM (2021).
#' @param descarga_parcial_CT String. Indicating the
#' level of partial discharge in the tapchanger
#' Options:
#' \code{descarga_parcial_CT = c("Baja", "Media", "Alta (No Confirmada)",
#'  "Alta (Confirmada)", "Default")}. See page 155, table 175 in CNAIM (2021).
#' @param Temperatura_lectura String. Indicating the criticality.
#' Options:
#' \code{Temperatura_lectura = c("Normal", "Moderadamente alta",
#' "Muy alta", "Default")}. See page 154, table 174 in CNAIM (2021).
#' @param tanque_principal String. Indicating the observed condition of the
#' main tank. Options:
#' \code{tanque_principal = c("Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. See page 131, table 83
#' in CNAIM (2021).
#' @param ventiladores_radiador Texto indicando el estado observado de los enfriadores/radiadores.
#'  Opciones:
#' \code{ventiladores_radiador = c("Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. See page 131, table 84
#' in CNAIM (2021).
#' @param pasatapas String. Indicating the observed condition of the
#' pasatapas. Options:
#' \code{pasatapas = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 131, table 85
#' in CNAIM (2021).
#' @param kiosk String. Indicating the observed condition of the
#' kiosk. Options:
#' \code{kiosk = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 132, table 86
#' in CNAIM (2021).
#' @param cable_boxes String. Indicating the observed condition of the
#' cable boxes. Options:
#' \code{cable_boxes = c("No Deterioration","Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 132, table 87
#' in CNAIM (2021).
#' @param external_tap String. Indicating the observed external condition of the
#'  tapchanger. Options:
#' \code{external_tap = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 133, table 88
#' in CNAIM (2021).
#' @param internal_tap String. Indicating the observed internal condition of the
#'  tapchanger. Options:
#' \code{internal_tap = c("Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 133, table 89
#' in CNAIM (2021).
#' @param mechnism_cond String. Indicating the observed condition of the
#'  drive mechnism. Options:
#' \code{mechnism_cond = c("No deterioration", "Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 133, table 90
#' in CNAIM (2021).
#' @param diverter_contacts String. Indicating the observed condition of the
#' selector and diverter contacts. Options:
#' \code{diverter_contacts = c("No deterioration", "Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 133, table 91
#' in CNAIM (2021).
#' @param diverter_braids String. Indicating the observed condition of the
#' selector and diverter braids. Options:
#' \code{diverter_braids = c("No deterioration", "Superficial/minor deterioration", "Some Deterioration",
#' "Substantial Deterioration", "Default")}. See page 134, table 92
#' in CNAIM (2021)
#' @param categoria_indice_corrosion Integer.
#' Especifique la categoria del indice de corrosión en el rango 1-5.
#' @param moisture Numeric. the amount of moisture given in (ppm) See page 162, table 203 in CNAIM (2021).
#' @param acidity Numeric. the amount of acidicy given in (mg KOH/g) See page 162, table 204 in CNAIM (2021).
#' @param bd_strength Numeric. the amount of breakdown strength given in (kV) See page 162, table 205 in CNAIM (2021).
#' @inheritParams oil_test_modifier
#' @inheritParams dga_test_modifier
#' @inheritParams ffa_test_modifier
#' @param gb_ref_given optional parameter to use custom reference values
#' @return DataFrame Current probability of failure
#' per annum per kilometer along with current health score.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Current probability of failure for a 66/10kV transformer
#' pof_transformer_33_66kv(tipo_transformador = "66kV Transformer (GM)",
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
#' kiosk = "Default",
#' cable_boxes = "Default",
#' external_tap = "Default",
#' internal_tap = "Default",
#' mechnism_cond = "Default",
#' diverter_contacts = "Default",
#' diverter_braids = "Default",
#' moisture = "Default",
#' acidity = "Default",
#' bd_strength = "Default",
#' hydrogen = "Default",
#' methane = "Default",
#' ethylene = "Default",
#' ethane = "Default",
#' acetylene = "Default",
#' hydrogen_pre = "Default",
#' methane_pre = "Default",
#' ethylene_pre = "Default",
#' ethane_pre = "Default",
#' acetylene_pre = "Default",
#' furfuraldehyde = "Default",
#' reliability_factor = "Default")

pof_transformador_34_5kv <- function(tipo_transformador = "66kV Transformer (GM)",
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
                                    kiosk = "Default",
                                    cable_boxes = "Default",
                                    external_tap = "Default",
                                    internal_tap = "Default",
                                    mechnism_cond = "Default",
                                    diverter_contacts = "Default",
                                    diverter_braids = "Default",
                                    moisture = "Default",
                                    acidity = "Default",
                                    bd_strength = "Default",
                                    hydrogen = "Default",
                                    methane = "Default",
                                    ethylene = "Default",
                                    ethane = "Default",
                                    acetylene = "Default",
                                    hydrogen_pre = "Default",
                                    methane_pre = "Default",
                                    ethylene_pre = "Default",
                                    ethane_pre = "Default",
                                    acetylene_pre = "Default",
                                    furfuraldehyde = "Default",
                                    reliability_factor = "Default",
                                    gb_ref_given = NULL) {
print(tipo_transformador)
  `Asset Register Category` = `Health Index Asset Category` =
    `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
    `K-Value (%)` = `C-Value` = `Asset Register Category` = `Sub-division` =
    `Asset Category` = NULL
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

  # cat("Termino generico 1:", generic_term_1)


  generic_term_2 <- gb_ref_taken$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...2`) %>% dplyr::pull()

  # cat("Termino generico 2:", generic_term_2)
 # Lectura de tabla 15-----------------------------
  # dato_cualquiera_1 <- gb_ref_taken$measured_cond_modifier_mmi_cal %>%
  #   dplyr::filter(`Asset Category` == "LV UGB") %>%
  #   dplyr::select(`Parameters for Combination Using MMI Technique - Factor Divider 1`) %>% dplyr::pull()
  # print(dato_cualquiera_1)


   # Lectura de tabla 1-----------------------------
# dato_cualquiera_2 <- gb_ref_taken$categorisation_of_assets %>%
#    dplyr::filter(`Asset Register Category` == "Transformador 34kV (GM)") %>%
#   dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()
# print(dato_cualquiera_2)


  # Normal expected life for transformador-----------------------------


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
  cat("factor de utilización TP:", duty_factor_tf)
  cat("factor de utilización Ctap:", duty_factor_tc)

  # factor de localización----------------------------------------------------
  location_factor_transformer <- location_factor(emplazamiento,
                                                 altura_m,
                                                 distancia_costa_km,
                                                 categoria_indice_corrosion,
                                                 asset_type = tipo_transformador)
  cat("factor de localización:", location_factor_transformer)

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


  # Descargas parciales en el transformador ----------------------------------------------
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
  cat("Descargas parciales en el transformador:", ci_factor_descarga_parcial_TP)

  # Descargas parciales en el cambiador de taps ------------------------------------------------
  mci_hv_tf_descarga_parcial_CT <-
    gb_ref_taken$mci_ehv_tf_tapchngr_prtl_dis

  ci_factor_descarga_parcial_CT <-
    mci_hv_tf_descarga_parcial_CT$`Condition Input Factor`[which(
      mci_hv_tf_descarga_parcial_CT$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial_CT)]

  ci_cap_descarga_parcial_CT <-
    mci_hv_tf_descarga_parcial_CT$`Condition Input Cap`[which(
      mci_hv_tf_descarga_parcial_CT$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial_CT)]

  ci_collar_descarga_parcial_CT <-
    mci_hv_tf_descarga_parcial_CT$`Condition Input Collar`[which(
      mci_hv_tf_descarga_parcial_CT$
        `Condition Criteria: Partial Discharge Test Result` ==
        descarga_parcial_CT)]

  cat("Descargas parciales en el cambiador de tap:", ci_factor_descarga_parcial_CT)

  # Lecturas de temperatura ----------------------------------------------------
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

  cat("Lecturas de temperatura:", ci_factor_temp_reading)

  # Factor de condicion medido -----------------------------------------------
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
  cat("Factor de condicion medido TP y CT:", measured_condition_factor_tf, measured_condition_factor_tc)

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


  # Transformer ------------OJO-----oci_ehv_tf_tanque_principal_cond--------------------------------------------

  # Condicion observada del tanque principal
  oci_ehv_tf_tanque_principal_cond <-
    gb_ref_taken$oci_ehv_tf_tanque_principal_cond

  Oi_collar_tanque_principal <-
    oci_ehv_tf_tanque_principal_cond$`Condition Input Collar`[which(
      oci_ehv_tf_tanque_principal_cond$`Condition Criteria: Observed Condition` ==
        tanque_principal)]

  Oi_cap_tanque_principal <-
    oci_ehv_tf_tanque_principal_cond$`Condition Input Cap`[which(
      oci_ehv_tf_tanque_principal_cond$`Condition Criteria: Observed Condition` ==
        tanque_principal)]

  Oi_factor_tanque_principal <-
    oci_ehv_tf_tanque_principal_cond$`Condition Input Factor`[which(
      oci_ehv_tf_tanque_principal_cond$`Condition Criteria: Observed Condition` ==
        tanque_principal)]

  cat("Factor de condicion del tanque principal:", Oi_factor_tanque_principal)


  # Condicion de los ventiladores y radiador

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

  cat("Factor de Condicion de los ventiladores y radiador:", Oi_factor_ventiladores_radiador)

  # Condicion observada del Pasatapas OJO ___ oci_ehv_tf__cond

  oci_ehv_tf__cond <-
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

  cat("Factor de Condicion de los pasatapas:", Oi_factor_pasatapas)

  # Kiosk

  oci_ehv_tf_kiosk_cond <-
    gb_ref_taken$oci_ehv_tf_kiosk_cond

  Oi_collar_kiosk <-
    oci_ehv_tf_kiosk_cond$`Condition Input Collar`[which(
      oci_ehv_tf_kiosk_cond$`Condition Criteria: Observed Condition` ==
        kiosk)]

  Oi_cap_kiosk <-
    oci_ehv_tf_kiosk_cond$`Condition Input Cap`[which(
      oci_ehv_tf_kiosk_cond$`Condition Criteria: Observed Condition` ==
        kiosk)]

  Oi_factor_kiosk <-
    oci_ehv_tf_kiosk_cond$`Condition Input Factor`[which(
      oci_ehv_tf_kiosk_cond$`Condition Criteria: Observed Condition` ==
        kiosk)]


  # Cable box
  oci_ehv_tf_cable_boxes_cond <-
    gb_ref_taken$oci_ehv_tf_cable_boxes_cond

  Oi_collar_cable_boxes <-
    oci_ehv_tf_cable_boxes_cond$`Condition Input Collar`[which(
      oci_ehv_tf_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        cable_boxes)]

  Oi_cap_cable_boxes <-
    oci_ehv_tf_cable_boxes_cond$`Condition Input Cap`[which(
      oci_ehv_tf_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        cable_boxes)]

  Oi_factor_cable_boxes <-
    oci_ehv_tf_cable_boxes_cond$`Condition Input Factor`[which(
      oci_ehv_tf_cable_boxes_cond$`Condition Criteria: Observed Condition` ==
        cable_boxes)]


  # Tapchanger --------------------------------------------------------------

  # External condition
  oci_ehv_tf_tapchanger_ext_cond <-
    gb_ref_taken$oci_ehv_tf_tapchanger_ext_cond

  Oi_collar_external_tap <-
    oci_ehv_tf_tapchanger_ext_cond$`Condition Input Collar`[which(
      oci_ehv_tf_tapchanger_ext_cond$`Condition Criteria: Observed Condition` ==
        external_tap)]

  Oi_cap_external_tap <-
    oci_ehv_tf_tapchanger_ext_cond$`Condition Input Cap`[which(
      oci_ehv_tf_tapchanger_ext_cond$`Condition Criteria: Observed Condition` ==
        external_tap)]

  Oi_factor_external_tap <-
    oci_ehv_tf_tapchanger_ext_cond$`Condition Input Factor`[which(
      oci_ehv_tf_tapchanger_ext_cond$`Condition Criteria: Observed Condition` ==
        external_tap)]


  # Internal condition
  oci_ehv_tf_int_cond <-
    gb_ref_taken$oci_ehv_tf_int_cond

  Oi_collar_internal_tap <-
    oci_ehv_tf_int_cond$`Condition Input Collar`[which(
      oci_ehv_tf_int_cond$`Condition Criteria: Observed Condition` ==
        internal_tap)]

  Oi_cap_internal_tap <-
    oci_ehv_tf_int_cond$`Condition Input Cap`[which(
      oci_ehv_tf_int_cond$`Condition Criteria: Observed Condition` ==
        internal_tap)]

  Oi_factor_internal_tap <-
    oci_ehv_tf_int_cond$`Condition Input Factor`[which(
      oci_ehv_tf_int_cond$`Condition Criteria: Observed Condition` ==
        internal_tap)]

  # Drive mechanism
  oci_ehv_tf_drive_mechnism_cond <-
    gb_ref_taken$oci_ehv_tf_drive_mechnism_cond

  Oi_collar_mechnism_cond <-
    oci_ehv_tf_drive_mechnism_cond$`Condition Input Collar`[which(
      oci_ehv_tf_drive_mechnism_cond$`Condition Criteria: Observed Condition` ==
        mechnism_cond)]

  Oi_cap_mechnism_cond <-
    oci_ehv_tf_drive_mechnism_cond$`Condition Input Cap`[which(
      oci_ehv_tf_drive_mechnism_cond$`Condition Criteria: Observed Condition` ==
        mechnism_cond)]

  Oi_factor_mechnism_cond <-
    oci_ehv_tf_drive_mechnism_cond$`Condition Input Factor`[which(
      oci_ehv_tf_drive_mechnism_cond$`Condition Criteria: Observed Condition` ==
        mechnism_cond)]

  # Selecter diverter contacts
  oci_ehv_tf_cond_select_divrter_cst <-
    gb_ref_taken$oci_ehv_tf_cond_select_div_cts

  Oi_collar_diverter_contacts <-
    oci_ehv_tf_cond_select_divrter_cst$`Condition Input Collar`[which(
      oci_ehv_tf_cond_select_divrter_cst$`Condition Criteria: Observed Condition` ==
        diverter_contacts)]

  Oi_cap_diverter_contacts <-
    oci_ehv_tf_cond_select_divrter_cst$`Condition Input Cap`[which(
      oci_ehv_tf_cond_select_divrter_cst$`Condition Criteria: Observed Condition` ==
        diverter_contacts)]

  Oi_factor_diverter_contacts <-
    oci_ehv_tf_cond_select_divrter_cst$`Condition Input Factor`[which(
      oci_ehv_tf_cond_select_divrter_cst$`Condition Criteria: Observed Condition` ==
        diverter_contacts)]


  # Selecter diverter braids
  oci_ehv_tf_cond_select_divrter_brd <-
    gb_ref_taken$oci_ehv_tf_cond_select_div_brd

  Oi_collar_diverter_braids <-
    oci_ehv_tf_cond_select_divrter_brd$`Condition Input Collar`[which(
      oci_ehv_tf_cond_select_divrter_brd$`Condition Criteria: Observed Condition` ==
        diverter_braids)]

  Oi_cap_diverter_braids <-
    oci_ehv_tf_cond_select_divrter_brd$`Condition Input Cap`[which(
      oci_ehv_tf_cond_select_divrter_brd$`Condition Criteria: Observed Condition` ==
        diverter_braids)]

  Oi_factor_diverter_braids <-
    oci_ehv_tf_cond_select_divrter_brd$`Condition Input Factor`[which(
      oci_ehv_tf_cond_select_divrter_brd$`Condition Criteria: Observed Condition` ==
        diverter_braids)]


  # Observed condition factor --------------------------------------

  # Transformer
  factors_tf_obs <- c(Oi_factor_tanque_principal,
                      Oi_factor_ventiladores_radiador,
                      Oi_factor_pasatapas,
                      Oi_factor_kiosk,
                      Oi_factor_cable_boxes)

  observed_condition_factor_tf <- mmi(factors_tf_obs,
                                      factor_divider_1_tf_obs,
                                      factor_divider_2_tf_obs,
                                      max_no_combined_factors_tf_obs)


  # Tapchanger

  factors_tc_obs <- c(Oi_factor_external_tap,
                      Oi_factor_internal_tap,
                      Oi_factor_mechnism_cond,
                      Oi_factor_diverter_contacts,
                      Oi_factor_diverter_braids)

  observed_condition_factor_tc <- mmi(factors_tc_obs,
                                      factor_divider_1_tc_obs,
                                      factor_divider_2_tc_obs,
                                      max_no_combined_factors_tc_obs)



  # Observed condition cap -----------------------------------------

  # Transformer
  caps_tf_obs <- c(Oi_cap_tanque_principal,
                   Oi_cap_ventiladores_radiador,
                   Oi_cap_pasatapas,
                   Oi_cap_kiosk,
                   Oi_cap_cable_boxes)

  observed_condition_cap_tf <- min(caps_tf_obs)

  # Tapchanger

  caps_tc_obs <- c(Oi_cap_external_tap,
                   Oi_cap_internal_tap,
                   Oi_cap_mechnism_cond,
                   Oi_cap_diverter_contacts,
                   Oi_cap_diverter_braids)

  observed_condition_cap_tc <- min(caps_tc_obs)


  # Observed condition collar ---------------------------------------

  # Transformer
  collars_tf_obs <- c(Oi_collar_tanque_principal,
                      Oi_collar_ventiladores_radiador,
                      Oi_collar_pasatapas,
                      Oi_collar_kiosk,
                      Oi_collar_cable_boxes)

  observed_condition_collar_tf <- max(collars_tf_obs)

  # Tapchanger

  collars_tc_obs <- c(Oi_collar_external_tap,
                      Oi_collar_internal_tap,
                      Oi_collar_mechnism_cond,
                      Oi_collar_diverter_contacts,
                      Oi_collar_diverter_braids)

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
  oil_test_mod <- oil_test_modifier(moisture,
                                    acidity,
                                    bd_strength)

  # DGA test modifier -------------------------------------------------------
  dga_test_mod <- dga_test_modifier(hydrogen,
                                    methane,
                                    ethylene,
                                    ethane,
                                    acetylene,
                                    hydrogen_pre,
                                    methane_pre,
                                    ethylene_pre,
                                    ethane_pre,
                                    acetylene_pre)
  # FFA test modifier -------------------------------------------------------
  ffa_test_mod <- ffa_test_modifier(furfuraldehyde)

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
                       reliability_factor = reliability_factor),
        current_health(initial_health_score_tf,
                       health_score_modifier_tc$health_score_factor_tc,
                       health_score_modifier_tc$health_score_cap_tc,
                       health_score_modifier_tc$health_score_collar_tc,
                       reliability_factor = reliability_factor))

  # Probability of failure for the 6.6/11 kV transformer today -----------------
  probability_of_failure <- k *
    (1 + (c * current_health_score) +
       (((c * current_health_score)^2) / factorial(2)) +
       (((c * current_health_score)^3) / factorial(3)))

  return(data.frame(pof = probability_of_failure, chs = current_health_score))
  }
