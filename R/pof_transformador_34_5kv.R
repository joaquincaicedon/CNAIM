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
#' @param edad_TP Numerico. Edad actual del transformador en años.
#' @param edad_CT Numerico. Edad actual del cambiador de taps en años.
#' @param descarga_parcial_TP Texto. Indica el nivel de descarga parciales.
#' Options:
#' \code{descarga_parcial_TP = c("Baja", "Media", "Alta (No Confirmada)",
#'  "Alta (Confirmada)", "Default")}. Ver página 154, tabla 173 en CNAIM (2021).
#' @param descarga_parcial_CT String. Indicating the
#' level of partial discharge in the tapchanger
#' Options:
#' \code{descarga_parcial_CT = c("Baja", "Media", "Alta (No Confirmada)",
#'  "Alta (Confirmada)", "Default")}. Ver página 155, tabla 175 en CNAIM (2021).
#' @param Temperatura_lectura String. Indicating the criticality.
#' Options:
#' \code{Temperatura_lectura = c("Normal", "Moderadamente alta",
#' "Muy alta", "Default")}. Ver página 154, tabla 174 en CNAIM (2021).
#' @param tanque_principal String. Indicating the observed condition of the
#' main tank. Options:
#' \code{tanque_principal = c("Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver página 131, tabla 83
#' en CNAIM (2021).
#' @param ventiladores_radiador Texto indicando el estado observado de los enfriadores/radiadores.
#'  Opciones:
#' \code{ventiladores_radiador = c("Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver página 131, tabla 84
#' en CNAIM (2021).
#' @param pasatapas Texto. Indicando la condición observada del
#' pasatapas. Options:
#' \code{pasatapas = c("Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver página 131, tabla 85
#' en CNAIM (2021).
#' @param quiosco String. Indicando la condición observada del quiosco. Options:
#' \code{quiosco = c("Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver página 132, tabla 86
#' en CNAIM (2021).
#' @param Caja_cables String. Indicando la condicion observada de la caja de cables. Opciones:
#' \code{Caja_cables = c("Sin deterioro","Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver página 132, tabla 87
#' en CNAIM (2021).
#' @param externo_tap Texto indicando la condición externa observada del
#' cambiador de tomas. Opciones:
#' \code{externo_tap = c("Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver la tabla 88 de la página 133
#' en CNAIM (2021).
#' @param interno_tap Texto indicando la condición interna observada del
#'  de tomas. Opciones:
#' \code{interno_tap = c("Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver la tabla 89 de la página 133
#' en CNAIM (2021).
#' @param Condicion_mecanismo Texto que indica la condición observada del mecanismo de
#' accionamiento del OLTC. Opciones:
#' \code{Condicion_mecanismo = c("Sin deterioro", "Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver página 133, tabla 90
#' en CNAIM (2021).
#' @param Contactos_derivador Texto con la indicación del estado observado de los
#' contactos del selector y del derivador de taps. Opciones:
#' \code{Contactos_derivador = c("Sin deterioro", "Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver página 133, tabla 91
#' en CNAIM (2021).
#' @param Trenzas_derivador Texto que indica el estado observado de las trenzas
#'  de derivador. Opciones:
#' \code{Trenzas_derivador = c("Sin deterioro", "Deterioro superficial/leve", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver página 134, tabla 92
#' en CNAIM (2021)
#' @param categoria_indice_corrosion Integer.
#' Especifique la categoria del indice de corrosión en el rango 1-5.
#' @param humedad Numérico. La cantidad de humedad dada en (ppm).  Ver página 162, tabla 203 en CNAIM (2021).
#' @param acidez Numerico. La cantidad de acidez dada en (mg KOH/g) Ver página 162, tabla 204 en CNAIM (2021).
#' @param Rigidez_dielectrica Numerico. valor de rigidez dielectrica dada en (kV) Ver página 162, tabla 205 en CNAIM (2021).
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
#' hidrógeno = "Default",
#' metano = "Default",
#' etileno = "Default",
#' etano = "Default",
#' acetileno = "Default",
#' hidrógeno_pre = "Default",
#' metano_pre = "Default",
#' etileno_pre = "Default",
#' etano_pre = "Default",
#' acetileno_pre = "Default",
#' furfuraldehído = "Default",
#' Factor_confiabilidad = "Default")

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

   # Ref. tabla Categorisation of Assets and Generic Terms for Assets  --

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
  # Health Score Modifier calibration tablas.
  # These overriding values are shown in tabla 35 to tabla 202
  # and tabla 207 in Appendix B.

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


  # Transformer ------------------------------------------------------------

  # Condicion observada del tanque principal
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

  # Condicion observada del Pasatapas

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

  cat("Factor de Condicion de los pasatapas:", Oi_factor_pasatapas)

  # condición del quiosco

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
  cat("Factor de Condicion del quiosco:", Oi_factor_quiosco)


  # Condicion de la caja de cables
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

  cat("Factor de Condicion de la caja de cables:", Oi_factor_Caja_cables)


  # Tapchanger --------------------------------------------------------------

  # Condicion externa del cambiador de taps
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
  cat("Factor de condicion externa del cambiador de taps:", Oi_factor_externo_tap)

  # Factor de condicion interna del cambiador de taps:
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
  cat("Factor de condicion interna del cambiador de taps:", Oi_factor_interno_tap)

  # Factor de condicion del mecanismo de control del cambiador de taps:
  oci_ehv_tf_ddrive_mechnism_cond <-
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

  cat("Factor de condicion del mecanismo de control del cambiador de taps:", Oi_factor_Condicion_mecanismo)

  # Condicion de los contactos de derivador de taps
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

  cat("Factor de Condicion de los contactos de derivador de taps:", Oi_factor_Contactos_derivador)

  # Condicion de las trenzas del derivador
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
  cat("Factor de Condicion de las trenzas del derivador:", Oi_factor_Trenzas_derivador)

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
  cat("Modificador relacionado con las pruebas en el aceite :",  oil_test_mod)


  # DGA test modifier -------------------------------------------------------
  dga_test_mod <- dga_test_modifier(hidrógeno,
                                    metano,
                                    etileno,
                                    etano,
                                    acetileno,
                                    hidrógeno_pre,
                                    metano_pre,
                                    etileno_pre,
                                    etano_pre,
                                    acetileno_pre)

  cat("Modificador relacionado con DGA:",  dga_test_mod)

  # Modificador por analisis de furanos -------------------------------------------------------
  ffa_test_mod <- ffa_test_modifier(furfuraldehído)

  cat("Modificador por analisis de furanos:",  ffa_test_mod)
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

  return(data.frame(pof = probability_of_failure, chs = current_health_score))
  }
