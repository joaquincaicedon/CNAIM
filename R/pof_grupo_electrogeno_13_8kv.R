#' @importFrom magrittr %>%
#' @title Probabilidad actual de falla para grupos electrógenos diésel de 13.8 kV.
#' @description Esta función calcula la probabilidad anual actual de falla 
#' para grupos electrógenos diésel de 13.8 kV. La función es una curva 
#' cúbica basada en los tres primeros términos de la serie de Taylor para
#' una función exponencial. Para más información sobre la función de
#' probabilidad de falla, consulte la sección 6 en la página 34 de 
#' CNAIM (2021).
#' @param tipo_generador Texto. Texto que hace referencia a la categoría 
#' específica de activos. Véase la página 17, tabla 1 en CNAIM (2021) y
#' modificación de CNAIM para incluir grupos electrógenos diésel.
#' Opciones:
#' \code{tipo_generador = "Grupo Electrógeno Diésel 13.8kV"}
#' @param año_fabricación Númerico. La vida útil normal esperada depende
#' del año de fabricación, véase la página 107, tabla 20 de CNAIM (2021) y
#' modificación de CNAIM para incluir grupos electrógenos diésel.
#' @inheritParams factor_trabajo_grupo_electrogeno_13_8kv
#' @inheritParams location_factor
#' @inheritParams current_health
#' @param edad_motor Numérico. Edad actual del motor en años.
#' @param edad_alternador Numérico. Edad actual del alternador en años.
#' 
#' CONDICIONES MEDIDAS ------------------------------------------------
#' @param rendimiento_motor Texto. Indica la condición medida del 
#' rendimiento del motor.
#' Opciones:
#' \code{rendimiento_motor = c("Óptimo", "Moderadamente degradado",
#' "Severamente degradado", "Default")}. Ver tabla 202A en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param velocidad_rpm_motor Texto. Indica la condición medida de la 
#' velocidad del motor en rpm.
#' Opciones:
#' \code{velocidad_rpm_motor = c("Estable", "Moderadamente desviada",
#' "Severamente desviada", "Default")}. Ver tabla 202B en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param consumo_motor Texto. Indica la condición medida del 
#' consumo de combustible del motor.
#' Opciones:
#' \code{consumo_motor = c("Óptimo", "Moderadamente aumentado",
#' "Excesivamente aumentado", "Default")}. Ver tabla 202C en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param freno_motor Texto. Indica la condición medida de la 
#' operación del freno del motor.
#' Opciones:
#' \code{freno_motor = c("Óptima", "Moderadamente retardada",
#' "Severamente averiada", "Default")}. Ver tabla 202D en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param resistencia_aislamiento_alternador Texto. Indica la condición medida de 
#' la resistencia de aislamiento del alternador.
#' Opciones:
#' \code{resistencia_aislamiento_alternador = c("Baja", "Media", "Alta (no confirmada)",
#' "Alta (confirmada)", "Default")}. Ver tabla 202E en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param descargas_parciales_alternador Texto. Indica la condición medida de 
#' las descargas parciales en el alternador.
#' Opciones:
#' \code{descargas_parciales_alternador = c("Baja", "Media", "Alta (no confirmada)",
#' "Alta (confirmada)", "Default")}. Ver tabla 202F en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param secuencia_alternador Texto. Indica la condición medida de la 
#' secuencia de fases del alternador.
#' Opciones:
#' \code{secuencia_alternador = c("Baja", "Media", "Alta (no confirmada)",
#' "Alta (confirmada)", "Default")}. Ver tabla 202G en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param vibraciones_alternador Texto. Indica la condición medida de la
#' presencia de vibraciones en el alternador.
#' Opciones:
#' \code{vibraciones_alternador = c("Baja", "Media", "Alta (no confirmada)",
#' "Alta (confirmada)", "Default")}. Ver tabla 202H en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param pérdidas_alternador Texto. Indica la condición medida del 
#' rendimiento del alternador.
#' Opciones:
#' \code{pérdidas_alternador = c("Baja", "Media", "Alta (no confirmada)",
#' "Alta (confirmada)", "Default")}. Ver tabla 202I en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param temperatura_arrollamiento_alternador Texto. Indica la condición medida del 
#' rendimiento del alternador.
#' Opciones:
#' \code{temperatura_arrollamiento_alternador = c("Normal", "Alta",
#' "Muy alta", "Default")}. Ver tabla 202J en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' 
#' CONDICIONES OBSERVADAS ----------------------------------------------
#' @param cubierta_motor Texto. Indica la condición observada de la 
#' cubierta de acero del motor.
#' Opciones:
#' \code{cubierta_motor = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143A en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param rodamientos_motor Texto. Indica la condición observada de los 
#' rodamientos del motor.
#' Opciones:
#' \code{rodamientos_motor = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143B en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param combustible_motor Texto. Indica la condición observada del 
#' sistema de combustible del motor.
#' Opciones:
#' \code{combustible_motor = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143C en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param enfriamiento_motor Texto. Indica la condición observada del 
#' sistema de enfriamiento del motor.
#' Opciones:
#' \code{enfriamiento_motor = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143D en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param lubricación_motor Texto. Indica la condición observada del 
#' sistema de lubricación del motor.
#' Opciones:
#' \code{lubricación_motor = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143E en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param aire_motor Texto. Indica la condición observada del 
#' sistema de inducción de aire del motor.
#' Opciones:
#' \code{aire_motor = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143F en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param turbocompresores_motor Texto. Indica la condición observada de los 
#' turbocompresores del motor.
#' Opciones:
#' \code{turbocompresores_motor = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143G en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param culatas_motor Texto. Indica la condición observada de 
#' las culatas de cilíndros del motor.
#' Opciones:
#' \code{culatas_motor = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143H en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param filtro_motor Texto. Indica la condición observada del 
#' filtro del motor.
#' Opciones:
#' \code{filtro_motor = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143I en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param cubierta_alternador Texto. Indica la condición observada de la 
#' cubierta del alternador.
#' Opciones:
#' \code{cubierta_generador = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143J en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param aislamiento_alternador Texto. Indica la condición observada del 
#' aislamiento del alternador.
#' Opciones:
#' \code{aislamiento_generador = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143K en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param cables_alternador Texto. Indica la condición observada de los 
#' cables y de la caja de terminales del alternador.
#' Opciones:
#' \code{cables_generador = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143L en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param rotor_alternador Texto. Indica la condición observada del 
#' rotor del alternador.
#' Opciones:
#' \code{rotor_generador = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143M en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param estator_alternador Texto. Indica la condición observada del 
#' estator del alternador.
#' Opciones:
#' \code{estator_generador = c("Deterioro específico/menor", "Cierto deterioro",
#' "Deterioro sustancial", "Default")}. Ver tabla 143N en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' 
#' @param gb_ref_given Parámetro opcional para utilizar valores de 
#' referencia personalizados
#' @return DataFrame. Probabilidad actual de falla por año y kilómetro, 
#' junto con la puntuación actual de salud.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' Y adaptación de CNAIM para considerar grupos electrógenos diésel.
#' @export
#' @examples
#' # Probabilidad actual de falla de un grupo electrógeno diésel de 13.8 kV.
#' pof_grupo_electrogeno_13_8kv(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
#'                              año_fabricación = 2010,
#'                              utilización_pct = "Default",
#'                              emplazamiento = "Default",
#'                              altitud_m = "Default",
#'                              distancia_costa_km = "Default",
#'                              indice_corrosion = "Default",
#'                              edad_motor = 15, 
#'                              edad_alternador = 15,
#'                              rendimiento_motor = "Default",
#'                              velocidad_rpm_motor = "Default",
#'                              consumo_motor = "Default",
#'                              freno_motor = "Default",
#'                              resistencia_aislamiento_alternador = "Default",
#'                              descargas_parciales_alternador = "Default",
#'                              secuencia_alternador = "Default",
#'                              vibraciones_alternador = "Default",
#'                              pérdidas_alternador = "Default",
#'                              temperatura_arrollamiento_alternador = "Default",
#'                              cubierta_motor = "Default",
#'                              rodamientos_motor = "Default",
#'                              combustible_motor = "Default",
#'                              enfriamiento_motor = "Default",
#'                              lubricación_motor = "Default",
#'                              aire_motor = "Default",
#'                              turbocompresores_motor = "Default",
#'                              culatas_motor = "Default",
#'                              filtro_motor = "Default",
#'                              cubierta_alternador = "Default", 
#'                              aislamiento_alternador = "Default", 
#'                              cables_alternador = "Default", 
#'                              rotor_alternador = "Default", 
#'                              estator_alternador = "Default")

pof_grupo_electrogeno_13_8kv <- function(tipo_generador = "Grupo Electrógeno Diésel 13.8kV",
                                         año_fabricación,
                                         utilización_pct = "Default",
                                         emplazamiento = "Default",
                                         altitud_m = "Default",
                                         distancia_costa_km = "Default",
                                         indice_corrosion = "Default",
                                         edad_motor, 
                                         edad_alternador,
                                         rendimiento_motor = "Default",
                                         velocidad_rpm_motor = "Default",
                                         consumo_motor = "Default",
                                         freno_motor = "Default",
                                         resistencia_aislamiento_alternador = "Default",
                                         descargas_parciales_alternador = "Default",
                                         secuencia_alternador = "Default",
                                         vibraciones_alternador = "Default",
                                         pérdidas_alternador = "Default",
                                         temperatura_arrollamiento_alternador = "Default",
                                         cubierta_motor = "Default",
                                         rodamientos_motor = "Default",
                                         combustible_motor = "Default",
                                         enfriamiento_motor = "Default",
                                         lubricación_motor = "Default",
                                         aire_motor = "Default",
                                         turbocompresores_motor = "Default",
                                         culatas_motor = "Default",
                                         filtro_motor = "Default",
                                         cubierta_alternador = "Default", 
                                         aislamiento_alternador = "Default", 
                                         cables_alternador = "Default", 
                                         rotor_alternador = "Default", 
                                         estator_alternador = "Default",
                                         gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` =
    `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
    `K-Value (%)` = `C-Value` = `Asset Register  Category` = `Sub-division` =
    `Asset Category` = NULL
  # due to NSE notes in R CMD check
  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  # Ref. tabla de categorización de activos y términos genéricos para activos --

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tipo_generador) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  generic_term_1 <- gb_ref_taken$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...1`) %>% dplyr::pull()

  generic_term_2 <- gb_ref_taken$generic_terms_for_assets %>%
    dplyr::filter(`Health Index Asset Category` == asset_category) %>%
    dplyr::select(`Generic Term...2`) %>% dplyr::pull()
  
  # Vida útil normal esperada para el motor del grupo electrógeno diésel ------------------

  vida_normal_esperada_motor <- gb_ref_taken$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == tipo_generador & `Sub-division` ==
                    "Motor") %>%
    dplyr::pull()

  # Vida útil normal esperada para el alternador del grupo electrógeno diésel -----------------------------

  vida_normal_esperada_alternador <- gb_ref_taken$normal_expected_life %>%
    dplyr::filter(`Asset Register  Category` == tipo_generador & `Sub-division` ==
                    "Alternador") %>%
    dplyr::pull()

  # Constantes C y K para la función de probabilidad de falla --------------------------------------
  k <- gb_ref_taken$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    'HV Generador Diésel') %>% dplyr::select(`K-Value (%)`) %>%
    dplyr::pull()/100

  c <- gb_ref_taken$pof_curve_parameters %>%
    dplyr::filter(`Functional Failure Category` ==
                    'HV Generador Diésel') %>% dplyr::select(`C-Value`) %>% dplyr::pull()

  # Factor de trabajo -------------------------------------------------------------
  factor_trabajo_GE <- factor_trabajo_grupo_electrogeno_13_8kv(utilización_pct)

  # Factor de ubicación ----------------------------------------------------
  factor_ubicación_GE <- location_factor(emplazamiento,
                                         altitud_m,
                                         distancia_costa_km,
                                         indice_corrosion,
                                         asset_type = tipo_generador)

  # Vida útil esperada para el motor del grupo electrógeno diésel ------------------------------
  vida_esperada_años_motor <- expected_life(normal_expected_life = vida_normal_esperada_motor,
                                            factor_trabajo_GE,
                                            factor_ubicación_GE)

  # Vida útil esperada para el alternador del grupo electrógeno diésel ------------------------------
  vida_esperada_años_alternador <- expected_life(normal_expected_life = vida_normal_esperada_alternador,
                                                 factor_trabajo_GE,
                                                 factor_ubicación_GE)

  # b1 (Tasa de envejecimiento inicial) ------------------------------------------------
  b1_motor <- beta_1(vida_esperada_años_motor)
  b1_alternador <- beta_1(vida_esperada_años_alternador)

  # Puntaje de salud inicial ----------------------------------------------------
  puntaje_salud_inicial_motor <- initial_health(b1_motor, edad_motor)
  puntaje_salud_inicial_alternador <- initial_health(b1_alternador, edad_alternador)

  ## NOTA
  # Típicamente, el límite inferior del puntaje de salud es 0.5 y
  # el límite superior del puntaje de salud es 10, lo que implica que no hay
  # anulación del puntaje de salud. Sin embargo, en algunos casos,
  # estos parámetros se establecen en otros valores en las tablas de calibración
  # del modificador del puntaje de salud.
  # Estos valores de anulación se muestran en la Tabla 35 a la Tabla 202
  # y en la Tabla 207 en el Apéndice B.

  # Entradas de condición medida ---------------------------------------------
  mcm_mmi_cal_df <-
    gb_ref_taken$measured_cond_modifier_mmi_cal

  mcm_mmi_cal_df <-
    mcm_mmi_cal_df[which(mcm_mmi_cal_df$`Asset Category` == "HV Generador Diésel"), ]

  factor_divisor_1_motor <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(mcm_mmi_cal_df$Subcomponent == "Motor")
    ])

  factor_divisor_1_alternador <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(mcm_mmi_cal_df$Subcomponent == "Alternador")
    ])

  factor_divisor_2_motor <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(mcm_mmi_cal_df$Subcomponent == "Motor")
    ])

  factor_divisor_2_alternador <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(mcm_mmi_cal_df$Subcomponent == "Alternador")
    ])

  max_no_factores_comb_motor <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(mcm_mmi_cal_df$Subcomponent == "Motor")
    ])

  max_no_factores_comb_alternador <-
    as.numeric(mcm_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(mcm_mmi_cal_df$Subcomponent == "Alternador")
    ])

  # CONDICIONES MEDIDAS DEL MOTOR -------------------------------------------
  # 1. Rendimiento del motor ------------------------------------------------
  mci_hv_gen_die_rendimien_motor <-
    gb_ref_taken$mci_hv_gen_die_rendimien_motor

  ci_factor_rendimien_motor <-
     mci_hv_gen_die_rendimien_motor$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_rendimien_motor$
        `Criterios de Condición: Rendimiento` ==
        rendimiento_motor)]

  ci_cap_rendimien_motor <-
     mci_hv_gen_die_rendimien_motor$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_rendimien_motor$
        `Criterios de Condición: Rendimiento` ==
        rendimiento_motor)]

  ci_collar_rendimien_motor <-
     mci_hv_gen_die_rendimien_motor$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_rendimien_motor$
        `Criterios de Condición: Rendimiento` ==
        rendimiento_motor)]

  # 2. Velocidad del motor en rpm -------------------------------------------
  mci_hv_gen_die_velocidad_motor <-
    gb_ref_taken$mci_hv_gen_die_velocidad_motor

  ci_factor_rendimien_motor <-
     mci_hv_gen_die_velocidad_motor$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_velocidad_motor$
        `Criterios de Condición: Velocidad` ==
        velocidad_rpm_motor)]

  ci_cap_rendimien_motor <-
     mci_hv_gen_die_velocidad_motor$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_velocidad_motor$
        `Criterios de Condición: Velocidad` ==
        velocidad_rpm_motor)]

  ci_collar_rendimien_motor <-
     mci_hv_gen_die_velocidad_motor$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_velocidad_motor$
        `Criterios de Condición: Velocidad` ==
        velocidad_rpm_motor)]
browser()
  # 3. Consumo de combustible del motor -------------------------------------
  # 4. Operación del freno del motor ----------------------------------------

  # CONDICIONES MEDIDAS DEL ALTERNADOR --------------------------------------
  # 1. Resistencia de aislamiento del alternador ----------------------------
  # 2. Descargas parciales en el alternador ---------------------------------
  # 3. Secuencia de fases del alternador ------------------------------------
  # 4. Vibraciones en el alternador -----------------------------------------
  # 5. Pérdidas en el alternador --------------------------------------------
  # 6. Temperatura de arrollamiento del alternador --------------------------

  # Factor de condición medida ----------------------------------------------

  # CONDICIONES OBSERVADAS DEL MOTOR ----------------------------------------
  # 1. Cubierta del motor ---------------------------------------------------
  # 2. Rodamientos del motor ------------------------------------------------
  # 3. Sistema de combustible del motor -------------------------------------
  # 4. Sistema de enfriamiento del motor ------------------------------------
  # 5. Sistema de lubricación del motor -------------------------------------
  # 6. Sistema de inducción de aire del motor -------------------------------
  # 7. Turbocompresores del motor -------------------------------------------
  # 8. Culatas de cilíndros del motor ---------------------------------------
  # 9. Filtro del motor -----------------------------------------------------

  # CONDICIONES OBSERVADAS DEL ALTERNADOR -----------------------------------
  # 1. Cubierta del alternador ----------------------------------------------
  # 2. Aislamiento del alternador -------------------------------------------
  # 3. Cables y caja de terminales del alternador ---------------------------
  # 4. Rotor del alternador -------------------------------------------------
  # 5. Estator del alternador -----------------------------------------------

  # Factor de condición observada -------------------------------------------

  mci_hv_tf_partial_discharge <-
    gb_ref_taken$mci_ehv_tf_main_tf_prtl_dis

  ci_factor_partial_discharge_tf <-
    mci_hv_tf_partial_discharge$`Condition Input Factor`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tf)]

  ci_cap_partial_discharge_tf <-
    mci_hv_tf_partial_discharge$`Condition Input Cap`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tf)]

  ci_collar_partial_discharge_tf <-
    mci_hv_tf_partial_discharge$`Condition Input Collar`[which(
      mci_hv_tf_partial_discharge$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tf)]


  # Partial discharge tapchanger ------------------------------------------------
  mci_hv_tf_partial_discharge_tc <-
    gb_ref_taken$mci_ehv_tf_tapchngr_prtl_dis

  ci_factor_partial_discharge_tc <-
    mci_hv_tf_partial_discharge_tc$`Condition Input Factor`[which(
      mci_hv_tf_partial_discharge_tc$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tc)]

  ci_cap_partial_discharge_tc <-
    mci_hv_tf_partial_discharge_tc$`Condition Input Cap`[which(
      mci_hv_tf_partial_discharge_tc$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tc)]

  ci_collar_partial_discharge_tc <-
    mci_hv_tf_partial_discharge_tc$`Condition Input Collar`[which(
      mci_hv_tf_partial_discharge_tc$
        `Condition Criteria: Partial Discharge Test Result` ==
        partial_discharge_tc)]


  # Temperature readings ----------------------------------------------------
  mci_hv_tf_temp_readings <-
    gb_ref_taken$mci_ehv_tf_temp_readings

  ci_factor_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Factor`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        temperature_reading)]

  ci_cap_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Cap`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        temperature_reading)]

  ci_collar_temp_reading <-
    mci_hv_tf_temp_readings$`Condition Input Collar`[which(
      mci_hv_tf_temp_readings$
        `Condition Criteria: Temperature Reading` ==
        temperature_reading)]

  # measured condition factor -----------------------------------------------
  factors_tf <- c(ci_factor_partial_discharge_tf,
                  ci_factor_temp_reading)

  measured_condition_factor_tf <- mmi(factors_tf,
                                      factor_divider_1_tf,
                                      factor_divider_2_tf,
                                      max_no_combined_factors_tf)


  measured_condition_factor_tc <- mmi(ci_factor_partial_discharge_tc,
                                      factor_divider_1_tc,
                                      factor_divider_2_tc,
                                      max_no_combined_factors_tc)

  # Measured condition cap --------------------------------------------------
  caps_tf <- c(ci_cap_partial_discharge_tf,
               ci_cap_temp_reading)
  measured_condition_cap_tf <- min(caps_tf)


  measured_condition_cap_tc <- ci_cap_partial_discharge_tc

  # Measured condition collar -----------------------------------------------
  collars_tf <- c(ci_collar_partial_discharge_tf,
                  ci_collar_temp_reading)
  measured_condition_collar_tf <- max(collars_tf)

  measured_condition_collar_tc <- ci_collar_partial_discharge_tc

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

  Oi_collar_main_tank <-
    oci_ehv_tf_main_tank_cond$`Condition Input Collar`[which(
      oci_ehv_tf_main_tank_cond$`Condition Criteria: Observed Condition` ==
        main_tank)]

  Oi_cap_main_tank <-
    oci_ehv_tf_main_tank_cond$`Condition Input Cap`[which(
      oci_ehv_tf_main_tank_cond$`Condition Criteria: Observed Condition` ==
        main_tank)]

  Oi_factor_main_tank <-
    oci_ehv_tf_main_tank_cond$`Condition Input Factor`[which(
      oci_ehv_tf_main_tank_cond$`Condition Criteria: Observed Condition` ==
        main_tank)]

  # Coolers/Radiator condition

  oci_ehv_tf_cooler_radiatr_cond <-
    gb_ref_taken$oci_ehv_tf_cooler_radiatr_cond

  Oi_collar_coolers_radiator <-
    oci_ehv_tf_cooler_radiatr_cond$`Condition Input Collar`[which(
      oci_ehv_tf_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        coolers_radiator)]

  Oi_cap_coolers_radiator <-
    oci_ehv_tf_cooler_radiatr_cond$`Condition Input Cap`[which(
      oci_ehv_tf_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        coolers_radiator)]

  Oi_factor_coolers_radiator <-
    oci_ehv_tf_cooler_radiatr_cond$`Condition Input Factor`[which(
      oci_ehv_tf_cooler_radiatr_cond$`Condition Criteria: Observed Condition` ==
        coolers_radiator)]


  # Bushings

  oci_ehv_tf_bushings_cond <-
    gb_ref_taken$oci_ehv_tf_bushings_cond

  Oi_collar_bushings <-
    oci_ehv_tf_bushings_cond$`Condition Input Collar`[which(
      oci_ehv_tf_bushings_cond$`Condition Criteria: Observed Condition` ==
        bushings)]

  Oi_cap_bushings <-
    oci_ehv_tf_bushings_cond$`Condition Input Cap`[which(
      oci_ehv_tf_bushings_cond$`Condition Criteria: Observed Condition` ==
        bushings)]

  Oi_factor_bushings <-
    oci_ehv_tf_bushings_cond$`Condition Input Factor`[which(
      oci_ehv_tf_bushings_cond$`Condition Criteria: Observed Condition` ==
        bushings)]

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
  factors_tf_obs <- c(Oi_factor_main_tank,
                      Oi_factor_coolers_radiator,
                      Oi_factor_bushings,
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
  caps_tf_obs <- c(Oi_cap_main_tank,
                   Oi_cap_coolers_radiator,
                   Oi_cap_bushings,
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
  collars_tf_obs <- c(Oi_collar_main_tank,
                      Oi_collar_coolers_radiator,
                      Oi_collar_bushings,
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
