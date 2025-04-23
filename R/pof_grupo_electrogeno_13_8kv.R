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
#' \code{resistencia_aislamiento_alternador = c("Excelente", "Buena", "Aceptable",
#' "Deficiente", "Crítica", "Default")}. Ver tabla 202E en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param descargas_parciales_alternador Texto. Indica la condición medida de 
#' las descargas parciales en el alternador.
#' Opciones:
#' \code{descargas_parciales_alternador = c("Muy bajas", "Bajas", "Medias",
#' "Altas", "Muy altas", "Default")}. Ver tabla 202F en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param secuencia_alternador Texto. Indica la condición medida de la 
#' secuencia de fases del alternador.
#' Opciones:
#' \code{secuencia_alternador = c("Correcta", "Inestable", "Invertida",
#' "Crítica", "Default")}. Ver tabla 202G en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param vibraciones_alternador Texto. Indica la condición medida de la
#' presencia de vibraciones en el alternador.
#' Opciones:
#' \code{vibraciones_alternador = c("Muy bajas", "Bajas", "Medias",
#' "Altas", "Muy altas", "Default")}. Ver tabla 202H en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param pérdidas_alternador Texto. Indica la condición medida del 
#' rendimiento del alternador.
#' Opciones:
#' \code{pérdidas_alternador = c("Muy bajas", "Bajas", "Medias",
#' "Altas", "Muy altas", "Default")}. Ver tabla 202I en CNAIM (2021)
#' modificada para incluir grupos electrógenos diésel.
#' @param temperatura_arrollamiento_alternador Texto. Indica la condición medida del 
#' rendimiento del alternador.
#' Opciones:
#' \code{temperatura_arrollamiento_alternador = c("Muy baja", "Baja", "Media",
#' "Alta", "Muy alta", "Default")}. Ver tabla 202J en CNAIM (2021)
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
#'                              estator_alternador = "Default",
#'                              factor_confiabilidad = "Default")

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
                                         factor_confiabilidad = "Default",
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
  mci_hv_gen_die_rendimiento_motor <-
    gb_ref_taken$mci_hv_gen_die_rendimien_motor

  ci_factor_rendimiento_motor <-
     mci_hv_gen_die_rendimiento_motor$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_rendimiento_motor$
        `Criterios de Condición: Rendimiento` ==
        rendimiento_motor)]

  ci_cap_rendimiento_motor <-
     mci_hv_gen_die_rendimiento_motor$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_rendimiento_motor$
        `Criterios de Condición: Rendimiento` ==
        rendimiento_motor)]

  ci_collar_rendimiento_motor <-
     mci_hv_gen_die_rendimiento_motor$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_rendimiento_motor$
        `Criterios de Condición: Rendimiento` ==
        rendimiento_motor)]

  # 2. Velocidad del motor en rpm -------------------------------------------
  mci_hv_gen_die_velocidad_motor <-
    gb_ref_taken$mci_hv_gen_die_velocidad_motor

  ci_factor_velocidad_motor <-
     mci_hv_gen_die_velocidad_motor$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_velocidad_motor$
        `Criterios de Condición: Velocidad` ==
        velocidad_rpm_motor)]

  ci_cap_velocidad_motor <-
     mci_hv_gen_die_velocidad_motor$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_velocidad_motor$
        `Criterios de Condición: Velocidad` ==
        velocidad_rpm_motor)]

  ci_collar_velocidad_motor <-
     mci_hv_gen_die_velocidad_motor$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_velocidad_motor$
        `Criterios de Condición: Velocidad` ==
        velocidad_rpm_motor)]

  # 3. Consumo de combustible del motor -------------------------------------
  mci_hv_gen_die_consumo_motor <-
    gb_ref_taken$mci_hv_gen_die_consumo_motor

  ci_factor_consumo_motor <-
     mci_hv_gen_die_consumo_motor$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_consumo_motor$
        `Criterios de Condición: Consumo de Combustible` ==
        consumo_motor)]

  ci_cap_consumo_motor <-
     mci_hv_gen_die_consumo_motor$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_consumo_motor$
        `Criterios de Condición: Consumo de Combustible` ==
        consumo_motor)]

  ci_collar_consumo_motor <-
     mci_hv_gen_die_consumo_motor$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_consumo_motor$
        `Criterios de Condición: Consumo de Combustible` ==
        consumo_motor)]

  # 4. Operación del freno del motor ----------------------------------------
  mci_hv_gen_die_freno_motor <-
    gb_ref_taken$mci_hv_gen_die_freno_motor

  ci_factor_freno_motor <-
     mci_hv_gen_die_freno_motor$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_freno_motor$
        `Criterios de Condición: Operación del Freno` ==
        freno_motor)]

  ci_cap_freno_motor <-
     mci_hv_gen_die_freno_motor$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_freno_motor$
        `Criterios de Condición: Operación del Freno` ==
        freno_motor)]

  ci_collar_freno_motor <-
     mci_hv_gen_die_freno_motor$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_freno_motor$
        `Criterios de Condición: Operación del Freno` ==
        freno_motor)]

  # CONDICIONES MEDIDAS DEL ALTERNADOR --------------------------------------
  # 1. Resistencia de aislamiento del alternador ----------------------------
  mci_hv_gen_die_resistencia_aislamiento_alternador <-
    gb_ref_taken$mci_hv_gen_die_aislamie_altern

  ci_factor_resistencia_aislamiento_alternador <-
     mci_hv_gen_die_resistencia_aislamiento_alternador$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_resistencia_aislamiento_alternador$
        `Criterios de Condición: Resistencia de Aislamiento` ==
        resistencia_aislamiento_alternador)]

  ci_cap_resistencia_aislamiento_alternador <-
     mci_hv_gen_die_resistencia_aislamiento_alternador$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_resistencia_aislamiento_alternador$
        `Criterios de Condición: Resistencia de Aislamiento` ==
        resistencia_aislamiento_alternador)]

  ci_collar_resistencia_aislamiento_alternador <-
     mci_hv_gen_die_resistencia_aislamiento_alternador$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_resistencia_aislamiento_alternador$
        `Criterios de Condición: Resistencia de Aislamiento` ==
        resistencia_aislamiento_alternador)]

  # 2. Descargas parciales en el alternador ---------------------------------
  mci_hv_gen_die_descargas_parciales_alternador <-
    gb_ref_taken$mci_hv_gen_die_descarga_altern

  ci_factor_descargas_parciales_alternador <-
     mci_hv_gen_die_descargas_parciales_alternador$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_descargas_parciales_alternador$
        `Criterios de Condición: Descargas Parciales` ==
        descargas_parciales_alternador)]

  ci_cap_descargas_parciales_alternador <-
     mci_hv_gen_die_descargas_parciales_alternador$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_descargas_parciales_alternador$
        `Criterios de Condición: Descargas Parciales` ==
        descargas_parciales_alternador)]

  ci_collar_descargas_parciales_alternador <-
     mci_hv_gen_die_descargas_parciales_alternador$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_descargas_parciales_alternador$
        `Criterios de Condición: Descargas Parciales` ==
        descargas_parciales_alternador)]

  # 3. Secuencia de fases del alternador ------------------------------------
  mci_hv_gen_die_secuencia_alternador <-
    gb_ref_taken$mci_hv_gen_die_secuen_altern

  ci_factor_secuencia_alternador <-
     mci_hv_gen_die_secuencia_alternador$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_secuencia_alternador$
        `Criterios de Condición: Secuencia de Fases` ==
        secuencia_alternador)]

  ci_cap_secuencia_alternador <-
     mci_hv_gen_die_secuencia_alternador$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_secuencia_alternador$
        `Criterios de Condición: Secuencia de Fases` ==
        secuencia_alternador)]

  ci_collar_secuencia_alternador <-
     mci_hv_gen_die_secuencia_alternador$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_secuencia_alternador$
        `Criterios de Condición: Secuencia de Fases` ==
        secuencia_alternador)]

  # 4. Vibraciones en el alternador -----------------------------------------
  mci_hv_gen_die_vibraciones_alternador <-
    gb_ref_taken$mci_hv_gen_die_vibrac_altern

  ci_factor_vibraciones_alternador <-
     mci_hv_gen_die_vibraciones_alternador$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_vibraciones_alternador$
        `Criterios de Condición: Vibraciones` ==
        vibraciones_alternador)]

  ci_cap_vibraciones_alternador <-
     mci_hv_gen_die_vibraciones_alternador$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_vibraciones_alternador$
        `Criterios de Condición: Vibraciones` ==
        vibraciones_alternador)]

  ci_collar_vibraciones_alternador <-
     mci_hv_gen_die_vibraciones_alternador$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_vibraciones_alternador$
        `Criterios de Condición: Vibraciones` ==
        vibraciones_alternador)]

  # 5. Pérdidas en el alternador --------------------------------------------
  mci_hv_gen_die_pérdidas_alternador <-
    gb_ref_taken$mci_hv_gen_die_perdidas_altern

  ci_factor_pérdidas_alternador <-
     mci_hv_gen_die_pérdidas_alternador$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_pérdidas_alternador$
        `Criterios de Condición: Pérdidas` ==
        pérdidas_alternador)]

  ci_cap_pérdidas_alternador <-
     mci_hv_gen_die_pérdidas_alternador$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_pérdidas_alternador$
        `Criterios de Condición: Pérdidas` ==
        pérdidas_alternador)]

  ci_collar_pérdidas_alternador <-
     mci_hv_gen_die_pérdidas_alternador$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_pérdidas_alternador$
        `Criterios de Condición: Pérdidas` ==
        pérdidas_alternador)]

  # 6. Temperatura de arrollamiento del alternador --------------------------
  mci_hv_gen_die_temperatura_arrollamiento_alternador <-
    gb_ref_taken$mci_hv_gen_die_temperat_altern

  ci_factor_temperatura_arrollamiento_alternador <-
     mci_hv_gen_die_temperatura_arrollamiento_alternador$`Factor de Condición de Entrada`[which(
       mci_hv_gen_die_temperatura_arrollamiento_alternador$
        `Criterios de Condición: Temperatura` ==
        temperatura_arrollamiento_alternador)]

  ci_cap_temperatura_arrollamiento_alternador <-
     mci_hv_gen_die_temperatura_arrollamiento_alternador$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_temperatura_arrollamiento_alternador$
        `Criterios de Condición: Temperatura` ==
        temperatura_arrollamiento_alternador)]

  ci_collar_temperatura_arrollamiento_alternador <-
     mci_hv_gen_die_temperatura_arrollamiento_alternador$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_temperatura_arrollamiento_alternador$
        `Criterios de Condición: Temperatura` ==
        temperatura_arrollamiento_alternador)]

  # Factor de condición medida ----------------------------------------------
  factores_motor <- c(ci_factor_rendimiento_motor,
                      ci_factor_velocidad_motor,
                      ci_factor_consumo_motor,
                      ci_factor_freno_motor)

  factor_condición_medida_motor <- mmi(factores_motor,
                                       factor_divisor_1_motor,
                                       factor_divisor_2_motor,
                                       max_no_factores_comb_motor)

  factores_alternador <- c(ci_factor_resistencia_aislamiento_alternador,
                           ci_factor_descargas_parciales_alternador,
                           ci_factor_secuencia_alternador,
                           ci_factor_vibraciones_alternador,
                           ci_factor_pérdidas_alternador,
                           ci_factor_temperatura_arrollamiento_alternador)

  factor_condición_medida_alternador <- mmi(factores_alternador,
                                            factor_divisor_1_alternador,
                                            factor_divisor_2_alternador,
                                            max_no_factores_comb_alternador)
  
  # Límite superior de la condición medida ----------------------------------
  caps_motor <- c(ci_cap_rendimiento_motor,
                  ci_cap_velocidad_motor,
                  ci_cap_consumo_motor,
                  ci_cap_freno_motor)

  cap_condición_medida_motor <- min(caps_motor)

  caps_alternador <- c(ci_cap_resistencia_aislamiento_alternador,
                       ci_cap_descargas_parciales_alternador,
                       ci_cap_secuencia_alternador,
                       ci_cap_vibraciones_alternador,
                       ci_cap_pérdidas_alternador,
                       ci_cap_temperatura_arrollamiento_alternador)

  cap_condición_medida_alternador <- min(caps_alternador)
  
  # Límite inferior de la condición medida ----------------------------------
  collars_motor <- c(ci_collar_rendimiento_motor,
                     ci_collar_velocidad_motor,
                     ci_collar_consumo_motor,
                     ci_collar_freno_motor)

  collar_condición_medida_motor <- max(collars_motor)

  collars_alternador <- c(ci_collar_resistencia_aislamiento_alternador,
                          ci_collar_descargas_parciales_alternador,
                          ci_collar_secuencia_alternador,
                          ci_collar_vibraciones_alternador,
                          ci_collar_pérdidas_alternador,
                          ci_collar_temperatura_arrollamiento_alternador)

  collar_condición_medida_alternador <- max(collars_alternador)

  # Modificador de la condición medida --------------------------------------
  modificador_condición_medida_motor <- data.frame(factor_condición_medida_motor,
                                                   cap_condición_medida_motor,
                                                   collar_condición_medida_motor)

  modificador_condición_medida_alternador <- data.frame(factor_condición_medida_alternador,
                                                        cap_condición_medida_alternador,
                                                        collar_condición_medida_alternador)

  # Entradas de condición observada -----------------------------------------
  oci_mmi_cal_df <-
    gb_ref_taken$observed_cond_modifier_mmi_cal

  oci_mmi_cal_df <-
    oci_mmi_cal_df[which(oci_mmi_cal_df$`Asset Category` == "HV Generador Diésel"), ]

  factor_divisor_1_motor_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(oci_mmi_cal_df$Subcomponent == "Motor")
    ])

  factor_divisor_1_alternador_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`[
      which(oci_mmi_cal_df$Subcomponent == "Alternador")
    ])

  factor_divisor_2_motor_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(oci_mmi_cal_df$Subcomponent == "Motor")
    ])

  factor_divisor_2_alternador_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`[
      which(oci_mmi_cal_df$Subcomponent == "Alternador")
    ])

  max_no_factores_comb_motor_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(oci_mmi_cal_df$Subcomponent == "Motor")
    ])

  max_no_factores_comb_alternador_obs <-
    as.numeric(oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`[
      which(oci_mmi_cal_df$Subcomponent == "Alternador")
    ])

  # CONDICIONES OBSERVADAS DEL MOTOR ----------------------------------------
  # 1. Cubierta del motor ---------------------------------------------------
  oci_hv_gen_die_cubierta_motor <-
    gb_ref_taken$oci_hv_gen_die_cubierta_motor

  oi_factor_cubierta_motor <-
     oci_hv_gen_die_cubierta_motor$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_cubierta_motor$
        `Criterios de Condición: Condición Observada` ==
        cubierta_motor)]

  oi_cap_cubierta_motor <-
     oci_hv_gen_die_cubierta_motor$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_cubierta_motor$
        `Criterios de Condición: Condición Observada` ==
        cubierta_motor)]

  oi_collar_cubierta_motor <-
     oci_hv_gen_die_cubierta_motor$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_cubierta_motor$
        `Criterios de Condición: Condición Observada` ==
        cubierta_motor)]

  # 2. Rodamientos del motor ------------------------------------------------
  oci_hv_gen_die_rodamientos_motor <-
    gb_ref_taken$oci_hv_gen_die_rodamien_motor

  oi_factor_rodamientos_motor <-
     oci_hv_gen_die_rodamientos_motor$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_rodamientos_motor$
        `Criterios de Condición: Condición Observada` ==
        rodamientos_motor)]

  oi_cap_rodamientos_motor <-
     oci_hv_gen_die_rodamientos_motor$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_rodamientos_motor$
        `Criterios de Condición: Condición Observada` ==
        rodamientos_motor)]

  oi_collar_rodamientos_motor <-
     oci_hv_gen_die_rodamientos_motor$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_rodamientos_motor$
        `Criterios de Condición: Condición Observada` ==
        rodamientos_motor)]

  # 3. Sistema de combustible del motor -------------------------------------
  oci_hv_gen_die_combustible_motor <-
    gb_ref_taken$oci_hv_gen_die_combust_motor

  oi_factor_combustible_motor <-
     oci_hv_gen_die_combustible_motor$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_combustible_motor$
        `Criterios de Condición: Condición Observada` ==
        combustible_motor)]

  oi_cap_combustible_motor <-
     oci_hv_gen_die_combustible_motor$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_combustible_motor$
        `Criterios de Condición: Condición Observada` ==
        combustible_motor)]

  oi_collar_combustible_motor <-
     oci_hv_gen_die_combustible_motor$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_combustible_motor$
        `Criterios de Condición: Condición Observada` ==
        combustible_motor)]

  # 4. Sistema de enfriamiento del motor ------------------------------------
  oci_hv_gen_die_enfriamiento_motor <-
    gb_ref_taken$oci_hv_gen_die_enfriamie_motor

  oi_factor_enfriamiento_motor <-
     oci_hv_gen_die_enfriamiento_motor$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_enfriamiento_motor$
        `Criterios de Condición: Condición Observada` ==
        enfriamiento_motor)]

  oi_cap_enfriamiento_motor <-
     oci_hv_gen_die_enfriamiento_motor$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_enfriamiento_motor$
        `Criterios de Condición: Condición Observada` ==
        enfriamiento_motor)]

  oi_collar_enfriamiento_motor <-
     oci_hv_gen_die_enfriamiento_motor$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_enfriamiento_motor$
        `Criterios de Condición: Condición Observada` ==
        enfriamiento_motor)]

  # 5. Sistema de lubricación del motor -------------------------------------
  oci_hv_gen_die_lubricacion_motor <-
    gb_ref_taken$oci_hv_gen_die_lubricac_motor
  
  oi_factor_lubricacion_motor <-
     oci_hv_gen_die_lubricacion_motor$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_lubricacion_motor$
        `Criterios de Condición: Condición Observada` ==
        lubricación_motor)]

  oi_cap_lubricacion_motor <-
     oci_hv_gen_die_lubricacion_motor$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_lubricacion_motor$
        `Criterios de Condición: Condición Observada` ==
        lubricación_motor)]

  oi_collar_lubricacion_motor <-
     oci_hv_gen_die_lubricacion_motor$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_lubricacion_motor$
        `Criterios de Condición: Condición Observada` ==
        lubricación_motor)]

  # 6. Sistema de inducción de aire del motor -------------------------------
  oci_hv_gen_die_aire_motor <-
    gb_ref_taken$oci_hv_gen_die_aire_motor

  oi_factor_aire_motor <-
     oci_hv_gen_die_aire_motor$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_aire_motor$
        `Criterios de Condición: Condición Observada` ==
        aire_motor)]

  oi_cap_aire_motor <-
     oci_hv_gen_die_aire_motor$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_aire_motor$
        `Criterios de Condición: Condición Observada` ==
        aire_motor)]

  oi_collar_aire_motor <-
     oci_hv_gen_die_aire_motor$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_aire_motor$
        `Criterios de Condición: Condición Observada` ==
        aire_motor)]

  # 7. Turbocompresores del motor -------------------------------------------
  oci_hv_gen_die_turbocompresores_motor <-
    gb_ref_taken$oci_hv_gen_die_turbocomp_motor

  oi_factor_turbocompresores_motor <-
     oci_hv_gen_die_turbocompresores_motor$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_turbocompresores_motor$
        `Criterios de Condición: Condición Observada` ==
        turbocompresores_motor)]
        
  oi_cap_turbocompresores_motor <-
     oci_hv_gen_die_turbocompresores_motor$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_turbocompresores_motor$
        `Criterios de Condición: Condición Observada` ==
        turbocompresores_motor)]

  oi_collar_turbocompresores_motor <-
     oci_hv_gen_die_turbocompresores_motor$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_turbocompresores_motor$
        `Criterios de Condición: Condición Observada` ==
        turbocompresores_motor)]

  # 8. Culatas de cilíndros del motor ---------------------------------------
  oci_hv_gen_die_culatas_motor <-
    gb_ref_taken$oci_hv_gen_die_culatas_motor

  oi_factor_culatas_motor <-
     oci_hv_gen_die_culatas_motor$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_culatas_motor$
        `Criterios de Condición: Condición Observada` ==
        culatas_motor)]

  oi_cap_culatas_motor <-
     oci_hv_gen_die_culatas_motor$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_culatas_motor$
        `Criterios de Condición: Condición Observada` ==
        culatas_motor)]

  oi_collar_culatas_motor <-
     oci_hv_gen_die_culatas_motor$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_culatas_motor$
        `Criterios de Condición: Condición Observada` ==
        culatas_motor)]

  # 9. Filtro del motor -----------------------------------------------------
  oci_hv_gen_die_filtro_motor <-
    gb_ref_taken$oci_hv_gen_die_filtro_motor

  oi_factor_filtro_motor <-
     oci_hv_gen_die_filtro_motor$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_filtro_motor$
        `Criterios de Condición: Condición Observada` ==
        filtro_motor)]

  oi_cap_filtro_motor <-
     oci_hv_gen_die_filtro_motor$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_filtro_motor$
        `Criterios de Condición: Condición Observada` ==
        filtro_motor)]

  oi_collar_filtro_motor <-
     oci_hv_gen_die_filtro_motor$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_filtro_motor$
        `Criterios de Condición: Condición Observada` ==
        filtro_motor)]

  # CONDICIONES OBSERVADAS DEL ALTERNADOR -----------------------------------
  # 1. Cubierta del alternador ----------------------------------------------
  oci_hv_gen_die_cubierta_alternador <-
    gb_ref_taken$oci_hv_gen_die_cubierta_altern

  oi_factor_cubierta_alternador <-
     oci_hv_gen_die_cubierta_alternador$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_cubierta_alternador$
        `Criterios de Condición: Condición Observada` ==
        cubierta_alternador)]

  oi_cap_cubierta_alternador <-
     oci_hv_gen_die_cubierta_alternador$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_cubierta_alternador$
        `Criterios de Condición: Condición Observada` ==
        cubierta_alternador)]

  oi_collar_cubierta_alternador <-
     oci_hv_gen_die_cubierta_alternador$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_cubierta_alternador$
        `Criterios de Condición: Condición Observada` ==
        cubierta_alternador)]

  # 2. Aislamiento del alternador -------------------------------------------
  oci_hv_gen_die_aislamiento_alternador <-
    gb_ref_taken$oci_hv_gen_die_aislamie_altern

  oi_factor_aislamiento_alternador <-
     oci_hv_gen_die_aislamiento_alternador$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_aislamiento_alternador$
        `Criterios de Condición: Condición Observada` ==
        aislamiento_alternador)]

  oi_cap_aislamiento_alternador <-
     oci_hv_gen_die_aislamiento_alternador$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_aislamiento_alternador$
        `Criterios de Condición: Condición Observada` ==
        aislamiento_alternador)]

  oi_collar_aislamiento_alternador <-
     oci_hv_gen_die_aislamiento_alternador$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_aislamiento_alternador$
        `Criterios de Condición: Condición Observada` ==
        aislamiento_alternador)]

  # 3. Cables y caja de terminales del alternador ---------------------------
  oci_hv_gen_die_cables_alternador <-
    gb_ref_taken$oci_hv_gen_die_cables_altern

  oi_factor_cables_alternador <-
     oci_hv_gen_die_cables_alternador$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_cables_alternador$
        `Criterios de Condición: Condición Observada` ==
        cables_alternador)]

  oi_cap_cables_alternador <-
     oci_hv_gen_die_cables_alternador$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_cables_alternador$
        `Criterios de Condición: Condición Observada` ==
        cables_alternador)]

  oi_collar_cables_alternador <-
     oci_hv_gen_die_cables_alternador$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_cables_alternador$
        `Criterios de Condición: Condición Observada` ==
        cables_alternador)]

  # 4. Rotor del alternador -------------------------------------------------
  oci_hv_gen_die_rotor_alternador <-
    gb_ref_taken$oci_hv_gen_die_rotor_altern

  oi_factor_rotor_alternador <-
     oci_hv_gen_die_rotor_alternador$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_rotor_alternador$
        `Criterios de Condición: Condición Observada` ==
        rotor_alternador)]

  oi_cap_rotor_alternador <-
     oci_hv_gen_die_rotor_alternador$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_rotor_alternador$
        `Criterios de Condición: Condición Observada` ==
        rotor_alternador)]

  oi_collar_rotor_alternador <-
     oci_hv_gen_die_rotor_alternador$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_rotor_alternador$
        `Criterios de Condición: Condición Observada` ==
        rotor_alternador)]

  # 5. Estator del alternador -----------------------------------------------
  oci_hv_gen_die_estator_alternador <-
    gb_ref_taken$oci_hv_gen_die_estator_altern

  oi_factor_estator_alternador <-
     oci_hv_gen_die_estator_alternador$`Factor de Condición de Entrada`[which(
       oci_hv_gen_die_estator_alternador$
        `Criterios de Condición: Condición Observada` ==
        estator_alternador)]

  oi_cap_estator_alternador <-
     oci_hv_gen_die_estator_alternador$`Límite Superior de Condición de Entrada`[which(
       oci_hv_gen_die_estator_alternador$
        `Criterios de Condición: Condición Observada` ==
        estator_alternador)]

  oi_collar_estator_alternador <-
     oci_hv_gen_die_estator_alternador$`Límite Inferior de Condición de Entrada`[which(
       oci_hv_gen_die_estator_alternador$
        `Criterios de Condición: Condición Observada` ==
        estator_alternador)]

  # Factor de condición observada -------------------------------------------
  factores_motor_obs <- c(oi_factor_cubierta_motor,
                          oi_factor_rodamientos_motor,
                          oi_factor_combustible_motor,
                          oi_factor_enfriamiento_motor,
                          oi_factor_lubricacion_motor,
                          oi_factor_aire_motor,
                          oi_factor_turbocompresores_motor,
                          oi_factor_culatas_motor,
                          oi_factor_filtro_motor)

  factor_condición_observada_motor <- mmi(factores_motor_obs,
                                          factor_divisor_1_motor_obs,
                                          factor_divisor_2_motor_obs,
                                          max_no_factores_comb_motor_obs)

  factores_alternador_obs <- c(oi_factor_cubierta_alternador,
                               oi_factor_aislamiento_alternador,
                               oi_factor_cables_alternador,
                               oi_factor_rotor_alternador,
                               oi_factor_estator_alternador)

  factor_condición_observada_alternador <- mmi(factores_alternador_obs,
                                               factor_divisor_1_alternador_obs,
                                               factor_divisor_2_alternador_obs,
                                               max_no_factores_comb_alternador_obs)

  # Límite superior de la condición observada -------------------------------
  caps_motor_obs <- c(oi_cap_cubierta_motor,
                      oi_cap_rodamientos_motor,
                      oi_cap_combustible_motor,
                      oi_cap_enfriamiento_motor,
                      oi_cap_lubricacion_motor,
                      oi_cap_aire_motor,
                      oi_cap_turbocompresores_motor,
                      oi_cap_culatas_motor,
                      oi_cap_filtro_motor)

  cap_condición_observada_motor <- min(caps_motor_obs)

  caps_alternador_obs <- c(oi_cap_cubierta_alternador,
                           oi_cap_aislamiento_alternador,
                           oi_cap_cables_alternador,
                           oi_cap_rotor_alternador,
                           oi_cap_estator_alternador)

  cap_condición_observada_alternador <- min(caps_alternador_obs)

  # Límite inferior de la condición observada -------------------------------
  collars_motor_obs <- c(oi_collar_cubierta_motor,
                         oi_collar_rodamientos_motor,
                         oi_collar_combustible_motor,
                         oi_collar_enfriamiento_motor,
                         oi_collar_lubricacion_motor,
                         oi_collar_aire_motor,
                         oi_collar_turbocompresores_motor,
                         oi_collar_culatas_motor,
                         oi_collar_filtro_motor)

  collar_condición_observada_motor <- max(collars_motor_obs)

  collars_alternador_obs <- c(oi_collar_cubierta_alternador,
                              oi_collar_aislamiento_alternador,
                              oi_collar_cables_alternador,
                              oi_collar_rotor_alternador,
                              oi_collar_estator_alternador)

  collar_condición_observada_alternador <- max(collars_alternador_obs)

  # Modificador de la condición observada -----------------------------------
  modificador_condición_observada_motor <- data.frame(factor_condición_observada_motor,
                                                      cap_condición_observada_motor,
                                                      collar_condición_observada_motor)

  modificador_condición_observada_alternador <- data.frame(factor_condición_observada_alternador,
                                                           cap_condición_observada_alternador,
                                                           collar_condición_observada_alternador)

  # Factor de puntaje de salud del motor ------------------------------------
  factor_puntaje_salud_motor <-
    health_score_excl_ehv_132kv_tf(factor_condición_observada_motor,
                                   factor_condición_medida_motor)
  
  # Límite superior del puntaje de salud del motor --------------------------
  cap_puntaje_salud_motor <- min(cap_condición_observada_motor,
                                 cap_condición_medida_motor)

  # Límite inferior del puntaje de salud del motor --------------------------
  collar_puntaje_salud_motor <- min(collar_condición_observada_motor,
                                    collar_condición_medida_motor)
  
  # Modificador de puntuación de salud del motor ----------------------------
  modificador_puntuación_salud_motor <- data.frame(factor_puntaje_salud_motor,
                                                   cap_puntaje_salud_motor,
                                                   collar_puntaje_salud_motor)

  # Factor de puntaje de salud del alternador -------------------------------
  factor_puntaje_salud_alternador <-
    health_score_excl_ehv_132kv_tf(factor_condición_observada_alternador,
                                   factor_condición_medida_alternador)
  
  # Límite superior del puntaje de salud del alternador ---------------------
  cap_puntaje_salud_alternador <- min(cap_condición_observada_alternador,
                                      cap_condición_medida_alternador)

  # Límite inferior del puntaje de salud del alternador ---------------------
  collar_puntaje_salud_alternador <- min(collar_condición_observada_alternador,
                                         collar_condición_medida_alternador)

  # Modificador de puntuación de salud del alternador -----------------------
  modificador_puntuación_salud_alternador <- data.frame(factor_puntaje_salud_alternador,
                                                        cap_puntaje_salud_alternador,
                                                        collar_puntaje_salud_alternador)

  # Puntuación de salud actual del grupo electrógeno ------------------------ 
  puntaje_salud_actual <-
    max(current_health(puntaje_salud_inicial_motor,
                       modificador_puntuación_salud_motor$factor_puntaje_salud_motor,
                       modificador_puntuación_salud_motor$cap_puntaje_salud_motor,
                       modificador_puntuación_salud_motor$collar_puntaje_salud_motor,
                       reliability_factor = factor_confiabilidad),
        current_health(puntaje_salud_inicial_alternador,
                       modificador_puntuación_salud_alternador$factor_puntaje_salud_alternador,
                       modificador_puntuación_salud_alternador$cap_puntaje_salud_alternador,
                       modificador_puntuación_salud_alternador$collar_puntaje_salud_alternador,
                       reliability_factor = factor_confiabilidad))

  # Probabilidad de falla (Probability of failure - PoF) actual para el frupo electrógeno
  probabilidad_falla <- k *
    (1 + (c * puntaje_salud_actual) +
       (((c * puntaje_salud_actual)^2) / factorial(2)) +
       (((c * puntaje_salud_actual)^3) / factorial(3)))

  return(data.frame(pof = probabilidad_falla, chs = puntaje_salud_actual))
}