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
  measured_condition_modifier_motor <- data.frame(factor_condición_medida_motor,
                                                  cap_condición_medida_motor,
                                                  collar_condición_medida_motor)

  measured_condition_modifier_alternador <- data.frame(factor_condición_medida_alternador,
                                                       cap_condición_medida_alternador,
                                                       collar_condición_medida_alternador)


browser()

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
      which(mcm_mmi_cal_df$Subcomponent == "Motor")
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
     mci_hv_gen_die_cubierta_motor$`Límite Superior de Condición de Entrada`[which(
       mci_hv_gen_die_cubierta_motor$
        `Criterios de Condición: Condición Observada` ==
        cubierta_motor)]

  oi_collar_cubierta_motor <-
     mci_hv_gen_die_cubierta_motor$`Límite Inferior de Condición de Entrada`[which(
       mci_hv_gen_die_cubierta_motor$
        `Criterios de Condición: Condición Observada` ==
        cubierta_motor)]

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
  # Límite superior de la condición observada -------------------------------
  # Límite inferior de la condición observada -------------------------------
  # Modificador de la condición observada -----------------------------------



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
