#' @importFrom magrittr %>%
#' @title Consecuencias Financieras de Falla para un Transformador de 13.2 kV
#' @description Esta función calcula las consecuencias financieras de falla
#' (cf. sección 7.3, página 79, CNAIM, 2021). Las consecuencias financieras
#' de falla se utilizan en la derivación de las consecuencias de falla,
#' ver \code{\link{cof}}().
#' @param kva Numérico. La potencia nominal del transformador medida en kVA
#' para un transformador de 13.2 kV. La potencia nominal se utiliza para derivar el
#' factor financiero de tipo. Para una descripción general del factor financiero de tipo, 
#' ver sección 7.3.3.1 en la página 80 de CNAIM (2021). Una configuración de 
#' \code{"Default"} resultará en un factor financiero de tipo igual a 1
#' (cf. sección D1.2.1, página 178, CNAIM, 2021).
#' @param acceso Cadena. Relacionado con la accesibilidad del transformador.
#' Opciones: \code{type = c("Tipo A", "Tipo B", "Tipo C", "Default")}.
#' Una configuración de \code{"Tipo A"} - Acceso normal.
#' Una configuración de \code{"Tipo B"} - Acceso restringido o espacio de trabajo confinado.
#' Una configuración de \code{"Tipo C"} - Subestación subterránea.
#' Una configuración de \code{"Default"} - Acceso normal, por lo tanto, igual a la configuración de \code{"Tipo A"}
#' (cf. tabla 221, página 180, CNAIM, 2021).
#' @param gb_ref_given parámetro opcional para usar valores de referencia personalizados.
#' @return Numérico. Consecuencias financieras de falla para un transformador de 6.6/11 kV.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Consecuencias financieras de falla para un transformador de 13.2 kV
#' f_cof_transformador_13_2kv(kva = 500, acceso = "Default")

f_cof_transformador_13_2kv <- function(kva = "Default",
                                       acceso = "Default",
                                       gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL
  # due to NSE notes in R CMD check

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  # Get category ------------------------------------------------------------
  asset_type <- "Transformador 13.2kV"

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == asset_type) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` == asset_type)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factors <- gb_ref_taken$type_financial_factors
  type_financial_factors_tf <- dplyr::filter(type_financial_factors,
                                             `Asset Register Category` == asset_type)

  if (kva == 'Default'){
    type_financial_factor <- 1
  } else {
    for (n in 1:nrow(type_financial_factors_tf)){
      lower <- as.numeric(type_financial_factors_tf$Lower[n])
      upper <- as.numeric(type_financial_factors_tf$Upper[n])
      if (kva >= lower & kva < upper){
        type_financial_factor <- type_financial_factors_tf$`Type Financial Factor`[n]
        break
      }
    }
  }

  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref_taken$access_factor_swg_tf_asset
  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                             `Asset Category` == asset_category)

  if (acceso == 'Default') acceso <- "Tipo A"
  if (acceso == "Tipo A") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type A Criteria - Normal Access ( & Default Value)`
  }
  else if (acceso == "Tipo B") {
    access_finacial_factor <-
      access_financial_factors_tf$
`Access Factor: Type B Criteria - Constrained Access or Confined Working Space`
  }
  else if (acceso == "Tipo C") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type C Criteria - Underground substation`
  }

  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_finacial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost)
}