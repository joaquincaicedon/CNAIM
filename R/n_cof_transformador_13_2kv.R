#' @importFrom magrittr %>%
#' @title Costo de Falla de Red para Todas las Categorías de Activos Excluyendo
#' Transformadores EHV y 132kV
#' @description Esta función calcula el costo de falla de red para
#' todas las categorías de activos excluyendo transformadores EHV y 132kV.
#' (cf. sección 7.6, página 87, CNAIM, 2021). El costo de falla de red
#' se utiliza en la derivación de las consecuencias de falla; ver \code{\link{cof}}().
#' @param tipo_transformador Cadena de texto.
#' Opciones: \code{asset_type_ncf = c("Transformador 13.2kV")}
#' @param no_usuarios Numérico. El número de clientes
#' alimentados por un activo individual.
#' @param kva_usuario Numérico. Si el activo tiene una demanda excepcionalmente alta
#' por cliente, especifique el kVA por cliente. Una configuración de \code{"Default"}
#' resulta en un factor de multiplicación de 1 (cf. tabla 18, página 90, CNAIM, 2021).
#' @param gb_ref_given Parámetro opcional para usar valores de referencia personalizados.
#' @return Numérico. Costo de falla de red.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Costo de falla de red para un transformador de 13.2 kV con 300 clientes
#' # y 2 kVA por cliente.
#' n_cof_transformador_13_2kv(tipo_transformador = "Transformador 13.2kV",
#'                            no_usuarios = 300,
#'                            kva_usuario = 2)

n_cof_transformador_13_2kv <- function(tipo_transformador = "Transformador 13.2kV",
                                       no_usuarios,
                                       kva_usuario = "Default",
                                       gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL
  # due to NSE notes in R CMD check

  # Get category ------------------------------------------------------------
  tipo_transformador <- "Transformador 13.2kV"

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == tipo_transformador) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure <-
    gb_ref_taken$reference_costs_of_failure

  reference_costs_of_failure_tf <- dplyr::filter(reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   tipo_transformador)

  # Reference network cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Customer factor ---------------------------------------------------------
  ref_nw_perf_cost_fail_lv_hv <- gb_ref_taken$ref_nw_perf_cost_fail_lv_hv
  ref_nw_perf_cost_fail_lv_hv_tf <- dplyr::filter(ref_nw_perf_cost_fail_lv_hv,
                                                  `Asset Category` ==
                                                    asset_category)

  ref_no_cust <-
    ref_nw_perf_cost_fail_lv_hv_tf$`Reference Number of Connected Customers`

  customer_no_adjust_lv_hv_asset <- gb_ref_taken$customer_no_adjust_lv_hv_asset

  for (n in 1:nrow(customer_no_adjust_lv_hv_asset)){
    if (kva_usuario == 'Default'){
      adj_cust_no <- 1
      break
    } else if (kva_usuario >= as.numeric(
      customer_no_adjust_lv_hv_asset$Lower[n]) &
      kva_usuario < as.numeric(
        customer_no_adjust_lv_hv_asset$Upper[n])){
      adj_cust_no <-
        customer_no_adjust_lv_hv_asset$
        `No. of Customers to be used in the derivation of Customer Factor`[n]
      break
    }
  }

  adj_cust_no <- gsub("([0-9]+).*$", "\\1", adj_cust_no) %>% as.numeric()

  customer_factor <- (adj_cust_no * no_usuarios) / ref_no_cust

  # Customer sensitivity factor ---------------------------------------------
  customer_sensitivity_factor <- 1 # See section 7.6.2.2, p. 89 in CNAIM (2021)

  # Network perfomance consequence factor -----------------------------------
  network_performance_consequence_factor <- customer_factor *
    customer_sensitivity_factor

  # Network performance cost of failure -------------------------------------
  network_cof <- network_performance_consequence_factor * ncost

  return(network_cof)
}