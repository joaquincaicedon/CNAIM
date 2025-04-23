#' @title Consecuencias de falla para un transformador de 13.2 kV
#' @description Esta función calcula las consecuencias de falla
#' para un transformador de 13.2 kV (cf. sección 7, página 75, CNAIM, 2021).
#' @inheritParams f_cof_transformador_13_2kv
#' @inheritParams s_cof_transformador_13_2kv
#' @inheritParams e_cof_transformador_13_2kv
#' @inheritParams n_cof_transformador_13_2kv
#' @param gb_ref_given Parámetro opcional para usar valores de referencia personalizados
#' @return Numérico. Consecuencias de falla para un transformador de 13.2 kV.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Consecuencias de falla para un transformador de 13.2 kV
#' cof_transformador_13_2kv(kva = 500,
#'                          acceso = "Tipo B",
#'                          tipo_riesgo = "Alto",
#'                          riesgo_ubicacion = "Alto",
#'                          distancia_agua = 50,
#'                          acotado = "No",
#'                          no_usuarios = 100,
#'                          kva_usuario = 1)

cof_transformador_13_2kv <- function(kva,
                                     acceso,
                                     tipo_riesgo,
                                     riesgo_ubicacion,
                                     distancia_agua,
                                     acotado,
                                     no_usuarios,
                                     kva_usuario,
                                     gb_ref_given = NULL) {

  finance <- f_cof_transformador_13_2kv(kva, acceso, gb_ref_given)

  safety <- s_cof_transformador_13_2kv(tipo_riesgo, riesgo_ubicacion,
                                       tipo_transformador = "Transformador 13.2kV", gb_ref_given)

  environmental <- e_cof_transformador_13_2kv(tipo_transformador = "Transformador 13.2kV",
                                              rated_capacity = kva,
                                              distancia_agua, acotado,
                                              gb_ref_given)

  network <- n_cof_transformador_13_2kv(tipo_transformador = "Transformador 13.2kV",
                                        no_usuarios, kva_usuario, gb_ref_given)

  CoF <- finance + safety + environmental + network

  return(data.frame(CoF_financiero = finance, CoF_seguridad = safety,
                    CoF_ambiental = environmental, CoF_red = network, CoF_total = CoF))
}
