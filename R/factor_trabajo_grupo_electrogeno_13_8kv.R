#' @title Factor de trabajo para grupos electrógenos diésel de 13.8 kV
#' @description Esta función calcula el factor de trabajo para 
#' grupos electrógenos diésel de 13.8 kV en función del porcentaje máximo 
#' de utilización en condiciones normales de funcionamiento. El factor de
#' trabajo se utiliza para calcular la vida útil esperada de un activo. 
#' Véase, por ejemplo, \code{\link{expected_life}}(). Para obtener 
#' información más general sobre la derivación del factor de trabajo, 
#' consulte la sección 6.6 en la página 51 de CNAIM (2021).
#' @param utilisation_pct Numérico. El porcentaje máximo de utilización 
#' en condiciones normales de funcionamiento.
#' @param gb_ref_given Parámetro opcional para utilizar valores de 
#' referencia personalizados
#' @return Numérico. Factor de trabajo para grupo electrógeno diésel de 13.8 kV.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' Y adaptación de CNAIM para considerar grupos electrógenos diésel.
#' @export
#' @examples
#' factor_trabajo_grupo_electrogeno_13_8kv(utilisation_pct = 95)

factor_trabajo_grupo_electrogeno_13_8kv <- function(utilisation_pct = "Default",
                                                    gb_ref_given = NULL) {

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  duty_factor_table <- gb_ref_taken$factor_trabajo_tab_generador

  for (n in 1:nrow(duty_factor_table)){
    if (utilisation_pct == 'Default'){
      duty_factor <- duty_factor_table$`Duty Factor`[nrow(duty_factor_table)]
      break
    } else if (utilisation_pct > as.numeric(duty_factor_table$Lower[n]) &
        utilisation_pct <= as.numeric(duty_factor_table$Upper[n])){
      duty_factor <- duty_factor_table$`Duty Factor`[n]
      break
    }
  }

  return(duty_factor)
}