#' Title
#'
#' @param obs.12C
#' @param obs.13C
#' @param fo.13C
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_total_mol_fraction_CO2 <-
function# Total mol fraction CO2
###
(obs.12C
###
, obs.13C
###
, fo.13C
###
)
{
  ##details<<
  ##\deqn{total_mol_fraction_CO2 = (obs.12C + obs.13C) / (1 - fo.13C)}
  total_mol_fraction_CO2 <- (obs.12C + obs.13C) / (1 - fo.13C);

  return( total_mol_fraction_CO2 );
  ### total_mol_fraction_CO2
}

