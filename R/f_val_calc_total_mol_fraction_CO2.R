#' Total mol fraction CO2
#'
#' \deqn{total_mol_fraction_CO2 = (obs_12C + obs_13C) / (1 - fo_13C)}
#'
#' @param obs_12C xxxPARAMxxx
#' @param obs_13C xxxPARAMxxx
#' @param fo_13C xxxPARAMxxx
#'
#' @return total_mol_fraction_CO2 xxxRETURNxxx
#'
f_val_calc_total_mol_fraction_CO2 <-
function# Total mol fraction CO2
###
(obs_12C
###
, obs_13C
###
, fo_13C
###
)
{
  ##details<<
  ##\deqn{total_mol_fraction_CO2 = (obs_12C + obs_13C) / (1 - fo_13C)}
  total_mol_fraction_CO2 <- (obs_12C + obs_13C) / (1 - fo_13C);

  return( total_mol_fraction_CO2 );
  ### total_mol_fraction_CO2
}

