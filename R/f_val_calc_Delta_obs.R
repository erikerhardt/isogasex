#' Delta, observed discrimination
#'
#' \deqn{temp_xidode = xi * (chamber_delta_o - reference_delta_e)}
#'
#' \deqn{Delta_obs = temp_xidode / (1 + chamber_delta_o - temp_xidode)}
#'
#' @param reference_delta_e xxxPARAMxxx
#' @param chamber_delta_o xxxPARAMxxx
#' @param xi xxxPARAMxxx
#'
#' @return Delta_obs xxxRETURNxxx
#'
f_val_calc_Delta_obs <-
function# Delta, observed discrimination
###
(reference_delta_e
###
, chamber_delta_o
###
, xi
###
)
{
  ##details<<
  ##\deqn{temp_xidode = xi * (chamber_delta_o - reference_delta_e)}
  ##\deqn{Delta_obs = temp_xidode / (1 + chamber_delta_o - temp_xidode)}
  temp_xidode <- xi * (chamber_delta_o - reference_delta_e);
  Delta_obs <- temp_xidode / (1 + chamber_delta_o - temp_xidode);

  return( Delta_obs );
  ### Delta_obs
}

