#' Title
#'
#' @param reference_delta_e
#' @param chamber_delta_o
#' @param xi
#'
#' @return
#' @export
#'
#' @examples
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

