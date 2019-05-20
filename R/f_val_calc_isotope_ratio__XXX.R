#' Isotope ratio
#'
#' \deqn{isotope_ratio = obs_13C / obs_12C}
#'
#' @param obs_12C
#' @param obs_13C
#'
#' @return isotope_ratio
#' @export
#'
#' @examples
f_val_calc_isotope_ratio <-
function# Isotope ratio
###
(obs_12C
###
, obs_13C
###
)
{
  ##details<<
  ##\deqn{isotope_ratio = obs_13C / obs_12C}
  isotope_ratio <- obs_13C / obs_12C;

  return( isotope_ratio );
  ### isotope_ratio
}

