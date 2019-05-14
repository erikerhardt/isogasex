#' Title
#'
#' @param obs.12C
#' @param obs.13C
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_isotope_ratio <-
function# Isotope ratio
###
(obs.12C
###
, obs.13C
###
)
{
  ##details<<
  ##\deqn{isotope.ratio = obs.13C / obs.12C}
  isotope.ratio <- obs.13C / obs.12C;

  return( isotope.ratio );
  ### isotope.ratio
}

