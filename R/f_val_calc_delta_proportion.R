#' Title
#'
#' @param obs.12C
#' @param obs.13C
#' @param R.std
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_delta_proportion <-
function# delta as proportion (Isotopic composition (d) as fraction)
###   I prefer using the fraction for calculations and then converting to composition at the end
(obs.12C
###
, obs.13C
###
, R.std
###
)
{
  ##details<<
  ## First calculates R.obs with \code{\link{f_val_calc_isotope_ratio}}
  ##\deqn{delta.proportion <- (R.obs / R.std) - 1}
  ## this quantity is not in per mil
  R.obs <- f_val_calc_isotope_ratio(obs.12C, obs.13C);
  delta.proportion <- (R.obs / R.std) - 1; # not in per mil

  return( delta.proportion );
  ### delta.proportion
}

