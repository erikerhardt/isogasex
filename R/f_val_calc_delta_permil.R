#' Title
#'
#' @param obs.12C
#' @param obs.13C
#' @param R_std
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_delta_permil <-
function# delta permil (Isotopic composition (d) per mil)
###
(obs.12C
###
, obs.13C
###
, R_std
###
)
{
  ##details<<
  ## Multiply 1000 by \code{\link{f_val_calc_delta_proportion}}
  delta_permil <- 1000 * f_val_calc_delta_proportion(obs.12C, obs.13C, R_std); # not in per mil

  return( delta_permil );
  ### delta_permil
}

