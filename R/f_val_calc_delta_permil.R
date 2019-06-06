#' delta permil (Isotopic composition (d) per mil)
#'
#' Multiply 1000 by \code{\link{f_val_calc_delta_proportion}}
#'
#' @param obs_12C xxxPARAMxxx
#' @param obs_13C xxxPARAMxxx
#' @param R_std xxxPARAMxxx
#'
#' @return delta_permil xxxRETURNxxx
#'
f_val_calc_delta_permil <-
function# delta permil (Isotopic composition (d) per mil)
###
(obs_12C
###
, obs_13C
###
, R_std
###
)
{
  ##details<<
  ## Multiply 1000 by \code{\link{f_val_calc_delta_proportion}}
  delta_permil <- 1000 * f_val_calc_delta_proportion(obs_12C, obs_13C, R_std); # not in per mil

  return( delta_permil );
  ### delta_permil
}

