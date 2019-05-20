#' Delta per mil, observed discrimination
#'
#' Multiply 1000 by \code{\link{f_val_calc_Delta_obs}}
#'
#' @param reference_delta_e
#' @param chamber_delta_o
#' @param xi
#'
#' @return Delta_obs_permil
#' @export
#'
#' @examples
f_val_calc_Delta_obs_permil <-
function# Delta per mil, observed discrimination
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
  ## Multiply 1000 by \code{\link{f_val_calc_Delta_obs}}
  Delta_obs_permil <- 1000 * f_val_calc_Delta_obs( reference_delta_e, chamber_delta_o, xi);

  return( Delta_obs_permil );
  ### Delta_obs_permil
}

