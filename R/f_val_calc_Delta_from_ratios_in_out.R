#' Delta from ratios in and out?, (Re/Ro)-1, is this really the same?
#'
#' First calculates R_ref and R_cham with \code{\link{f_val_calc_isotope_ratio}}
#'
#' \deqn{Delta_from_ratios_in_out <- (R_ref / R_cham) - 1}
#' this quantity is not in per mil
#'
#' @param ref.12Ce xxxPARAMxxx
#' @param ref.13Ce xxxPARAMxxx
#' @param cham.12Co xxxPARAMxxx
#' @param cham.13Co xxxPARAMxxx
#'
#' @return Delta_from_ratios_in_out xxxRETURNxxx
#'
f_val_calc_Delta_from_ratios_in_out <-
function# Delta from ratios in and out?, (Re/Ro)-1, is this really the same?
###
(ref.12Ce
###
, ref.13Ce
###
, cham.12Co
###
, cham.13Co
###
)
{
  ##details<<
  ## First calculates R_ref and R_cham with \code{\link{f_val_calc_isotope_ratio}}
  R_ref  <- f_val_calc_isotope_ratio(ref.12Ce, ref.13Ce);
  R_cham <- f_val_calc_isotope_ratio(cham.12Co, cham.13Co);

  ##details<<
  ##\deqn{Delta_from_ratios_in_out <- (R_ref / R_cham) - 1}
  ## this quantity is not in per mil
  Delta_from_ratios_in_out <- (R_ref / R_cham) - 1; # not in per mil

  return( Delta_from_ratios_in_out );
  ### Delta_from_ratios_in_out
}

