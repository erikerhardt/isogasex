#' Delta from ratios in and out?, (Re/Ro)-1, is this really the same?
#'
#' First calculates R_ref and R_cham with \code{\link{f_val_calc_isotope_ratio}}
#'
#' \deqn{Delta_from_ratios_in_out <- (R_ref / R_cham) - 1}
#' this quantity is not in per mil
#'
#' @param ref_12Ce xxxPARAMxxx
#' @param ref_13Ce xxxPARAMxxx
#' @param cham_12Co xxxPARAMxxx
#' @param cham_13Co xxxPARAMxxx
#'
#' @return Delta_from_ratios_in_out xxxRETURNxxx
#'
f_val_calc_Delta_from_ratios_in_out <-
function# Delta from ratios in and out?, (Re/Ro)-1, is this really the same?
###
(ref_12Ce
###
, ref_13Ce
###
, cham_12Co
###
, cham_13Co
###
)
{
  ##details<<
  ## First calculates R_ref and R_cham with \code{\link{f_val_calc_isotope_ratio}}
  R_ref  <- f_val_calc_isotope_ratio(ref_12Ce, ref_13Ce);
  R_cham <- f_val_calc_isotope_ratio(cham_12Co, cham_13Co);

  ##details<<
  ##\deqn{Delta_from_ratios_in_out <- (R_ref / R_cham) - 1}
  ## this quantity is not in per mil
  Delta_from_ratios_in_out <- (R_ref / R_cham) - 1; # not in per mil

  return( Delta_from_ratios_in_out );
  ### Delta_from_ratios_in_out
}

