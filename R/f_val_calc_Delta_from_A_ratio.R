#' D from A ratio, (Ro/(13A/12A))-1, should be the same as Dobs above
#'
#' First calculates R_cham with \code{\link{f_val_calc_isotope_ratio}}
#'
#' \deqn{Delta_from_A_ratio <- (R_cham / (TDL_13A / TDL_12A)) - 1}
#' this quantity is not in per mil
#'
#' @param cham_12Co xxxPARAMxxx
#' @param cham_13Co xxxPARAMxxx
#' @param TDL_13A xxxPARAMxxx
#' @param TDL_12A xxxPARAMxxx
#'
#' @return Delta_from_A_ratio xxxRETURNxxx
#'
f_val_calc_Delta_from_A_ratio <-
function# D from A ratio, (Ro/(13A/12A))-1, should be the same as Dobs above
###
(cham_12Co
###
, cham_13Co
###
, TDL_13A
###
, TDL_12A
###
)
{
  ##details<<
  ## First calculates R_cham with \code{\link{f_val_calc_isotope_ratio}}
  R_cham <- f_val_calc_isotope_ratio(cham_12Co, cham_13Co);

  ##details<<
  ##\deqn{Delta_from_A_ratio <- (R_cham / (TDL_13A / TDL_12A)) - 1}
  ## this quantity is not in per mil
  Delta_from_A_ratio <- (R_cham / (TDL_13A / TDL_12A)) - 1; # not in per mil

  return( Delta_from_A_ratio );
  ### Delta_from_A_ratio
}

