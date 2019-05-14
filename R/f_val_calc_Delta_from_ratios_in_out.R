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
  ## First calculates R.ref and R.cham with \code{\link{f_val_calc_isotope_ratio}}
  R.ref  <- f_val_calc_isotope_ratio(ref.12Ce, ref.13Ce);
  R.cham <- f_val_calc_isotope_ratio(cham.12Co, cham.13Co);

  ##details<<
  ##\deqn{Delta.from.ratios.in.out <- (R.ref / R.cham) - 1}
  ## this quantity is not in per mil
  Delta.from.ratios.in.out <- (R.ref / R.cham) - 1; # not in per mil

  return( Delta.from.ratios.in.out );
  ### Delta.from.ratios.in.out
}

