f_val_calc_Delta_obs_permil <-
function# Delta per mil, observed discrimination
###
(reference.delta.e
###
, chamber.delta.o
###
, xi
###
)
{
  ##details<<
  ## Multiply 1000 by \code{\link{f_val_calc_Delta_obs}}
  Delta.obs.permil <- 1000 * f_val_calc_Delta_obs( reference.delta.e, chamber.delta.o, xi);

  return( Delta.obs.permil );
  ### Delta.obs.permil
}

