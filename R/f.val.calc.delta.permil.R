f.val.calc.delta.permil <-
function# delta permil (Isotopic composition (d) per mil)
###
(obs.12C
###
, obs.13C
###
, R.std
###
)
{
  ##details<<
  ## Multiply 1000 by \code{\link{f.val.calc.delta.proportion}}
  delta.permil <- 1000 * f.val.calc.delta.proportion(obs.12C, obs.13C, R.std); # not in per mil

  return( delta.permil );
  ### delta.permil
}

