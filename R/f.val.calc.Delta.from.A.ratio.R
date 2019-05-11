f.val.calc.Delta.from.A.ratio <-
function# D from A ratio, (Ro/(13A/12A))-1, should be the same as Dobs above
###
(cham.12Co
###
, cham.13Co
###
, TDL.13A
###
, TDL.12A
###
)
{
  ##details<<
  ## First calculates R.cham with \code{\link{f.val.calc.isotope.ratio}}
  R.cham <- f.val.calc.isotope.ratio(cham.12Co, cham.13Co);

  ##details<<
  ##\deqn{Delta.from.A.ratio <- (R.cham / (TDL.13A / TDL.12A)) - 1}
  ## this quantity is not in per mil
  Delta.from.A.ratio <- (R.cham / (TDL.13A / TDL.12A)) - 1; # not in per mil

  return( Delta.from.A.ratio );
  ### Delta.from.A.ratio
}

