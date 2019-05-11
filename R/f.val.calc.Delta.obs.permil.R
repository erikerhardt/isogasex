f.val.calc.Delta.obs.permil <-
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
  ## Multiply 1000 by \code{\link{f.val.calc.Delta.obs}}
  Delta.obs.permil <- 1000 * f.val.calc.Delta.obs( reference.delta.e, chamber.delta.o, xi);

  return( Delta.obs.permil );
  ### Delta.obs.permil
}

