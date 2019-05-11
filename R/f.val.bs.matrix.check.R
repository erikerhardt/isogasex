f.val.bs.matrix.check <-
function# Helper for \code{\link{f.val.bs.matrix}}, if the vector has zero length, return NA (for when using only TDL and not Licor)
###
(val
###
)
{
  if (length(val) == 0) {
    return(NA);
    ### NA
  } else {
    return(val);
    ### val
  }
}

