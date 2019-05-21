#' Helper for \code{\link{f_val_bs_matrix}}, if the vector has zero length, return NA (for when using only TDL and not Licor)
#'
#' @param val
#'
#' @return val
#'
#' @examples
f_val_bs_matrix_check <-
function# Helper for \code{\link{f_val_bs_matrix}}, if the vector has zero length, return NA (for when using only TDL and not Licor)
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

