#' x without y
#'
#' @param x xxxPARAMxxx
#' @param y xxxPARAMxxx
#'
#' @return x without y
#'
x_wo_y <-
function# define function to find indices that were excluded
###
(x
###
,y
###
)
{
  return( x[!x %in% y] ); # x without y
  ### x without y
}

