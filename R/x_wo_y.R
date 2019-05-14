#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
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

