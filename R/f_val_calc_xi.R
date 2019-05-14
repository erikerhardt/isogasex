#' Title
#'
#' @param Ce
#' @param Co
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_xi <-
function# xi
###
(Ce
###
, Co
###
)
{
  ##details<<
  ##\deqn{xi = Ce / (Ce - Co)}
  xi <- Ce / (Ce - Co);

  return( xi );
  ### xi
}

