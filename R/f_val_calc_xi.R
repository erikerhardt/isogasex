#' xi
#'
#' \deqn{xi = Ce / (Ce - Co)}
#'
#' @param Ce xxxPARAMxxx
#' @param Co xxxPARAMxxx
#'
#' @return xi xxxRETURNxxx
#'
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

