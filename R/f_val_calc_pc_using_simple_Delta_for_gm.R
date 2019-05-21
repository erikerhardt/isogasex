#' pc using simple D for gm, includes boundary layer
#'
#' \deqn{pc = (Delta * pa - a_b * (pa - ps) - a * ps) / (b_gm - a)}
#'
#' @param Delta
#' @param pa
#' @param ps
#' @param a
#' @param b_gm
#' @param a_b
#'
#' @return pc
#'
#' @examples
f_val_calc_pc_using_simple_Delta_for_gm <-
function# pc using simple D for gm, includes boundary layer
###
(Delta
###
, pa
###
, ps
###
, a
###
, b_gm
###
, a_b
###
)
{
  ##details<<
  ##\deqn{pc = (Delta * pa - a_b * (pa - ps) - a * ps) / (b_gm - a)}
  pc <- (Delta * pa - a_b * (pa - ps) - a * ps) / (b_gm - a);

  return( pc );
  ### pc
}

