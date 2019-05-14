#' Title
#'
#' @param Delta
#' @param pa
#' @param ps
#' @param a
#' @param b.gm
#' @param a.b
#'
#' @return
#' @export
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
, b.gm
###
, a.b
###
)
{
  ##details<<
  ##\deqn{pc = (Delta * pa - a.b * (pa - ps) - a * ps) / (b.gm - a)}
  pc <- (Delta * pa - a.b * (pa - ps) - a * ps) / (b.gm - a);

  return( pc );
  ### pc
}

