#' pc using simple D for gm, includes boundary layer
#'
#' \deqn{pc = (Delta * pa - a_b * (pa - ps) - a * ps) / (b_gm - a)}
#'
#' @param Delta xxxPARAMxxx
#' @param pa xxxPARAMxxx
#' @param ps xxxPARAMxxx
#' @param a xxxPARAMxxx
#' @param b_gm xxxPARAMxxx
#' @param a_b xxxPARAMxxx
#'
#' @return pc xxxRETURNxxx
#'
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

