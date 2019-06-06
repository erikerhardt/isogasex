#' pc using simple D for modeling
#'
#' \deqn{pc = (pa * (Delta - a)) / (b_modeling - a)}
#'
#' @param Delta xxxPARAMxxx
#' @param pa xxxPARAMxxx
#' @param a xxxPARAMxxx
#' @param b_modeling xxxPARAMxxx
#'
#' @return pc xxxRETURNxxx
#'
f_val_calc_pc_using_simple_Delta_for_modeling <-
function# pc using simple D for modeling
###
(Delta
###
, pa
###
, a
###
, b_modeling
###
)
{
  ##details<<
  ##\deqn{pc = (pa * (Delta - a)) / (b_modeling - a)}
  pc <- (pa * (Delta - a)) / (b_modeling - a);

  return( pc );
  ### pc
}

