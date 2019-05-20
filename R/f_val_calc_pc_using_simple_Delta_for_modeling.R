#' pc using simple D for modeling
#'
#' \deqn{pc = (pa * (Delta - a)) / (b_modeling - a)}
#'
#' @param Delta
#' @param pa
#' @param a
#' @param b_modeling
#'
#' @return pc
#' @export
#'
#' @examples
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

