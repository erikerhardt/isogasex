#' Title
#'
#' @param a
#' @param b
#' @param a_b
#' @param pa
#' @param ps
#' @param pi
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_Delta_i_simple_for_gm <-
function# Delta_i simple for gm, predicted discrimination including boundary layer effects but not decarboxylation effects
###
(a
###
, b
###
, a_b
###
, pa
###
, ps
###
, pi
###
)
{
  ##details<<
  ##\deqn{Delta_i_simple_for_gm = a_b * ((pa - ps) / pa) + a*((ps - pi) / pa) + b * (pi / pa)}
  Delta_i_simple_for_gm <- a_b * ((pa - ps) / pa) + a*((ps - pi) / pa) + b * (pi / pa);

  return( Delta_i_simple_for_gm );
  ### Delta_i_simple_for_gm
}

