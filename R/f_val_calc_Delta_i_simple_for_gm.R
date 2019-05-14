#' Title
#'
#' @param a
#' @param b
#' @param a.b
#' @param pa
#' @param ps
#' @param pi
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_Delta_i_simple_for_gm <-
function# Delta.i simple for gm, predicted discrimination including boundary layer effects but not decarboxylation effects
###
(a
###
, b
###
, a.b
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
  ##\deqn{Delta.i.simple.for.gm = a.b * ((pa - ps) / pa) + a*((ps - pi) / pa) + b * (pi / pa)}
  Delta.i.simple.for.gm <- a.b * ((pa - ps) / pa) + a*((ps - pi) / pa) + b * (pi / pa);

  return( Delta.i.simple.for.gm );
  ### Delta.i.simple.for.gm
}

