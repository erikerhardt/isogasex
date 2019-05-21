#' Delta_i simple for modeling  a + (b-a) pi/pa predicted discrimination including boundary layer effects but using b adjustments to approximate effects of gm and decarboxylations
#'
#' \deqn{Delta_i_simple_for_modeling = a + (b - a) * pi / pa}
#'
#' @param a
#' @param b
#' @param pa
#' @param pi
#'
#' @return Delta_i_simple_for_modeling
#'
#' @examples
f_val_calc_Delta_i_simple_for_modeling <-
function# Delta_i simple for modeling  a + (b-a) pi/pa predicted discrimination including boundary layer effects but using b adjustments to approximate effects of gm and decarboxylations
###
(a
###
, b
###
, pa
###
, pi
###
)
{
  ##details<<
  ##\deqn{Delta_i_simple_for_modeling = a + (b - a) * pi / pa}
  Delta_i_simple_for_modeling <- a + (b - a) * pi / pa;

  return( Delta_i_simple_for_modeling );
  ### Delta_i_simple_for_modeling
}

