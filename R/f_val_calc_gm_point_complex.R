#' Title
#'
#' @param b
#' @param bs
#' @param al
#' @param A
#' @param pa
#' @param Di
#' @param D.obs
#' @param f.photo
#' @param Gamma.star
#' @param e
#' @param Rd
#' @param k
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_gm_point_complex <-
function# gm point complex, internal leaf (mesophyll) conductance calculated for every D value estimating decarboxylation effects
###
(b
###
, bs
###
, al
###
, A
###
, pa
###
, Di
###
, D.obs
###
, f.photo
###
, Gamma.star
###
, e
###
, Rd
###
, k
###
)
{
  ##details<<
  ##\deqn{gm.point.complex = ((b - bs - al) * A / pa) / (Di - D.obs - (f.photo * Gamma.star / pa) - ((e * Rd) / (k * pa)))}
  gm.point.complex <- ((b - bs - al) * A / pa) / (Di - D.obs - (f.photo * Gamma.star / pa) - ((e * Rd) / (k * pa)));

  return( gm.point.complex );
  ### gm.point.complex
}

