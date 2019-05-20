#' gm point complex, internal leaf (mesophyll) conductance calculated for every D value estimating decarboxylation effects
#'
#' \deqn{gm_point_complex = ((b - bs - al) * A / pa) / (Di - D_obs - (f_photo * Gamma_star / pa) - ((e * Rd) / (k * pa)))}
#'
#' @param b
#' @param bs
#' @param al
#' @param A
#' @param pa
#' @param Di
#' @param D_obs
#' @param f_photo
#' @param Gamma_star
#' @param e
#' @param Rd
#' @param k
#'
#' @return gm_point_complex
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
, D_obs
###
, f_photo
###
, Gamma_star
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
  ##\deqn{gm_point_complex = ((b - bs - al) * A / pa) / (Di - D_obs - (f_photo * Gamma_star / pa) - ((e * Rd) / (k * pa)))}
  gm_point_complex <- ((b - bs - al) * A / pa) / (Di - D_obs - (f_photo * Gamma_star / pa) - ((e * Rd) / (k * pa)));

  return( gm_point_complex );
  ### gm_point_complex
}

