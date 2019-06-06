#' gm point complex, internal leaf (mesophyll) conductance calculated for every D value estimating decarboxylation effects
#'
#' \deqn{gm_point_complex = ((b - bs - al) * A / pa) / (Di - D_obs - (f_photo * Gamma_star / pa) - ((e * Rd) / (k * pa)))}
#'
#' @param b xxxPARAMxxx
#' @param bs xxxPARAMxxx
#' @param al xxxPARAMxxx
#' @param A xxxPARAMxxx
#' @param pa xxxPARAMxxx
#' @param Di xxxPARAMxxx
#' @param D_obs xxxPARAMxxx
#' @param f_photo xxxPARAMxxx
#' @param Gamma_star xxxPARAMxxx
#' @param e xxxPARAMxxx
#' @param Rd xxxPARAMxxx
#' @param k xxxPARAMxxx
#'
#' @return gm_point_complex xxxRETURNxxx
#'
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

