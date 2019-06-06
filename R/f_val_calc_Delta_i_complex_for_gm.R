#' Delta_i complex for gm  CHECK DERIVATION  predicted discrimination including boundary layer effects AND decarboxylation effects
#'
#' \deqn{Delta_i_complex_for_gm = a_b * ((pa - ps) / pa) + a * ((ps - pi) / pa) + b * (pi / pa) - (f_photo * Gamma_star / pa) - (e * Rd / (k * pa))}
#'
#' @param a xxxPARAMxxx
#' @param b xxxPARAMxxx
#' @param a_b xxxPARAMxxx
#' @param pa xxxPARAMxxx
#' @param ps xxxPARAMxxx
#' @param pi xxxPARAMxxx
#' @param b_gm xxxPARAMxxx
#' @param f_photo xxxPARAMxxx
#' @param Gamma_star xxxPARAMxxx
#' @param e xxxPARAMxxx
#' @param Rd xxxPARAMxxx
#' @param k xxxPARAMxxx
#'
#' @return Delta_i_complex_for_gm xxxRETURNxxx
#'
f_val_calc_Delta_i_complex_for_gm <-
function# Delta_i complex for gm  CHECK DERIVATION  predicted discrimination including boundary layer effects AND decarboxylation effects
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
, b_gm
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
  ##\deqn{Delta_i_complex_for_gm = a_b * ((pa - ps) / pa) + a * ((ps - pi) / pa) + b * (pi / pa) - (f_photo * Gamma_star / pa) - (e * Rd / (k * pa))}
  Delta_i_complex_for_gm <-
    a_b * ((pa - ps) / pa) + a * ((ps - pi) / pa) + b * (pi / pa) - (f_photo * Gamma_star / pa) - (e * Rd / (k * pa));

  return( Delta_i_complex_for_gm );
  ### Delta_i_complex_for_gm
}

