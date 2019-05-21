#' pc using complex D, full model  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-(eRd/k+fG*)-Dpa]/(bs+al-b)  includes boundary layer and decarboxylation effects
#'
#' \deqn{pc = (a_b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa  - (e * Rd / k + f * Gamma_star)) / (bs + al - b)}
#'
#' @param Delta
#' @param pa
#' @param ps
#' @param pi
#' @param a
#' @param a_b
#' @param al
#' @param b
#' @param bs
#' @param e
#' @param Rd
#' @param k
#' @param f
#' @param Gamma_star
#'
#' @return pc
#'
#' @examples
f_val_calc_pc_using_complex_Delta_full_model <-
function# pc using complex D, full model  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-(eRd/k+fG*)-Dpa]/(bs+al-b)  includes boundary layer and decarboxylation effects
###
(Delta
###
, pa
###
, ps
###
, pi
###
, a
###
, a_b
###
, al
###
, b
###
, bs
###
, e
###
, Rd
###
, k
###
, f
###
, Gamma_star
###
)
{
  ##details<<
  ##\deqn{pc = (a_b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa  - (e * Rd / k + f * Gamma_star)) / (bs + al - b)}
  pc <- (a_b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa  - (e * Rd / k + f * Gamma_star)) / (bs + al - b);

  return( pc );
  ### pc
}

