#' pc using complex D, no decarboxylation  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-Dpa]/(bs+al-b)  is this different from two up? They give different values but both may not be derived properly
#'
#' \deqn{pc = (a_b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa) / (bs + al - b)}
#'
#' @param Delta xxxPARAMxxx
#' @param pa xxxPARAMxxx
#' @param ps xxxPARAMxxx
#' @param pi xxxPARAMxxx
#' @param a xxxPARAMxxx
#' @param a_b xxxPARAMxxx
#' @param al xxxPARAMxxx
#' @param b xxxPARAMxxx
#' @param bs xxxPARAMxxx
#'
#' @return pc xxxRETURNxxx
#'
f_val_calc_pc_using_complex_Delta_no_decarboxylation <-
function# pc using complex D, no decarboxylation  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-Dpa]/(bs+al-b)  is this different from two up? They give different values but both may not be derived properly
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
)
{
  ##details<<
  ##\deqn{pc = (a_b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa) / (bs + al - b)}
  pc <- (a_b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa) / (bs + al - b);

  return( pc );
  ### pc
}

