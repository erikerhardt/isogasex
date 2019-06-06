#' pc using gm,  total partial pressure of CO2 at the site of carboxylation, Press is the atmospheric pressure value from the LI6400
#'
#' \deqn{pc = pi - (A / gm)}
#'
#' @param pi xxxPARAMxxx
#' @param A xxxPARAMxxx
#' @param gm xxxPARAMxxx
#'
#' @return pc xxxRETURNxxx
#'
f_val_calc_pc_using_gm <-
function# pc using gm,  total partial pressure of CO2 at the site of carboxylation, Press is the atmospheric pressure value from the LI6400
###
(pi
###
, A
###
, gm
###
)
{
  ##details<<
  ##\deqn{pc = pi - (A / gm)}
  pc <- pi - (A / gm);

  return( pc );
  ### pc
}

