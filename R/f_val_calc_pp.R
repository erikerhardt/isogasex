#' pa, partial pressure of CO2 above the leaf, Press is the atmospheric pressure value from the LI6400
#'
#' \deqn{pa = (Co / 10^6) * (Press * 1000)}
#'
#' @param Co xxxPARAMxxx
#' @param Press xxxPARAMxxx
#'
#' @return pa xxxRETURNxxx
#'
f_val_calc_pp <-
function# pa, partial pressure of CO2 above the leaf, Press is the atmospheric pressure value from the LI6400
###
(Co
###
, Press
###
)
{
  ##details<<
  ##\deqn{pa = (Co / 10^6) * (Press * 1000)}
  pa <- (Co / 10^6) * (Press * 1000);

  return( pa );
  ### pa
}

