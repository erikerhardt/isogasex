#' delta13C Respired, isotopic composition of respired CO2
#'
#' \deqn{delta_13C_Resp = (chamber_delta_o - reference_delta_e * (1 - p)) / p}
#'
#' @param reference_delta_e
#' @param chamber_delta_o
#' @param p
#'
#' @return delta_13C_Resp
#'
#' @examples
f_val_calc_delta_13C_Resp <-
function# delta13C Respired, isotopic composition of respired CO2
###
(reference_delta_e
###
, chamber_delta_o
###
, p
###
)
{
  ##details<<
  ##\deqn{delta_13C_Resp = (chamber_delta_o - reference_delta_e * (1 - p)) / p}
  delta_13C_Resp <- (chamber_delta_o - reference_delta_e * (1 - p)) / p;

  return( delta_13C_Resp );
  ### delta_13C_Resp
}

