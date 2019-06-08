#' corrected (Corrected 12C, Corrected 13C)
#'
#' \deqn{corrected = (obs * gain) + offset}
#'
#' @param obs_12C xxxPARAMxxx
#' @param obs_13C xxxPARAMxxx
#' @param gain_12C xxxPARAMxxx
#' @param gain_13C xxxPARAMxxx
#' @param offset_12C xxxPARAMxxx
#' @param offset_13C xxxPARAMxxx
#'
#' @return corrected 12C and 13C
#'
f_val_calc_corrected <-
function# corrected (Corrected 12C, Corrected 13C)
###
(obs_12C
###
, obs_13C
###
, gain_12C
###
, gain_13C
###
, offset_12C
###
, offset_13C
###
)
{
  corrected <- list();  # create a list to return with data

  ##details<<
  ##\deqn{corrected = (obs * gain) + offset;}
  corrected$corrected_12C <- (obs_12C * gain_12C) + offset_12C;
  corrected$corrected_13C <- (obs_13C * gain_13C) + offset_13C;

  return( corrected );
  ### corrected 12C and 13C
}

