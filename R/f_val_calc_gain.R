#' gain (12C Gain, 13C Gain)
#'
#' \deqn{gain = (true_value_hi - true.value.lo) / (cal.tank_hi - cal.tank.lo)}
#'
#' @param true_value_hi_12C xxxPARAMxxx
#' @param true_value_hi_13C xxxPARAMxxx
#' @param true_value_lo_12C xxxPARAMxxx
#' @param true_value_lo_13C xxxPARAMxxx
#' @param cal_tank_hi_12C xxxPARAMxxx
#' @param cal_tank_hi_13C xxxPARAMxxx
#' @param cal_tank_lo_12C xxxPARAMxxx
#' @param cal_tank_lo_13C xxxPARAMxxx
#'
#' @return gain for 12C and 13C xxxRETURNxxx
#'
f_val_calc_gain <-
function# gain (12C Gain, 13C Gain)
###
(true_value_hi_12C
###
, true_value_hi_13C
###
, true_value_lo_12C
###
, true_value_lo_13C
###
, cal_tank_hi_12C
###
, cal_tank_hi_13C
###
, cal_tank_lo_12C
###
, cal_tank_lo_13C
###
)
{
  gain <- as.list(new.env());  # create a list to return with data

  ##details<<
  ##\deqn{gain = (true_value_hi - true.value.lo) / (cal.tank_hi - cal.tank.lo)}
  gain$gain_12C <- (true_value_hi_12C - true_value_lo_12C) / (cal_tank_hi_12C - cal_tank_lo_12C);
  gain$gain_13C <- (true_value_hi_13C - true_value_lo_13C) / (cal_tank_hi_13C - cal_tank_lo_13C);

  return( gain );
  ### gain for 12C and 13C
}

