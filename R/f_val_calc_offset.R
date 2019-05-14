#' Title
#'
#' @param true.value.hi.12C
#' @param true.value.hi.13C
#' @param gain.12C
#' @param gain.13C
#' @param cal.tank.hi.12C
#' @param cal.tank.hi.13C
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_offset <-
function# offset (12C Offset, 13C Offset) using hi tank
###
(true.value.hi.12C
###
, true.value.hi.13C
###
, gain.12C
###
, gain.13C
###
, cal.tank.hi.12C
###
, cal.tank.hi.13C
###
)
{
  offset <- as.list(new.env());  # create a list to return with data

  ##details<<
  ##\deqn{offset = true.value.hi - (gain * cal.tank.hi)}
  offset$offset.12C <- true.value.hi.12C - (gain.12C * cal.tank.hi.12C);
  offset$offset.13C <- true.value.hi.13C - (gain.13C * cal.tank.hi.13C);

  return( offset );
  ### offset for 12C and 13C
}

