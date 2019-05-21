#' Calc amount of time since program started
#'
#' Calculate time elapsed.
#'
#' @param time_start
#'
#' @return time_sofar
#'
#' @examples
progress_time <-
function# Calc amount of time since program started
###
(time_start
###
)
{
  ##details<<
  ## Calculate time elapsed.

  time_sofar <- proc.time()[3] - time_start;   # calculate time elapsed

  return( floor(time_sofar) );
  ### floor(time_sofar)
}

