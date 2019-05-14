progress_time <-
function# Calc amount of time since program started
###
(time.start
###
)
{
  ##details<<
  ## Calculate time elapsed.

  time.sofar <- proc.time()[3] - time.start;   # calculate time elapsed

  return( floor(time.sofar) );
  ### floor(time.sofar)
}

