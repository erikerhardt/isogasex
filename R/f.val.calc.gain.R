f.val.calc.gain <-
function# gain (12C Gain, 13C Gain)
###
(true.value.hi.12C
###
, true.value.hi.13C
###
, true.value.lo.12C
###
, true.value.lo.13C
###
, cal.tank.hi.12C
###
, cal.tank.hi.13C
###
, cal.tank.lo.12C
###
, cal.tank.lo.13C
###
)
{
  gain <- as.list(new.env());  # create a list to return with data

  ##details<<
  ##\deqn{gain = (true.value.hi - true.value.lo) / (cal.tank.hi - cal.tank.lo)}
  gain$gain.12C <- (true.value.hi.12C - true.value.lo.12C) / (cal.tank.hi.12C - cal.tank.lo.12C);
  gain$gain.13C <- (true.value.hi.13C - true.value.lo.13C) / (cal.tank.hi.13C - cal.tank.lo.13C);

  return( gain );
  ### gain for 12C and 13C
}

