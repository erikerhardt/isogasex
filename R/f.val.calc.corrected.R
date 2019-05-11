f.val.calc.corrected <-
function# corrected (Corrected 12C, Corrected 13C)
###
(obs.12C
###
, obs.13C
###
, gain.12C
###
, gain.13C
###
, offset.12C
###
, offset.13C
###
)
{
  corrected <- as.list(new.env());  # create a list to return with data

  ##details<<
  ##\deqn{corrected = (obs * gain) + offset;}
  corrected$corrected.12C <- (obs.12C * gain.12C) + offset.12C;
  corrected$corrected.13C <- (obs.13C * gain.13C) + offset.13C;

  return( corrected );
  ### corrected 12C and 13C
}

