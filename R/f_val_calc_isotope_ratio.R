f_val_calc_isotope_ratio <-
function# Isotope ratio
###
(obs.12C
###
, obs.13C
###
)
{
  ##details<<
  ##\deqn{isotope.ratio = obs.13C / obs.12C}
  isotope.ratio <- obs.13C / obs.12C;

  return( isotope.ratio );
  ### isotope.ratio
}

