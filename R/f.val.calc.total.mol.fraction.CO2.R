f.val.calc.total.mol.fraction.CO2 <-
function# Total mol fraction CO2
###
(obs.12C
###
, obs.13C
###
, fo.13C
###
)
{
  ##details<<
  ##\deqn{total.mol.fraction.CO2 = (obs.12C + obs.13C) / (1 - fo.13C)}
  total.mol.fraction.CO2 <- (obs.12C + obs.13C) / (1 - fo.13C);

  return( total.mol.fraction.CO2 );
  ### total.mol.fraction.CO2
}

