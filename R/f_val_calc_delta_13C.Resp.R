f_val_calc_delta_13C_Resp <-
function# delta13C Respired, isotopic composition of respired CO2
###
(reference.delta.e
###
, chamber.delta.o
###
, p
###
)
{
  ##details<<
  ##\deqn{delta.13C.Resp = (chamber.delta.o - reference.delta.e * (1 - p)) / p}
  delta.13C.Resp <- (chamber.delta.o - reference.delta.e * (1 - p)) / p;

  return( delta.13C.Resp );
  ### delta.13C.Resp
}

