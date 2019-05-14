f_val_calc_Delta_i_simple_for_modeling <-
function# Delta.i simple for modeling  a + (b-a) pi/pa predicted discrimination including boundary layer effects but using b adjustments to approximate effects of gm and decarboxylations
###
(a
###
, b
###
, pa
###
, pi
###
)
{
  ##details<<
  ##\deqn{Delta.i.simple.for.modeling = a + (b - a) * pi / pa}
  Delta.i.simple.for.modeling <- a + (b - a) * pi / pa;

  return( Delta.i.simple.for.modeling );
  ### Delta.i.simple.for.modeling
}

