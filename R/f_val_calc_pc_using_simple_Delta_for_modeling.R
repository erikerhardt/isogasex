f_val_calc_pc_using_simple_Delta_for_modeling <-
function# pc using simple D for modeling
###
(Delta
###
, pa
###
, a
###
, b.modeling
###
)
{
  ##details<<
  ##\deqn{pc = (pa * (Delta - a)) / (b.modeling - a)}
  pc <- (pa * (Delta - a)) / (b.modeling - a);

  return( pc );
  ### pc
}

