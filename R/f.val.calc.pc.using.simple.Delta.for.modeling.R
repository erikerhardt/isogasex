f.val.calc.pc.using.simple.Delta.for.modeling <-
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

