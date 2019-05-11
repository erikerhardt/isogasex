f.val.calc.pc.using.simple.Delta.for.gm <-
function# pc using simple D for gm, includes boundary layer
###
(Delta
###
, pa
###
, ps
###
, a
###
, b.gm
###
, a.b
###
)
{
  ##details<<
  ##\deqn{pc = (Delta * pa - a.b * (pa - ps) - a * ps) / (b.gm - a)}
  pc <- (Delta * pa - a.b * (pa - ps) - a * ps) / (b.gm - a);

  return( pc );
  ### pc
}

