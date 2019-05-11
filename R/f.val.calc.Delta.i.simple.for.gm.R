f.val.calc.Delta.i.simple.for.gm <-
function# Delta.i simple for gm, predicted discrimination including boundary layer effects but not decarboxylation effects
###
(a
###
, b
###
, a.b
###
, pa
###
, ps
###
, pi
###
)
{
  ##details<<
  ##\deqn{Delta.i.simple.for.gm = a.b * ((pa - ps) / pa) + a*((ps - pi) / pa) + b * (pi / pa)}
  Delta.i.simple.for.gm <- a.b * ((pa - ps) / pa) + a*((ps - pi) / pa) + b * (pi / pa);

  return( Delta.i.simple.for.gm );
  ### Delta.i.simple.for.gm
}

