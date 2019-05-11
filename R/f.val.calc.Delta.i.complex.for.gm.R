f.val.calc.Delta.i.complex.for.gm <-
function# Delta.i complex for gm  CHECK DERIVATION  predicted discrimination including boundary layer effects AND decarboxylation effects
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
, b.gm
###
, f.photo
###
, Gamma.star
###
, e
###
, Rd
###
, k
###
)
{
  ##details<<
  ##\deqn{Delta.i.complex.for.gm = a.b * ((pa - ps) / pa) + a * ((ps - pi) / pa) + b * (pi / pa) - (f.photo * Gamma.star / pa) - (e * Rd / (k * pa))}
  Delta.i.complex.for.gm <-
    a.b * ((pa - ps) / pa) + a * ((ps - pi) / pa) + b * (pi / pa) - (f.photo * Gamma.star / pa) - (e * Rd / (k * pa));

  return( Delta.i.complex.for.gm );
  ### Delta.i.complex.for.gm
}

