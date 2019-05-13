f_val_calc_pc_using_complex_Delta_full_model <-
function# pc using complex D, full model  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-(eRd/k+fG*)-Dpa]/(bs+al-b)  includes boundary layer and decarboxylation effects
###
(Delta
###
, pa
###
, ps
###
, pi
###
, a
###
, a.b
###
, al
###
, b
###
, bs
###
, e
###
, Rd
###
, k
###
, f
###
, Gamma.star
###
)
{
  ##details<<
  ##\deqn{pc = (a.b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa  - (e * Rd / k + f * Gamma.star)) / (bs + al - b)}
  pc <- (a.b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa  - (e * Rd / k + f * Gamma.star)) / (bs + al - b);

  return( pc );
  ### pc
}

