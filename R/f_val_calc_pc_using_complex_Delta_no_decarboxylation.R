f_val_calc_pc_using_complex_Delta_no_decarboxylation <-
function# pc using complex D, no decarboxylation  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-Dpa]/(bs+al-b)  is this different from two up? They give different values but both may not be derived properly
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
)
{
  ##details<<
  ##\deqn{pc = (a.b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa) / (bs + al - b)}
  pc <- (a.b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa) / (bs + al - b);

  return( pc );
  ### pc
}

