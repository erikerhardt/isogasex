f_val_calc_gm_point_simple <-
function# gm point simple, internal leaf (mesophyll) conductance calculated for every D value ignoring decarboxylation effects
###
(b
###
, bs
###
, al
###
, A
###
, pa
###
, Di
###
, D.obs
###
)
{
  ##details<<
  ##\deqn{gm.point.simple = (( b - bs - al) * A / pa) / (Di - D.obs)}
  gm.point.simple <- (( b - bs - al) * A / pa) / (Di - D.obs);

  return( gm.point.simple );
  ### gm.point.simple
}

