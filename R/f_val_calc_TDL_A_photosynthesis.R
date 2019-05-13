f_val_calc_TDL_A_photosynthesis <-
function# A, TDL photosynthesis
###
(flow
###
, Ce
###
, Co
###
, leaf.area
###
)
{
  ##details<<
  ##\deqn{A = (flow / 10^6) * (Ce - Co) / (leaf.area / 10^4)}
  A <- (flow / 10^6) * (Ce - Co) / (leaf.area / 10^4);

  return( A );
  ### A
}

