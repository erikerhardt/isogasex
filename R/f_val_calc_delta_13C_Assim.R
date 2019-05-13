f_val_calc_delta_13C_Assim <-
function# delta13C Assimilated, isotopic composition of assimilated sugars
### d13CA (do-Dobs)/(Dobs+1)  isotopic composition of assimilated sugars
### (Chris did this differently for d13Cs in his paper see following:
###     I think this could be calculated for modelling using the equivalent of de instead of do
###     i.e. when the leaf modelled is not in a chamber so the air above a leaf is equivalent to the bulk atmosphere)
(chamber.delta.o
###
, Delta.obs
###
)
{
  ##details<<
  ##\deqn{delta.13C.Assim = (chamber.delta.o - Delta.obs) / (Delta.obs + 1)}
  delta.13C.Assim <- (chamber.delta.o - Delta.obs) / (Delta.obs + 1);

  return( delta.13C.Assim );
  ### delta.13C.Assim
}

