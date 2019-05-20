#' delta13C Assimilated, isotopic composition of assimilated sugars
#'
#' d13CA (do-Dobs)/(Dobs+1)  isotopic composition of assimilated sugars
#' (Chris did this differently for d13Cs in his paper see following:
#' I think this could be calculated for modelling using the equivalent of de instead of do
#' i.e. when the leaf modelled is not in a chamber so the air above a leaf is equivalent to the bulk atmosphere)
#'
#' \deqn{delta_13C_Assim = (chamber_delta_o - Delta_obs) / (Delta_obs + 1)}
#'
#' @param chamber_delta_o
#' @param Delta_obs
#'
#' @return delta_13C_Assim
#' @export
#'
#' @examples
f_val_calc_delta_13C_Assim <-
function# delta13C Assimilated, isotopic composition of assimilated sugars
### d13CA (do-Dobs)/(Dobs+1)  isotopic composition of assimilated sugars
### (Chris did this differently for d13Cs in his paper see following:
###     I think this could be calculated for modelling using the equivalent of de instead of do
###     i.e. when the leaf modelled is not in a chamber so the air above a leaf is equivalent to the bulk atmosphere)
(chamber_delta_o
###
, Delta_obs
###
)
{
  ##details<<
  ##\deqn{delta_13C_Assim = (chamber_delta_o - Delta_obs) / (Delta_obs + 1)}
  delta_13C_Assim <- (chamber_delta_o - Delta_obs) / (Delta_obs + 1);

  return( delta_13C_Assim );
  ### delta_13C_Assim
}

