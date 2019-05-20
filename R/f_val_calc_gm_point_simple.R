#' gm point simple, internal leaf (mesophyll) conductance calculated for every D value ignoring decarboxylation effects
#'
#' \deqn{gm_point_simple = (( b - bs - al) * A / pa) / (Di - D_obs)}
#'
#' @param b
#' @param bs
#' @param al
#' @param A
#' @param pa
#' @param Di
#' @param D_obs
#'
#' @return gm_point_simple
#' @export
#'
#' @examples
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
, D_obs
###
)
{
  ##details<<
  ##\deqn{gm_point_simple = (( b - bs - al) * A / pa) / (Di - D_obs)}
  gm_point_simple <- (( b - bs - al) * A / pa) / (Di - D_obs);

  return( gm_point_simple );
  ### gm_point_simple
}

