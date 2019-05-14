#' Title
#'
#' @param reference.delta.e
#' @param chamber.delta.o
#' @param xi
#'
#' @return
#' @export
#'
#' @examples
f_val_calc_Delta_obs <-
function# Delta, observed discrimination
###
(reference.delta.e
###
, chamber.delta.o
###
, xi
###
)
{
  ##details<<
  ##\deqn{temp.xidode = xi * (chamber.delta.o - reference.delta.e)}
  ##\deqn{Delta.obs = temp.xidode / (1 + chamber.delta.o - temp.xidode)}
  temp.xidode <- xi * (chamber.delta.o - reference.delta.e);
  Delta.obs <- temp.xidode / (1 + chamber.delta.o - temp.xidode);

  return( Delta.obs );
  ### Delta.obs
}

