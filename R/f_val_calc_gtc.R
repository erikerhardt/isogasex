#' gtc, total (stomatal and boundary layer) conductance for CO2
#'
#' \deqn{gtc = 1 / ((1 / gbc) + (1 / gsc))}
#'
#' @param gbc
#' @param gsc
#'
#' @return gtc
#' @export
#'
#' @examples
f_val_calc_gtc <-
function# gtc, total (stomatal and boundary layer) conductance for CO2
###
(gbc
###
, gsc
###
)
{
  ##details<<
  ##\deqn{gtc = 1 / ((1 / gbc) + (1 / gsc))}
  gtc <- 1 / ((1 / gbc) + (1 / gsc));

  return( gtc );
  ### gtc
}

