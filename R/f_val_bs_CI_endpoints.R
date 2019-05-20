#' Title
#'
#' @param x
#' @param n
#' @param ind_CI
#'
#' @return
#' @export
#'
#' @examples
f_val_bs_CI_endpoints <-
function# create CI endpoints
###
(x
###
, n
###
, ind_CI
###
)
{
  ##details<<
  ## Sorts BS samples, takes quantiles for equal-tailed sig_CI Level CIs.

  x_CI <- matrix(0,nrow=n,ncol=2);
  for (i_n in 1:n) {
    x_CI[i_n,] <- sort(x[i_n,])[ind_CI];
  }

  return(x_CI);
  ### x_CI
}

