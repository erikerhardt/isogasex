#' Title
#'
#' @param x
#' @param n
#' @param ind.CI
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
, ind.CI
###
)
{
  ##details<<
  ## Sorts BS samples, takes quantiles for equal-tailed sig.CI Level CIs.

  x.CI <- matrix(0,nrow=n,ncol=2);
  for (i.n in 1:n) {
    x.CI[i.n,] <- sort(x[i.n,])[ind.CI];
  }

  return(x.CI);
  ### x.CI
}

