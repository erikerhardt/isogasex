#' calculate Par BS mean
#'
#' Sample from normal distribution with mean and se from the data, calculating mean of n parametric bootstrap resamples.
#'
#' @param x xxxPARAMxxx
#' @param x_var xxxPARAMxxx
#' @param n_sam xxxPARAMxxx
#' @param n xxxPARAMxxx
#'
#' @return x_mean xxxRETURNxxx
#' @importFrom stats rnorm
#'
f_par_bs_mean <-
function# calculate Par BS mean
###
(x
###
, x_var
###
, n_sam
###
, n
###
)
{
  ##details<<
  ## Sample from normal distribution with mean and se from the data, calculating mean of n parametric bootstrap resamples.
  x_mean <- rep(0,n);
  for (i_bs in 1:n) {
    x_mean[i_bs] <- stats::rnorm(1, mean=x[i_bs], sd=sqrt(x_var/n_sam[i_bs]) );
  };

  return( x_mean );
  ### x_mean
}

