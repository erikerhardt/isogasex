f_par_bs_mean <-
function# calculate Par BS mean
###
(x
###
, x.var
###
, n.sam
###
, n
###
)
{
  ##details<<
  ## Sample from normal distribution with mean and se from the data, calculating mean of n parametric bootstrap resamples.
  x.mean <- rep(0,n);
  for (i.bs in 1:n) {
    x.mean[i.bs] <- rnorm(1, mean=x[i.bs], sd=sqrt(x.var/n.sam[i.bs]) );
  };

  return( x.mean );
  ### x.mean
}

