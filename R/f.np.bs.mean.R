f.np.bs.mean <-
function# calculate NP BS mean
###
(x
###
, first.ind
###
, last.ind
###
, n.sam
###
, n
###
)
{
  ##details<<
  ## Resample from data with replacement, calculating mean of n nonparametric bootstrap resamples.
  x.mean <- rep(0,n);
  for (i.bs in 1:n) {
    x.mean[i.bs] <- mean( sample(x=x[seq(first.ind[i.bs],last.ind[i.bs])], size=n.sam[i.bs], replace=TRUE), na.rm=TRUE);
  };

  return( x.mean );
  ### x.mean
}

