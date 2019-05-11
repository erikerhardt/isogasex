f.val.bs.CI.endpoints <-
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

