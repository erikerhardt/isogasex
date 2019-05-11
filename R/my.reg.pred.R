my.reg.pred <-
function# basic SLR y pred
###
(beta.hat
###
, x
###
)
{
  ##details<<
  ## Calculate predictions given the intercept and slope of a simple linear regression fit.

  # DEBUG
  # beta.hat = my.reg.fit(temp.y, temp.x)
  # x        = line.x

  n <- length(x);

  # 4/27/2013 9:36PM For some reason, once we had all NaNs for a cycle in plot.data.cycles.R
  #   This returned a beta.hat = NA, so return NAs of the right dimension.
  #   I don't know why this happened, the data looks ok.
  if (any(is.na(beta.hat))) {
    warning("Unable to predict regression line, beta.hat=NA (my.reg.pred.R)")
    return(matrix(NA, nrow = n, ncol = 1))
  }

  x1 <- cbind(matrix(rep(1,n),ncol=1), matrix(x,ncol=1));
  y.hat <- x1 %*% beta.hat;

  return( y.hat );
  ### y.hat
}

