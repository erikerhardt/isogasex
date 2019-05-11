my.reg.fit <-
function# basic SLR beta fit
###
(y
###
, x
###
)
{
  ##details<<
  ## Calculate intercept and slope of a simple linear regression fit.

  n <- length(x);
  y1 <- matrix(y,ncol=1);
  x1 <- cbind(matrix(rep(1,n),ncol=1), matrix(x-mean(x),ncol=1)); # centered x
  #x1 <- cbind(matrix(rep(1,n),ncol=1), matrix(x,ncol=1));
  if (qr(x1)$rank == 2) { # calculate regression line if the matrix is full rank  "0.1-16" "2012-07-10"
    beta.hat <- solve( t(x1) %*% x1 ) %*% t(x1) %*% y1; # slope correct, intercept from centered x
  } else { # is only not full rank if a single observation
    beta.hat <- c(y1,0)
  }
  y2 <- beta.hat[1] + beta.hat[2]*mean(x);
  beta.hat[1] <- beta.hat[1] - (y2-beta.hat[1]); # intercept corrected

  return( beta.hat );
  ### beta.hat
}

