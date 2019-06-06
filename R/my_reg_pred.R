#' basic SLR y pred
#'
#' Calculate predictions given the intercept and slope of a simple linear regression fit.
#'
#' @param beta_hat xxxPARAMxxx
#' @param x xxxPARAMxxx
#'
#' @return y_hat xxxRETURNxxx
#'
my_reg_pred <-
function# basic SLR y pred
###
(beta_hat
###
, x
###
)
{
  ##details<<
  ## Calculate predictions given the intercept and slope of a simple linear regression fit.

  # DEBUG
  # beta_hat = my_reg_fit(temp_y, temp_x)
  # x        = line_x

  n <- length(x);

  # 4/27/2013 9:36PM For some reason, once we had all NaNs for a cycle in plot_data_cycles.R
  #   This returned a beta_hat = NA, so return NAs of the right dimension.
  #   I don't know why this happened, the data looks ok.
  if (any(is.na(beta_hat))) {
    warning("Unable to predict regression line, beta_hat=NA (my_reg_pred.R)")
    return(matrix(NA, nrow = n, ncol = 1))
  }

  x1 <- cbind(matrix(rep(1,n),ncol=1), matrix(x,ncol=1));
  y_hat <- x1 %*% beta_hat;

  return( y_hat );
  ### y_hat
}

