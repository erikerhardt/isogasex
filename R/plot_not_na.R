#' if no data to plot, plot a dummy box (for when certain Licor columns are not collected)
#'
#' If no data to plot, returns a "no data" (0,0) point as plot place holder.
#'
#' @param x_time xxxPARAMxxx
#' @param y_var xxxPARAMxxx
#' @param pch xxxPARAMxxx
#' @param type xxxPARAMxxx
#' @param cex xxxPARAMxxx
#' @param xlab xxxPARAMxxx
#' @param ylab xxxPARAMxxx
#' @param main xxxPARAMxxx
#'
#' @return plot_not_na_val xxxRETURNxxx
#'
plot_not_na <-
function# if no data to plot, plot a dummy box (for when certain Licor columns are not collected)
###
(x_time
###
, y_var
###
, pch=20
###
, type
###
, cex=.1
###
, xlab=""
###
, ylab
###
, main
###
)
{
  ##details<<
  ## If no data to plot, returns a "no data" (0,0) point as plot place holder.

  if (sum(is.na(y_var)) < length(x_time))
    { plot(x_time, y_var, pch=pch, type=type, cex=cex, xlab=xlab, ylab=ylab, main=main); plot_not_na_val <- 1; }
  else
    { plot(0, 0, pch="x", ylab="no data", main=main); plot_not_na_val <- 0; };

  return( plot_not_na_val );
  ### plot_not_na_val
}

