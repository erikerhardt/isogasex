plot_not_na <-
function# if no data to plot, plot a dummy box (for when certain Licor columns are not collected)
###
(x.time
###
, y.var
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

  if (sum(is.na(y.var)) < length(x.time))
    { plot(x.time, y.var, pch=pch, type=type, cex=cex, xlab=xlab, ylab=ylab, main=main); plot_not_na.val <- 1; }
  else
    { plot(0, 0, pch="x", ylab="no data", main=main); plot_not_na.val <- 0; };

  return( plot_not_na.val );
  ### plot_not_na.val
}

