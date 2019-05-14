#' Title
#'
#' @param x.bs
#' @param x.sum
#' @param x.CI
#' @param x.title
#' @param x.time
#' @param x.n
#' @param R.bootstrap
#' @param plot.format.list
#' @param output.fn.prefix
#'
#' @return
#' @export
#'
#' @examples
f_plot_CI_individuals <-
function# plot all variables with bs values, mean value, and CI intervals
###
(x.bs
###
, x.sum
###
, x.CI
###
, x.title
###
, x.time
###
, x.n
###
, R.bootstrap
###
, plot.format.list
###
, output.fn.prefix
###
)
{
  ##details<<
  ## For every variable, plots the mean and bootstrap CI, after removing NAs with \code{\link{plot_not_na}}.
  for (i.plot in plot.format.list)
  {
    x.title <- gsub("[ ]","",x.title);
    plot.filename <- paste("BS_CHECK_",gsub("[ $]","_",x.title), sep="");
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(1,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)

    bs.time <- matrix(x.time, nrow=x.n, ncol=R.bootstrap); # matrix of times to plot with matrix of BS samples
    bs.y     <- x.bs;
    bs.y.sum <- x.sum;
    bs.y.CI  <- x.CI;

    plot_not_na.val <- plot_not_na(bs.time, bs.y, pch=20, type="p", cex=.1, xlab="summary index", ylab="", main=x.title);
    if (plot_not_na.val) {
      points(bs.time[,1], bs.y.CI[,1] , type="l", col="green");
      points(bs.time[,1], bs.y.CI[,2] , type="l", col="cyan");
      points(bs.time[,1], bs.y.sum    , type="l", col="red"  );
    }

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

