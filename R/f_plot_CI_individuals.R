#' plot all variables with bs values, mean value, and CI intervals
#'
#' For every variable, plots the mean and bootstrap CI, after removing NAs with \code{\link{plot_not_na}}.
#'
#' @param x_bs
#' @param x_sum
#' @param x_CI
#' @param x_title
#' @param x_time
#' @param x_n
#' @param R_bootstrap
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return NULL
#' @export
#'
#' @examples
f_plot_CI_individuals <-
function# plot all variables with bs values, mean value, and CI intervals
###
(x_bs
###
, x_sum
###
, x_CI
###
, x_title
###
, x_time
###
, x_n
###
, R_bootstrap
###
, plot_format_list
###
, output_fn_prefix
###
)
{
  ##details<<
  ## For every variable, plots the mean and bootstrap CI, after removing NAs with \code{\link{plot_not_na}}.
  for (i_plot in plot_format_list)
  {
    x_title <- gsub("[ ]","",x_title);
    plot_filename <- paste("BS_CHECK_",gsub("[ $]","_",x_title), sep="");
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(1,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)

    bs_time <- matrix(x_time, nrow=x_n, ncol=R_bootstrap); # matrix of times to plot with matrix of BS samples
    bs_y     <- x_bs;
    bs_y_sum <- x_sum;
    bs_y_CI  <- x_CI;

    plot_not_na_val <- plot_not_na(bs_time, bs_y, pch=20, type="p", cex=.1, xlab="summary index", ylab="", main=x_title);
    if (plot_not_na_val) {
      points(bs_time[,1], bs_y_CI[,1] , type="l", col="green");
      points(bs_time[,1], bs_y_CI[,2] , type="l", col="cyan");
      points(bs_time[,1], bs_y_sum    , type="l", col="red"  );
    }

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  invisible(NULL);
  ### NULL
}

