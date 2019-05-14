#' Title
#'
#' @param chamber.Totalgbc
#' @param chamber.13gbc
#' @param chamber.Totalgsc
#' @param chamber.13gsc
#' @param chamber.Totalgtc
#' @param chamber.13gtc
#' @param x.time
#' @param plot.format.list
#' @param output.fn.prefix
#'
#' @return
#' @export
#'
#' @examples
plot_gbc_gsc_gtc <-
function# Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2
###
(chamber.Totalgbc
###
, chamber.13gbc
###
, chamber.Totalgsc
###
, chamber.13gsc
###
, chamber.Totalgtc
###
, chamber.13gtc
###
, x.time
###
, plot.format.list
###
, output.fn.prefix
###
)
{
  ##details<<
  ## Plots the input variables.

  for (i.plot in plot.format.list)
  {
    plot.filename <- "plot_gbc_gsc_gtc_conductance_for_CO2";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, chamber.Totalgbc, pch=20, type="l", ylab="", main="Total and 12 gbc, boundary layer conductance for CO2");
    plot_not_na(x.time, chamber.13gbc   , pch=20, type="l", ylab="", main="13 gbc, boundary layer conductance for CO2");
    plot_not_na(x.time, chamber.Totalgsc, pch=20, type="l", ylab="", main="Total and 12 gsc, stomatal conductance for CO2");
    plot_not_na(x.time, chamber.13gsc   , pch=20, type="l", ylab="", main="13 gsc, stomatal conductance for CO2");
    plot_not_na(x.time, chamber.Totalgtc, pch=20, type="l", ylab="", main="Total and 12 gtc, total (stomatal and boundary layer) conductance for CO2");
    plot_not_na(x.time, chamber.13gtc   , pch=20, type="l", ylab="", main="13 gtc, total (stomatal and boundary layer) conductance for CO2");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

