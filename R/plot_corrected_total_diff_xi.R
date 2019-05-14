#' Title
#'
#' @param ref.12C
#' @param ref.13C
#' @param cham.12C
#' @param cham.13C
#' @param ref.tot
#' @param cham.tot
#' @param diff.tot
#' @param xi
#' @param x.time
#' @param plot.format.list
#' @param output.fn.prefix
#'
#' @return
#' @export
#'
#' @examples
plot_corrected_total_diff_xi <-
function# Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample
###
(ref.12C
###
, ref.13C
###
, cham.12C
###
, cham.13C
###
, ref.tot
###
, cham.tot
###
, diff.tot
###
, xi
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
    plot.filename <- "plot_corrected_total_diff";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(4,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, ref.12C , pch=20, type="l", ylab="ppm", main="reference 12CO2");
    plot_not_na(x.time, cham.12C, pch=20, type="l", ylab="ppm", main="chamber 12CO2");
    plot_not_na(x.time, ref.13C , pch=20, type="l", ylab="ppm", main="reference 13CO2");
    plot_not_na(x.time, cham.13C, pch=20, type="l", ylab="ppm", main="chamber 13CO2");
    plot_not_na(x.time, ref.tot , pch=20, type="l", ylab="ppm", main="reference total");
    plot_not_na(x.time, cham.tot, pch=20, type="l", ylab="ppm", main="chamber total");
    plot_not_na(x.time, diff.tot, pch=20, type="l", ylab="ppm", main="chamber-reference difference");
    plot_not_na(x.time, xi      , pch=20, type="l", ylab=""   , main="xi");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

