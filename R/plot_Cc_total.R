#' Title
#'
#' @param chamber.TotalCc
#' @param chamber.12Cc
#' @param chamber.13Cc
#' @param x.time
#' @param plot.format.list
#' @param output.fn.prefix
#'
#' @return
#' @export
#'
#' @examples
plot_Cc_total <-
function# Plot Cc, ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol
###
(chamber.TotalCc
###
, chamber.12Cc
###
, chamber.13Cc
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
    plot.filename <- "plot_Cc_ppm_CO2_concentration";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, chamber.TotalCc, pch=20, type="l", ylab="", main="Total Cc, ppm CO2 concentration at the site of carboxylation");
    plot_not_na(x.time, chamber.12Cc   , pch=20, type="l", ylab="", main="12 Cc, ppm CO2 concentration at the site of carboxylation");
    plot_not_na(x.time, chamber.13Cc   , pch=20, type="l", ylab="", main="13 Cc, ppm CO2 concentration at the site of carboxylation");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

