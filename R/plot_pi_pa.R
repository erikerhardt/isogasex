#' Title
#'
#' @param chamber.Totalpi
#' @param chamber.Totalpa
#' @param chamber.Totalpi_pa
#' @param x.time
#' @param plot.format.list
#' @param output.fn.prefix
#'
#' @return
#' @export
#'
#' @examples
plot_pi_pa <-
function# Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions
###
(chamber.Totalpi
###
, chamber.Totalpa
###
, chamber.Totalpi_pa
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
    plot.filename <- "plot_pi_pa_ratio";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, chamber.Totalpi   , pch=20, type="l", ylab="", main="Total pi, partial pressure of CO2 in the substomatal cavities");
    plot_not_na(x.time, chamber.Totalpa   , pch=20, type="l", ylab="", main="Total pa, partial pressure of CO2 above the leaf");
    plot_not_na(x.time, chamber.Totalpi_pa, pch=20, type="l", ylab="", main="Total pi/pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

