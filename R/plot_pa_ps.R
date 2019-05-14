#' Title
#'
#' @param chamber.Totalpa
#' @param chamber.12pa
#' @param chamber.13pa
#' @param chamber.Totalps
#' @param chamber.12ps
#' @param chamber.13ps
#' @param x.time
#' @param plot.format.list
#' @param output.fn.prefix
#'
#' @return
#' @export
#'
#' @examples
plot_pa_ps <-
function# Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface
###
(chamber.Totalpa
###
, chamber.12pa
###
, chamber.13pa
###
, chamber.Totalps
###
, chamber.12ps
###
, chamber.13ps
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
    plot.filename <- "plot_pa_ps_partial_pressure_of_CO2";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, chamber.Totalpa, pch=20, type="l", ylab="", main="Total pa, partial pressure of CO2 above the leaf");
    plot_not_na(x.time, chamber.Totalps, pch=20, type="l", ylab="", main="Total ps, partial pressure of CO2 at the leaf surface");
    plot_not_na(x.time, chamber.12pa   , pch=20, type="l", ylab="", main="12 pa, partial pressure of CO2 above the leaf");
    plot_not_na(x.time, chamber.12ps   , pch=20, type="l", ylab="", main="12 ps, partial pressure of CO2 at the leaf surface");
    plot_not_na(x.time, chamber.13pa   , pch=20, type="l", ylab="", main="13 pa, partial pressure of CO2 above the leaf");
    plot_not_na(x.time, chamber.13ps   , pch=20, type="l", ylab="", main="13 ps, partial pressure of CO2 at the leaf surface");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

