#' Title
#'
#' @param chamber.Totalpc.using.gm
#' @param chamber.12pc.using.gm
#' @param chamber.13pc.using.gm
#' @param x.time
#' @param plot.format.list
#' @param output.fn.prefix
#'
#' @return
#' @export
#'
#' @examples
plot_pc_total <-
function# Plot pc, total partial pressure of CO2 at the site of carboxylation
###
(chamber.Totalpc.using.gm
###
, chamber.12pc.using.gm
###
, chamber.13pc.using.gm
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
    plot.filename <- "plot_pc_total_partial_pressure_CO2";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, chamber.Totalpc.using.gm, pch=20, type="l", ylab="", main="Total pc, total partial pressure of CO2 at the site of carboxylation");
    plot_not_na(x.time, chamber.12pc.using.gm   , pch=20, type="l", ylab="", main="12 pc, total partial pressure of CO2 at the site of carboxylation");
    plot_not_na(x.time, chamber.13pc.using.gm   , pch=20, type="l", ylab="", main="13 pc, total partial pressure of CO2 at the site of carboxylation");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

