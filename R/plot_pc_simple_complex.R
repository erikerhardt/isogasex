#' Title
#'
#' @param chamber.Totalpc.using.simple.Delta.for.gm
#' @param chamber.Totalpc.using.simple.Delta.for.modeling
#' @param chamber.Totalpc.using.complex.Delta.no.decarboxylation
#' @param chamber.Totalpc.using.complex.Delta.full.model
#' @param x.time
#' @param plot.format.list
#' @param output.fn.prefix
#'
#' @return
#' @export
#'
#' @examples
plot_pc_simple_complex <-
function# Plot pc, simple and complex
###
(chamber.Totalpc.using.simple.Delta.for.gm
###
, chamber.Totalpc.using.simple.Delta.for.modeling
###
, chamber.Totalpc.using.complex.Delta.no.decarboxylation
###
, chamber.Totalpc.using.complex.Delta.full.model
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
    plot.filename <- "plot_pc_simple_complex";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(4,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, chamber.Totalpc.using.simple.Delta.for.gm             , pch=20, type="l", ylab="", main="Total pc using simple D for gm, includes boundary layer");
    plot_not_na(x.time, chamber.Totalpc.using.simple.Delta.for.modeling       , pch=20, type="l", ylab="", main="Total pc using simple D for modeling");
    plot_not_na(x.time, chamber.Totalpc.using.complex.Delta.no.decarboxylation, pch=20, type="l", ylab="", main="Total pc using complex D, no decarboxylation");
    plot_not_na(x.time, chamber.Totalpc.using.complex.Delta.full.model        , pch=20, type="l", ylab="", main="Total pc using complex D, full model");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

