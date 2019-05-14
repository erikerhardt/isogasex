plot_A_photosynthesis <-
function# Plot A photosynthesis
###
(TDL.A.photosynthesis
###
, TDL.12A.photosynthesis
###
, TDL.13A.photosynthesis
###
, Licor.A.photosynthesis
###
, Delta.from.ratios.in.out
###
, Delta.from.A.ratio
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
    plot.filename <- "plot_A_photosynthesis";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, Licor.A.photosynthesis  , pch=20, type="l", ylab="", main="A, Licor photosynthesis");
    plot_not_na(x.time, TDL.A.photosynthesis    , pch=20, type="l", ylab="", main="A, TDL photosynthesis");
    plot_not_na(x.time, TDL.12A.photosynthesis  , pch=20, type="l", ylab="", main="12A, TDL photosynthesis");
    plot_not_na(x.time, TDL.13A.photosynthesis  , pch=20, type="l", ylab="", main="13A, TDL photosynthesis");
    plot_not_na(x.time, Delta.from.ratios.in.out, pch=20, type="l", ylab="", main="Delta from ratios in and out?");
    plot_not_na(x.time, Delta.from.A.ratio      , pch=20, type="l", ylab="", main="D from A ratio, should be the same as Delta.obs");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

