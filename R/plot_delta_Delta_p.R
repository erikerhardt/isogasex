plot_delta_Delta_p <-
function# Plot delta, Delta, and p
###
(reference.delta.e
###
, chamber.delta.o
###
, chamber.reference.delta.diff.CoCe
###
, Delta.obs
###
, p
###
, delta.13C.Assim
###
, delta.13C.Resp
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
    plot.filename <- "plot_delta_Delta_p";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(4,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, reference.delta.e                , pch=20, type="l", ylab="", main="reference delta.e");
    plot_not_na(x.time, chamber.delta.o                  , pch=20, type="l", ylab="", main="chamber delta.o");
    plot_not_na(x.time, chamber.reference.delta.diff.CoCe, pch=20, type="l", ylab="", main="delta chamber-reference difference");
    plot_not_na(x.time, Delta.obs                        , pch=20, type="l", ylab="", main="Delta obs");
    plot_not_na(x.time, p                                , pch=20, type="l", ylab="", main="p");
    plot_not_na(x.time, x.time                           , pch=20, type="l", ylab="", main="(blank)");
    plot_not_na(x.time, delta.13C.Assim                  , pch=20, type="l", ylab="", main="delta Assim");
    plot_not_na(x.time, delta.13C.Resp                   , pch=20, type="l", ylab="", main="delta Resp");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

