plot.Delta.i <-
function# Plot Delta.i, predicted discrimination
###
(chamber.Delta.i.simple.for.gm
###
, chamber.Delta.i.simple.for.modeling
###
, chamber.Delta.i.complex.for.gm
###
, chamber.Delta.i.simple.for.gm_Delta.obs
###
, chamber.Delta.i.complex.for.gm_Delta.obs
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
    plot.filename <- "plot_Delta_i_predicted_discrimination";
    s.plot.settings.begin.end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(5,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot.not.na(x.time, chamber.Delta.i.simple.for.gm           , pch=20, type="l", ylab="", main="Delta.i simple for gm");
    plot.not.na(x.time, chamber.Delta.i.simple.for.modeling     , pch=20, type="l", ylab="", main="Delta.i simple for modeling");
    plot.not.na(x.time, chamber.Delta.i.complex.for.gm          , pch=20, type="l", ylab="", main="Delta.i complex for gm");
    plot.not.na(x.time, chamber.Delta.i.simple.for.gm_Delta.obs , pch=20, type="l", ylab="", main="difference of Delta.i simple for gm and Delta.obs from TDL");
    plot.not.na(x.time, chamber.Delta.i.complex.for.gm_Delta.obs, pch=20, type="l", ylab="", main="difference of Delta.i complex for gm and Delta.obs from TDL");

    #axis(3); axis(4); # add axis labels to top and right sides
    s.plot.settings.begin.end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

