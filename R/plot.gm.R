plot.gm <-
function# Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects
###
(chamber.Totalgm.point.simple
###
, chamber.12gm.point.simple
###
, chamber.13gm.point.simple
###
, chamber.Totalgm.point.complex
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
    plot.filename <- "plot_gm_internal_conductance";
    s.plot.settings.begin.end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(4,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot.not.na(x.time, chamber.Totalgm.point.simple , pch=20, type="l", ylab="", main="Total gm point simple, ignoring decarboxylation effects");
    plot.not.na(x.time, chamber.12gm.point.simple    , pch=20, type="l", ylab="", main="12 gm point simple, ignoring decarboxylation effects");
    plot.not.na(x.time, chamber.13gm.point.simple    , pch=20, type="l", ylab="", main="13 gm point simple, ignoring decarboxylation effects");
    plot.not.na(x.time, chamber.Totalgm.point.complex, pch=20, type="l", ylab="", main="Total gm point complex, estimating decarboxylation effects");

    #axis(3); axis(4); # add axis labels to top and right sides
    s.plot.settings.begin.end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

