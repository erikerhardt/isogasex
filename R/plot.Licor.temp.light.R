plot.Licor.temp.light <-
function# Plot Licor Temp Light values
###
(VPD
###
, E.transpiration
###
, leaf.temp
###
, air.temp
###
, light.in
###
, light.out
###
, x.time
###
, plot.format.list
###
, output.fn.prefix
###
)
            #, flow.adjusted   # 9/5/2012
{
  ##details<<
  ## Plots the input variables.

  for (i.plot in plot.format.list)
  {
    plot.filename <- "plot_Licor_temp_light";
    s.plot.settings.begin.end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot.not.na(x.time, VPD              , pch=20, type="l", ylab="", main="VPD, Licor");
    plot.not.na(x.time, E.transpiration  , pch=20, type="l", ylab="", main="Transpiration Trmmol, Licor");
    plot.not.na(x.time, leaf.temp        , pch=20, type="l", ylab="", main="Leaf Temp, Licor");
    plot.not.na(x.time, air.temp         , pch=20, type="l", ylab="", main="Air Temp, Licor");
    plot.not.na(x.time, light.in         , pch=20, type="l", ylab="", main="Light in  PARi, Licor");
    plot.not.na(x.time, light.out        , pch=20, type="l", ylab="", main="Light out PARo, Licor");
    #plot.not.na(x.time, flow.adjusted    , pch=20, type="l", ylab="", main="Adjusted flow when water removed"); # 9/5/2012
    #plot.not.na(x.time, x.time           , pch=20, type="l", ylab="", main="(blank)");                          # 9/5/2012

    #axis(3); axis(4); # add axis labels to top and right sides
    s.plot.settings.begin.end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

