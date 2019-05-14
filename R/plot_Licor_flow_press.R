plot_Licor_flow_press <-
function# Plot Licor uin, xin, La, Atm.press values 7/21/2012
###
(Licor.flow.uin
###
, Licor.H2OR.xin
###
, Licor.La
###
, Licor.Atm.press
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
    plot.filename <- "plot_Licor_flow_press";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, Licor.flow.uin   , pch=20, type="l", ylab="", main="Licor.flow.uin");
    plot_not_na(x.time, Licor.H2OR.xin   , pch=20, type="l", ylab="", main="Licor.H2OR.xin");
    plot_not_na(x.time, Licor.La         , pch=20, type="l", ylab="", main="Licor.La");
    plot_not_na(x.time, Licor.Atm.press        , pch=20, type="l", ylab="", main="Licor.Atm.press");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

