plot_Ca_Cs <-
function# Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface
###
(chamber.TotalCa
###
, chamber.12Ca
###
, chamber.13Ca
###
, chamber.TotalCs
###
, chamber.12Cs
###
, chamber.13Cs
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
    plot.filename <- "plot_Ca_Cs_CO2_concentrations";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, chamber.TotalCa, pch=20, type="l", ylab="", main="Total Ca, ppm CO2 concentration above the leaf");
    plot_not_na(x.time, chamber.TotalCs, pch=20, type="l", ylab="", main="Total Cs, ppm CO2 concentration at the leaf surface");
    plot_not_na(x.time, chamber.12Ca   , pch=20, type="l", ylab="", main="12 Ca, ppm CO2 concentration above the leaf");
    plot_not_na(x.time, chamber.12Cs   , pch=20, type="l", ylab="", main="12 Cs, ppm CO2 concentration at the leaf surface");
    plot_not_na(x.time, chamber.13Ca   , pch=20, type="l", ylab="", main="13 Ca, ppm CO2 concentration above the leaf");
    plot_not_na(x.time, chamber.13Cs   , pch=20, type="l", ylab="", main="13 Cs, ppm CO2 concentration at the leaf surface");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

