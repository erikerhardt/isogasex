plot_Ci_pi <-
function# Plot Ci, pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities
###
(chamber.TotalCi
###
, chamber.12Ci
###
, chamber.13Ci
###
, chamber.Totalpi
###
, chamber.12pi
###
, chamber.13pi
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
    plot.filename <- "plot_Ci_pi_partial_pressure_of_CO2";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, chamber.TotalCi, pch=20, type="l", ylab="", main="Total Ci, ppm CO2 concentration in the sub-stomatal cavities");
    plot_not_na(x.time, chamber.Totalpi, pch=20, type="l", ylab="", main="Total pi, partial pressure of CO2 in the substomatal cavities");
    plot_not_na(x.time, chamber.12Ci   , pch=20, type="l", ylab="", main="12 Ci, ppm CO2 concentration in the sub-stomatal cavities");
    plot_not_na(x.time, chamber.12pi   , pch=20, type="l", ylab="", main="12 pi, partial pressure of CO2 in the substomatal cavities");
    plot_not_na(x.time, chamber.13Ci   , pch=20, type="l", ylab="", main="13 Ci, ppm CO2 concentration in the sub-stomatal cavities");
    plot_not_na(x.time, chamber.13pi   , pch=20, type="l", ylab="", main="13 pi, partial pressure of CO2 in the substomatal cavities");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

