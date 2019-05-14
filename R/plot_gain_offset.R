plot_gain_offset <-
function# Plot active gains, offsets, and corrected magnitudes
###
(gain.12C
###
, gain.13C
###
, offset.12C
###
, offset.13C
###
, x.time
###
, plot.format.list
###
, output.fn.prefix
###
)
## DEBUG
# gain.12C   =  val.temp$gain.12C
# gain.13C   =  val.temp$gain.13C
# offset.12C =  val.temp$offset.12C
# offset.13C =  val.temp$offset.13C
# x.time     =  val.TDL$time
{
  ##details<<
  ## Plots the input variables.

  for (i.plot in plot.format.list)
  {
    plot.filename <- "plot_gain_offset";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x.time, gain.12C,   pch=20, type="l", ylab="ppm", main="gain 12CO2");
    plot_not_na(x.time, gain.13C,   pch=20, type="l", ylab="ppm", main="gain 13CO2");
    plot_not_na(x.time, offset.12C, pch=20, type="l", ylab="ppm", main="offset 12CO2");
    plot_not_na(x.time, offset.13C, pch=20, type="l", ylab="ppm", main="offset 13CO2");

    temp.corrected <-
      f_val_calc_corrected(1, 1, gain.12C, gain.13C, offset.12C, offset.13C)
    corrected.12C <- temp.corrected$corrected.12C;
    corrected.13C <- temp.corrected$corrected.13C;

    ylim.12 <- c(min(min(corrected.12C[!is.na(corrected.12C)]),1),max(max(corrected.12C[!is.na(corrected.12C)]),1));
    ylim.13 <- c(min(min(corrected.13C[!is.na(corrected.13C)]),1),max(max(corrected.13C[!is.na(corrected.13C)]),1));

    plot(x.time, corrected.12C, pch=20, type="l", ylim=ylim.12, ylab="ppm", main="corrected 12CO2 (relative to input of 1)");
    abline(h=1); # 1 is reference
    plot(x.time, corrected.13C, pch=20, type="l", ylim=ylim.13, ylab="ppm", main="corrected 13CO2 (relative to input of 1)");
    abline(h=1); # 1 is reference

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end", i.plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

