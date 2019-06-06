#' Plot active gains, offsets, and corrected magnitudes
#'
#' Plots the input variables.
#'
#' @param gain_12C xxxPARAMxxx
#' @param gain_13C xxxPARAMxxx
#' @param offset_12C xxxPARAMxxx
#' @param offset_13C xxxPARAMxxx
#' @param x_time xxxPARAMxxx
#' @param plot_format_list xxxPARAMxxx
#' @param output_fn_prefix xxxPARAMxxx
#'
#' @return NULL xxxRETURNxxx
#'
plot_gain_offset <-
function# Plot active gains, offsets, and corrected magnitudes
###
(gain_12C
###
, gain_13C
###
, offset_12C
###
, offset_13C
###
, x_time
###
, plot_format_list
###
, output_fn_prefix
###
)
## DEBUG
# gain_12C   =  val_temp$gain_12C
# gain_13C   =  val_temp$gain_13C
# offset_12C =  val_temp$offset_12C
# offset_13C =  val_temp$offset_13C
# x_time     =  val_TDL$time
{
  ##details<<
  ## Plots the input variables.

  for (i_plot in plot_format_list)
  {
    plot_filename <- "plot_gain_offset";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, gain_12C,   pch=20, type="l", ylab="ppm", main="gain 12CO2");
    plot_not_na(x_time, gain_13C,   pch=20, type="l", ylab="ppm", main="gain 13CO2");
    plot_not_na(x_time, offset_12C, pch=20, type="l", ylab="ppm", main="offset 12CO2");
    plot_not_na(x_time, offset_13C, pch=20, type="l", ylab="ppm", main="offset 13CO2");

    temp_corrected <-
      f_val_calc_corrected(1, 1, gain_12C, gain_13C, offset_12C, offset_13C)
    corrected_12C <- temp_corrected$corrected_12C;
    corrected_13C <- temp_corrected$corrected_13C;

    ylim_12 <- c(min(min(corrected_12C[!is.na(corrected_12C)]),1),max(max(corrected_12C[!is.na(corrected_12C)]),1));
    ylim_13 <- c(min(min(corrected_13C[!is.na(corrected_13C)]),1),max(max(corrected_13C[!is.na(corrected_13C)]),1));

    plot(x_time, corrected_12C, pch=20, type="l", ylim=ylim_12, ylab="ppm", main="corrected 12CO2 (relative to input of 1)");
    abline(h=1); # 1 is reference
    plot(x_time, corrected_13C, pch=20, type="l", ylim=ylim_13, ylab="ppm", main="corrected 13CO2 (relative to input of 1)");
    abline(h=1); # 1 is reference

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  invisible(NULL);
  ### NULL
}

