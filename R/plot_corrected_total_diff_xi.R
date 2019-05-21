#' Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample
#'
#' Plots the input variables.
#'
#' @param ref.12C
#' @param ref.13C
#' @param cham.12C
#' @param cham.13C
#' @param ref_tot
#' @param cham_tot
#' @param diff_tot
#' @param xi
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return NULL
#'
#' @examples
plot_corrected_total_diff_xi <-
function# Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample
###
(ref.12C
###
, ref.13C
###
, cham.12C
###
, cham.13C
###
, ref_tot
###
, cham_tot
###
, diff_tot
###
, xi
###
, x_time
###
, plot_format_list
###
, output_fn_prefix
###
)
{
  ##details<<
  ## Plots the input variables.

  for (i_plot in plot_format_list)
  {
    plot_filename <- "plot_corrected_total_diff";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(4,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, ref.12C , pch=20, type="l", ylab="ppm", main="reference 12CO2");
    plot_not_na(x_time, cham.12C, pch=20, type="l", ylab="ppm", main="chamber 12CO2");
    plot_not_na(x_time, ref.13C , pch=20, type="l", ylab="ppm", main="reference 13CO2");
    plot_not_na(x_time, cham.13C, pch=20, type="l", ylab="ppm", main="chamber 13CO2");
    plot_not_na(x_time, ref_tot , pch=20, type="l", ylab="ppm", main="reference total");
    plot_not_na(x_time, cham_tot, pch=20, type="l", ylab="ppm", main="chamber total");
    plot_not_na(x_time, diff_tot, pch=20, type="l", ylab="ppm", main="chamber-reference difference");
    plot_not_na(x_time, xi      , pch=20, type="l", ylab=""   , main="xi");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  invisible(NULL);
  ### NULL
}

