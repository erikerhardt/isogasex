#' Title
#'
#' @param chamber_TotalCi
#' @param chamber_12Ci
#' @param chamber_13Ci
#' @param chamber_Totalpi
#' @param chamber_12pi
#' @param chamber_13pi
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return
#' @export
#'
#' @examples
plot_Ci_pi <-
function# Plot Ci, pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities
###
(chamber_TotalCi
###
, chamber_12Ci
###
, chamber_13Ci
###
, chamber_Totalpi
###
, chamber_12pi
###
, chamber_13pi
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
    plot_filename <- "plot_Ci_pi_partial_pressure_of_CO2";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, chamber_TotalCi, pch=20, type="l", ylab="", main="Total Ci, ppm CO2 concentration in the sub-stomatal cavities");
    plot_not_na(x_time, chamber_Totalpi, pch=20, type="l", ylab="", main="Total pi, partial pressure of CO2 in the substomatal cavities");
    plot_not_na(x_time, chamber_12Ci   , pch=20, type="l", ylab="", main="12 Ci, ppm CO2 concentration in the sub-stomatal cavities");
    plot_not_na(x_time, chamber_12pi   , pch=20, type="l", ylab="", main="12 pi, partial pressure of CO2 in the substomatal cavities");
    plot_not_na(x_time, chamber_13Ci   , pch=20, type="l", ylab="", main="13 Ci, ppm CO2 concentration in the sub-stomatal cavities");
    plot_not_na(x_time, chamber_13pi   , pch=20, type="l", ylab="", main="13 pi, partial pressure of CO2 in the substomatal cavities");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

