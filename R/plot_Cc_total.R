#' Plot Cc, ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol
#'
#' Plots the input variables.
#'
#' @param chamber_TotalCc
#' @param chamber_12Cc
#' @param chamber_13Cc
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return NULL
#'
#' @examples
plot_Cc_total <-
function# Plot Cc, ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol
###
(chamber_TotalCc
###
, chamber_12Cc
###
, chamber_13Cc
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
    plot_filename <- "plot_Cc_ppm_CO2_concentration";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, chamber_TotalCc, pch=20, type="l", ylab="", main="Total Cc, ppm CO2 concentration at the site of carboxylation");
    plot_not_na(x_time, chamber_12Cc   , pch=20, type="l", ylab="", main="12 Cc, ppm CO2 concentration at the site of carboxylation");
    plot_not_na(x_time, chamber_13Cc   , pch=20, type="l", ylab="", main="13 Cc, ppm CO2 concentration at the site of carboxylation");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  invisible(NULL);
  ### NULL
}

