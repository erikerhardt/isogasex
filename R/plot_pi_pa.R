#' Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions
#'
#' Plots the input variables.
#'
#' @param chamber_Totalpi xxxPARAMxxx
#' @param chamber_Totalpa xxxPARAMxxx
#' @param chamber_Totalpi_pa xxxPARAMxxx
#' @param x_time xxxPARAMxxx
#' @param plot_format_list xxxPARAMxxx
#' @param output_fn_prefix xxxPARAMxxx
#'
#' @return NULL xxxRETURNxxx
#'
plot_pi_pa <-
function# Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions
###
(chamber_Totalpi
###
, chamber_Totalpa
###
, chamber_Totalpi_pa
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
    plot_filename <- "plot_pi_pa_ratio";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, chamber_Totalpi   , pch=20, type="l", ylab="", main="Total pi, partial pressure of CO2 in the substomatal cavities");
    plot_not_na(x_time, chamber_Totalpa   , pch=20, type="l", ylab="", main="Total pa, partial pressure of CO2 above the leaf");
    plot_not_na(x_time, chamber_Totalpi_pa, pch=20, type="l", ylab="", main="Total pi/pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  invisible(NULL);
  ### NULL
}

