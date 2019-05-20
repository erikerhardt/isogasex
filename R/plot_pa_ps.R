#' Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface
#'
#' Plots the input variables.
#'
#' @param chamber_Totalpa
#' @param chamber_12pa
#' @param chamber_13pa
#' @param chamber_Totalps
#' @param chamber_12ps
#' @param chamber_13ps
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return NULL
#' @export
#'
#' @examples
plot_pa_ps <-
function# Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface
###
(chamber_Totalpa
###
, chamber_12pa
###
, chamber_13pa
###
, chamber_Totalps
###
, chamber_12ps
###
, chamber_13ps
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
    plot_filename <- "plot_pa_ps_partial_pressure_of_CO2";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, chamber_Totalpa, pch=20, type="l", ylab="", main="Total pa, partial pressure of CO2 above the leaf");
    plot_not_na(x_time, chamber_Totalps, pch=20, type="l", ylab="", main="Total ps, partial pressure of CO2 at the leaf surface");
    plot_not_na(x_time, chamber_12pa   , pch=20, type="l", ylab="", main="12 pa, partial pressure of CO2 above the leaf");
    plot_not_na(x_time, chamber_12ps   , pch=20, type="l", ylab="", main="12 ps, partial pressure of CO2 at the leaf surface");
    plot_not_na(x_time, chamber_13pa   , pch=20, type="l", ylab="", main="13 pa, partial pressure of CO2 above the leaf");
    plot_not_na(x_time, chamber_13ps   , pch=20, type="l", ylab="", main="13 ps, partial pressure of CO2 at the leaf surface");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  invisible(NULL);
  ### NULL
}

