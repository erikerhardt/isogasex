#' Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2
#'
#' Plots the input variables.
#'
#' @param chamber_Totalgbc
#' @param chamber_13gbc
#' @param chamber_Totalgsc
#' @param chamber_13gsc
#' @param chamber_Totalgtc
#' @param chamber_13gtc
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return NULL
#' @export
#'
#' @examples
plot_gbc_gsc_gtc <-
function# Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2
###
(chamber_Totalgbc
###
, chamber_13gbc
###
, chamber_Totalgsc
###
, chamber_13gsc
###
, chamber_Totalgtc
###
, chamber_13gtc
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
    plot_filename <- "plot_gbc_gsc_gtc_conductance_for_CO2";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, chamber_Totalgbc, pch=20, type="l", ylab="", main="Total and 12 gbc, boundary layer conductance for CO2");
    plot_not_na(x_time, chamber_13gbc   , pch=20, type="l", ylab="", main="13 gbc, boundary layer conductance for CO2");
    plot_not_na(x_time, chamber_Totalgsc, pch=20, type="l", ylab="", main="Total and 12 gsc, stomatal conductance for CO2");
    plot_not_na(x_time, chamber_13gsc   , pch=20, type="l", ylab="", main="13 gsc, stomatal conductance for CO2");
    plot_not_na(x_time, chamber_Totalgtc, pch=20, type="l", ylab="", main="Total and 12 gtc, total (stomatal and boundary layer) conductance for CO2");
    plot_not_na(x_time, chamber_13gtc   , pch=20, type="l", ylab="", main="13 gtc, total (stomatal and boundary layer) conductance for CO2");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  invisible(NULL);
  ### NULL
}

