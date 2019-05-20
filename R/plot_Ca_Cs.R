#' Title
#'
#' @param chamber_TotalCa
#' @param chamber_12Ca
#' @param chamber_13Ca
#' @param chamber_TotalCs
#' @param chamber_12Cs
#' @param chamber_13Cs
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return
#' @export
#'
#' @examples
plot_Ca_Cs <-
function# Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface
###
(chamber_TotalCa
###
, chamber_12Ca
###
, chamber_13Ca
###
, chamber_TotalCs
###
, chamber_12Cs
###
, chamber_13Cs
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
    plot_filename <- "plot_Ca_Cs_CO2_concentrations";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, chamber_TotalCa, pch=20, type="l", ylab="", main="Total Ca, ppm CO2 concentration above the leaf");
    plot_not_na(x_time, chamber_TotalCs, pch=20, type="l", ylab="", main="Total Cs, ppm CO2 concentration at the leaf surface");
    plot_not_na(x_time, chamber_12Ca   , pch=20, type="l", ylab="", main="12 Ca, ppm CO2 concentration above the leaf");
    plot_not_na(x_time, chamber_12Cs   , pch=20, type="l", ylab="", main="12 Cs, ppm CO2 concentration at the leaf surface");
    plot_not_na(x_time, chamber_13Ca   , pch=20, type="l", ylab="", main="13 Ca, ppm CO2 concentration above the leaf");
    plot_not_na(x_time, chamber_13Cs   , pch=20, type="l", ylab="", main="13 Cs, ppm CO2 concentration at the leaf surface");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

