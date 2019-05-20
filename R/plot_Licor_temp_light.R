#' Title
#'
#' @param VPD
#' @param E_transpiration
#' @param leaf_temp
#' @param air_temp
#' @param light_in
#' @param light_out
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return
#' @export
#'
#' @examples
plot_Licor_temp_light <-
function# Plot Licor Temp Light values
###
(VPD
###
, E_transpiration
###
, leaf_temp
###
, air_temp
###
, light_in
###
, light_out
###
, x_time
###
, plot_format_list
###
, output_fn_prefix
###
)
            #, flow_adjusted   # 9/5/2012
{
  ##details<<
  ## Plots the input variables.

  for (i_plot in plot_format_list)
  {
    plot_filename <- "plot_Licor_temp_light";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, VPD              , pch=20, type="l", ylab="", main="VPD, Licor");
    plot_not_na(x_time, E_transpiration  , pch=20, type="l", ylab="", main="Transpiration Trmmol, Licor");
    plot_not_na(x_time, leaf_temp        , pch=20, type="l", ylab="", main="Leaf Temp, Licor");
    plot_not_na(x_time, air_temp         , pch=20, type="l", ylab="", main="Air Temp, Licor");
    plot_not_na(x_time, light_in         , pch=20, type="l", ylab="", main="Light in  PARi, Licor");
    plot_not_na(x_time, light_out        , pch=20, type="l", ylab="", main="Light out PARo, Licor");
    #plot_not_na(x_time, flow_adjusted    , pch=20, type="l", ylab="", main="Adjusted flow when water removed"); # 9/5/2012
    #plot_not_na(x_time, x_time           , pch=20, type="l", ylab="", main="(blank)");                          # 9/5/2012

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

