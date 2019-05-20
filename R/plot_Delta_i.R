#' Title
#'
#' @param chamber_Delta_i_simple_for_gm
#' @param chamber_Delta_i_simple_for_modeling
#' @param chamber_Delta_i_complex_for_gm
#' @param chamber_Delta_i_simple_for_gm_Delta_obs
#' @param chamber_Delta_i_complex_for_gm_Delta_obs
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return
#' @export
#'
#' @examples
plot_Delta_i <-
function# Plot Delta_i, predicted discrimination
###
(chamber_Delta_i_simple_for_gm
###
, chamber_Delta_i_simple_for_modeling
###
, chamber_Delta_i_complex_for_gm
###
, chamber_Delta_i_simple_for_gm_Delta_obs
###
, chamber_Delta_i_complex_for_gm_Delta_obs
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
    plot_filename <- "plot_Delta_i_predicted_discrimination";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(5,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, chamber_Delta_i_simple_for_gm           , pch=20, type="l", ylab="", main="Delta_i simple for gm");
    plot_not_na(x_time, chamber_Delta_i_simple_for_modeling     , pch=20, type="l", ylab="", main="Delta_i simple for modeling");
    plot_not_na(x_time, chamber_Delta_i_complex_for_gm          , pch=20, type="l", ylab="", main="Delta_i complex for gm");
    plot_not_na(x_time, chamber_Delta_i_simple_for_gm_Delta_obs , pch=20, type="l", ylab="", main="difference of Delta_i simple for gm and Delta_obs from TDL");
    plot_not_na(x_time, chamber_Delta_i_complex_for_gm_Delta_obs, pch=20, type="l", ylab="", main="difference of Delta_i complex for gm and Delta_obs from TDL");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

