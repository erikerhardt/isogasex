#' Title
#'
#' @param TDL_A_photosynthesis
#' @param TDL_12A_photosynthesis
#' @param TDL_13A_photosynthesis
#' @param Licor_A_photosynthesis
#' @param Delta_from_ratios_in_out
#' @param Delta_from_A_ratio
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return
#' @export
#'
#' @examples
plot_A_photosynthesis <-
function# Plot A photosynthesis
###
(TDL_A_photosynthesis
###
, TDL_12A_photosynthesis
###
, TDL_13A_photosynthesis
###
, Licor_A_photosynthesis
###
, Delta_from_ratios_in_out
###
, Delta_from_A_ratio
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
    plot_filename <- "plot_A_photosynthesis";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, Licor_A_photosynthesis  , pch=20, type="l", ylab="", main="A, Licor photosynthesis");
    plot_not_na(x_time, TDL_A_photosynthesis    , pch=20, type="l", ylab="", main="A, TDL photosynthesis");
    plot_not_na(x_time, TDL_12A_photosynthesis  , pch=20, type="l", ylab="", main="12A, TDL photosynthesis");
    plot_not_na(x_time, TDL_13A_photosynthesis  , pch=20, type="l", ylab="", main="13A, TDL photosynthesis");
    plot_not_na(x_time, Delta_from_ratios_in_out, pch=20, type="l", ylab="", main="Delta from ratios in and out?");
    plot_not_na(x_time, Delta_from_A_ratio      , pch=20, type="l", ylab="", main="D from A ratio, should be the same as Delta_obs");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

