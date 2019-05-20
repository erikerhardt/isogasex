#' Title
#'
#' @param reference_delta_e
#' @param chamber_delta_o
#' @param chamber_reference_delta_diff_CoCe
#' @param Delta_obs
#' @param p
#' @param delta_13C_Assim
#' @param delta_13C_Resp
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return
#' @export
#'
#' @examples
plot_delta_Delta_p <-
function# Plot delta, Delta, and p
###
(reference_delta_e
###
, chamber_delta_o
###
, chamber_reference_delta_diff_CoCe
###
, Delta_obs
###
, p
###
, delta_13C_Assim
###
, delta_13C_Resp
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
    plot_filename <- "plot_delta_Delta_p";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(4,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, reference_delta_e                , pch=20, type="l", ylab="", main="reference delta.e");
    plot_not_na(x_time, chamber_delta_o                  , pch=20, type="l", ylab="", main="chamber delta.o");
    plot_not_na(x_time, chamber_reference_delta_diff_CoCe, pch=20, type="l", ylab="", main="delta chamber-reference difference");
    plot_not_na(x_time, Delta_obs                        , pch=20, type="l", ylab="", main="Delta obs");
    plot_not_na(x_time, p                                , pch=20, type="l", ylab="", main="p");
    plot_not_na(x_time, x_time                           , pch=20, type="l", ylab="", main="(blank)");
    plot_not_na(x_time, delta_13C_Assim                  , pch=20, type="l", ylab="", main="delta Assim");
    plot_not_na(x_time, delta_13C_Resp                   , pch=20, type="l", ylab="", main="delta Resp");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

