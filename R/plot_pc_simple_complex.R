#' Title
#'
#' @param chamber_Totalpc_using_simple_Delta_for_gm
#' @param chamber_Totalpc_using_simple_Delta_for_modeling
#' @param chamber_Totalpc_using_complex_Delta_no_decarboxylation
#' @param chamber_Totalpc_using_complex_Delta_full_model
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return
#' @export
#'
#' @examples
plot_pc_simple_complex <-
function# Plot pc, simple and complex
###
(chamber_Totalpc_using_simple_Delta_for_gm
###
, chamber_Totalpc_using_simple_Delta_for_modeling
###
, chamber_Totalpc_using_complex_Delta_no_decarboxylation
###
, chamber_Totalpc_using_complex_Delta_full_model
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
    plot_filename <- "plot_pc_simple_complex";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(4,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, chamber_Totalpc_using_simple_Delta_for_gm             , pch=20, type="l", ylab="", main="Total pc using simple D for gm, includes boundary layer");
    plot_not_na(x_time, chamber_Totalpc_using_simple_Delta_for_modeling       , pch=20, type="l", ylab="", main="Total pc using simple D for modeling");
    plot_not_na(x_time, chamber_Totalpc_using_complex_Delta_no_decarboxylation, pch=20, type="l", ylab="", main="Total pc using complex D, no decarboxylation");
    plot_not_na(x_time, chamber_Totalpc_using_complex_Delta_full_model        , pch=20, type="l", ylab="", main="Total pc using complex D, full model");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  return( NULL );
  ### NULL
}

