#' Plot pc, total partial pressure of CO2 at the site of carboxylation
#'
#' Plots the input variables.
#'
#' @param chamber_Totalpc_using_gm xxxPARAMxxx
#' @param chamber_12pc_using_gm xxxPARAMxxx
#' @param chamber_13pc_using_gm xxxPARAMxxx
#' @param x_time xxxPARAMxxx
#' @param plot_format_list xxxPARAMxxx
#' @param output_fn_prefix xxxPARAMxxx
#'
#' @return NULL xxxRETURNxxx
#' @importFrom graphics par
#'
plot_pc_total <-
function# Plot pc, total partial pressure of CO2 at the site of carboxylation
###
(chamber_Totalpc_using_gm
###
, chamber_12pc_using_gm
###
, chamber_13pc_using_gm
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
    plot_filename <- "plot_pc_total_partial_pressure_CO2";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    graphics::par(mfrow=c(3,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, chamber_Totalpc_using_gm, pch=20, type="l", ylab="", main="Total pc, total partial pressure of CO2 at the site of carboxylation");
    plot_not_na(x_time, chamber_12pc_using_gm   , pch=20, type="l", ylab="", main="12 pc, total partial pressure of CO2 at the site of carboxylation");
    plot_not_na(x_time, chamber_13pc_using_gm   , pch=20, type="l", ylab="", main="13 pc, total partial pressure of CO2 at the site of carboxylation");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  invisible(NULL);
  ### NULL
}

