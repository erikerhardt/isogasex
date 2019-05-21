#' Plot Licor uin, xin, La, Atm_press values 7/21/2012
#'
#' Plots the input variables.
#'
#' @param Licor_flow_uin
#' @param Licor_H2OR_xin
#' @param Licor_La
#' @param Licor_Atm_press
#' @param x_time
#' @param plot_format_list
#' @param output_fn_prefix
#'
#' @return NULL
#'
#' @examples
plot_Licor_flow_press <-
function# Plot Licor uin, xin, La, Atm_press values 7/21/2012
###
(Licor_flow_uin
###
, Licor_H2OR_xin
###
, Licor_La
###
, Licor_Atm_press
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
    plot_filename <- "plot_Licor_flow_press";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot_not_na(x_time, Licor_flow_uin   , pch=20, type="l", ylab="", main="Licor_flow_uin");
    plot_not_na(x_time, Licor_H2OR_xin   , pch=20, type="l", ylab="", main="Licor_H2OR_xin");
    plot_not_na(x_time, Licor_La         , pch=20, type="l", ylab="", main="Licor_La");
    plot_not_na(x_time, Licor_Atm_press        , pch=20, type="l", ylab="", main="Licor_Atm_press");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end", i_plot);
  } # plotting loop

  invisible(NULL);
  ### NULL
}

