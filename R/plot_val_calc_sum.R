#' This function plots all the calculated values
#'
#' most plots are not created when not using Licor file
#'
#' Plots many variables by calling the \code{plot.*} functions.
#'
#' Plot active gains, offsets, and corrected magnitudes. \code{\link{plot_gain_offset}}
#'
#' Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample. \code{\link{plot_corrected_total_diff_xi}}
#'
#' Plot A photosynthesis. \code{\link{plot_A_photosynthesis}}
#'
#' Plot Licor Temp Light values. \code{\link{plot_Licor_temp_light}}
#'
#' Plot Licor uin, xin, La, Atm_press values 7/21/2012. \code{\link{plot_Licor_flow_press}}
#'
#' Plot delta, Delta, and p. \code{\link{plot_delta_Delta_p}}
#'
#' Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface. \code{\link{plot_Ca_Cs}}
#'
#' Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface. \code{\link{plot_pa_ps}}
#'
#' Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2. \code{\link{plot_gbc_gsc_gtc}}
#'
#' Plot Ci, Pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities. \code{\link{plot_Ci_pi}}
#'
#' Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions. \code{\link{plot_pi_pa}}
#'
#' Plot Delta_i, predicted discrimination. \code{\link{plot_Delta_i}}
#'
#' Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects. \code{\link{plot_gm}}
#'
#' Plot pc, total partial pressure of CO2 at the site of carboxylation. \code{\link{plot_pc_total}}
#'
#' Plot pc, simple and complex. \code{\link{plot_pc_simple_complex}}
#'
#' Plot Cc, ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol. \code{\link{plot_Cc_total}}
#'
#' @param val_TDL xxxPARAMxxx
#' @param val_Licor xxxPARAMxxx
#' @param val_const xxxPARAMxxx
#' @param val_temp xxxPARAMxxx
#' @param plot_format_list xxxPARAMxxx
#' @param output_fn_prefix xxxPARAMxxx
#' @param sw xxxPARAMxxx
#'
#' @return NULL xxxRETURNxxx
#'
plot_val_calc_sum <-
function# This function plots all the calculated values
### most plots are not created when not using Licor file
(val_TDL
###
, val_Licor
###
, val_const
###
, val_temp
###
, plot_format_list
###
, output_fn_prefix
###
, sw
###
)
{
  ##details<<
  ## Plots many variables by calling the \code{plot.*} functions.

  ## DEBUG
  # val_TDL          <- val$sum$TDL      ;
  # val_Licor        <- val$sum$Licor    ;
  # val_const        <- val$const        ;
  # val_temp         <- val$calc$sum     ;
  # plot_format_list <- plot_format_list ;
  # output_fn_prefix <- output_fn_prefix ;

  ##details<<
  ## Plot active gains, offsets, and corrected magnitudes. \code{\link{plot_gain_offset}}
    p_o <- paste("      Plot active gains, offsets, and corrected magnitudes\n"); write_out(p_o);
  plot_gain_offset(val_temp$gain_12C, val_temp$gain_13C, val_temp$offset_12C, val_temp$offset_13C, val_TDL$time, plot_format_list, output_fn_prefix);

  ##details<<
  ## Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample. \code{\link{plot_corrected_total_diff_xi}}
    p_o <- paste("      Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample\n"); write_out(p_o);
  plot_corrected_total_diff_xi( val_temp$reference_12Ce, val_temp$reference_13Ce
                               ,val_temp$chamber_12Co, val_temp$chamber_13Co
                               ,val_temp$reference_TotalCe, val_temp$chamber_TotalCo
                               ,val_temp$chamber_reference_Total_diff_CeCo, val_temp$xi, val_TDL$time, plot_format_list, output_fn_prefix);

 if (sw$use_Licor ) {
  ##details<<
  ## Plot A photosynthesis. \code{\link{plot_A_photosynthesis}}
    p_o <- paste("      Plot A photosynthesis\n"); write_out(p_o);
  plot_A_photosynthesis( val_temp$TDL_A_photosynthesis, val_temp$TDL_12A_photosynthesis, val_temp$TDL_13A_photosynthesis
              ,val_temp$Licor_A_photosynthesis, val_temp$Delta_from_ratios_in_out, val_temp$Delta_from_A_ratio
              ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot Licor Temp Light values. \code{\link{plot_Licor_temp_light}}
    p_o <- paste("      Plot Licor Temp Light values\n"); write_out(p_o);
  plot_Licor_temp_light(val_temp$VPD, val_temp$E_transpiration, val_temp$leaf_temp, val_temp$air_temp
                        ,val_temp$light_in, val_temp$light_out
                        #, val_temp$flow_adjusted # 9/5/2012
                        ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot Licor uin, xin, La, Atm_press values 7/21/2012. \code{\link{plot_Licor_flow_press}}
    p_o <- paste("      Plot Licor uin, xin, La, Atm_press values\n"); write_out(p_o);
  plot_Licor_flow_press(val_temp$Licor_flow_uin, val_temp$Licor_H2OR_xin, val_temp$Licor_La, val_temp$Licor_Atm_press
                        ,val_TDL$time, plot_format_list, output_fn_prefix)
 } # if (sw$use_Licor )

  ##details<<
  ## Plot delta, Delta, and p. \code{\link{plot_delta_Delta_p}}
    p_o <- paste("      Plot delta, Delta, and p\n"); write_out(p_o);
  plot_delta_Delta_p( val_temp$reference_delta_e, val_temp$chamber_delta_o, val_temp$chamber_reference_delta_diff_CoCe
                     ,val_temp$Delta_obs, val_temp$p, val_temp$delta_13C_Assim, val_temp$delta_13C_Resp
                     , val_TDL$time, plot_format_list, output_fn_prefix)

 if (sw$use_Licor ) {
  ##details<<
  ## Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface. \code{\link{plot_Ca_Cs}}
    p_o <- paste("      Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface\n"); write_out(p_o);
  plot_Ca_Cs(val_temp$chamber_TotalCa, val_temp$chamber_12Ca, val_temp$chamber_13Ca
            ,val_temp$chamber_TotalCs, val_temp$chamber_12Cs, val_temp$chamber_13Cs
            ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface. \code{\link{plot_pa_ps}}
    p_o <- paste("      Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface\n"); write_out(p_o);
  plot_pa_ps(val_temp$chamber_Totalpa, val_temp$chamber_12pa, val_temp$chamber_13pa
            ,val_temp$chamber_Totalps, val_temp$chamber_12ps, val_temp$chamber_13ps
            ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2. \code{\link{plot_gbc_gsc_gtc}}
    p_o <- paste("      Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2\n"); write_out(p_o);
  plot_gbc_gsc_gtc(val_temp$chamber_Totalgbc, val_temp$chamber_13gbc
                  ,val_temp$chamber_Totalgsc, val_temp$chamber_13gsc
                  ,val_temp$chamber_Totalgtc, val_temp$chamber_13gtc
                  ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot Ci, Pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities. \code{\link{plot_Ci_pi}}
    p_o <- paste("      Plot Ci, pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities\n"); write_out(p_o);
  plot_Ci_pi(val_temp$chamber_TotalCi, val_temp$chamber_12Ci, val_temp$chamber_13Ci
            ,val_temp$chamber_Totalpi, val_temp$chamber_12pi, val_temp$chamber_13pi
            ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions. \code{\link{plot_pi_pa}}
    p_o <- paste("      Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf\n"); write_out(p_o);
  plot_pi_pa(val_temp$chamber_Totalpi, val_temp$chamber_Totalpa, val_temp$chamber_Totalpi_pa
            ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot Delta_i, predicted discrimination. \code{\link{plot_Delta_i}}
    p_o <- paste("      Plot Delta_i, predicted discrimination\n"); write_out(p_o);
  plot_Delta_i(val_temp$chamber_Delta_i_simple_for_gm, val_temp$chamber_Delta_i_simple_for_modeling, val_temp$chamber_Delta_i_complex_for_gm
              ,val_temp$chamber_Delta_i_simple_for_gm_Delta_obs, val_temp$chamber_Delta_i_complex_for_gm_Delta_obs
              ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects. \code{\link{plot_gm}}
    p_o <- paste("      Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects\n"); write_out(p_o);
  plot_gm(val_temp$chamber_Totalgm_point_simple, val_temp$chamber_12gm_point_simple, val_temp$chamber_13gm_point_simple
         ,val_temp$chamber_Totalgm_point_complex
         ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot pc, total partial pressure of CO2 at the site of carboxylation. \code{\link{plot_pc_total}}
    p_o <- paste("      Plot pc, total partial pressure of CO2 at the site of carboxylation\n"); write_out(p_o);
  plot_pc_total( val_temp$chamber_Totalpc_using_gm, val_temp$chamber_12pc_using_gm, val_temp$chamber_13pc_using_gm
                ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot pc, simple and complex. \code{\link{plot_pc_simple_complex}}
    p_o <- paste("      Plot pc, simple and complex\n"); write_out(p_o);
  plot_pc_simple_complex(val_temp$chamber_Totalpc_using_simple_Delta_for_gm, val_temp$chamber_Totalpc_using_simple_Delta_for_modeling
                        ,val_temp$chamber_Totalpc_using_complex_Delta_no_decarboxylation, val_temp$chamber_Totalpc_using_complex_Delta_full_model
                        ,val_TDL$time, plot_format_list, output_fn_prefix)

  ##details<<
  ## Plot Cc, ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol. \code{\link{plot_Cc_total}}
    p_o <- paste("      Plot Cc, ppm CO2 concentration at the site of carboxylation\n"); write_out(p_o);
  plot_Cc_total(val_temp$chamber_TotalCc, val_temp$chamber_12Cc, val_temp$chamber_13Cc
               ,val_TDL$time, plot_format_list, output_fn_prefix)
 } # if (sw$use_Licor )

  invisible(NULL);
  ### NULL
}

