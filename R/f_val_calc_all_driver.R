#' This function calls all the \code{f_val_calc.*} files
#'
#' SECTION Licor data assign variables
#'
#' SECTION Concentrations.
#'
#' Calibration tank concentrations
#'
#' Calibration tank gain. \code{\link{f_val_calc_gain}}
#'
#' Calibration tank offset. \code{\link{f_val_calc_offset}}
#'
#' Corrected reference values. \code{\link{f_val_calc_corrected}}
#'
#' Corrected chamber values. \code{\link{f_val_calc_corrected}}
#'
#' Total reference and chamber values. \code{\link{f_val_calc_total_mol_fraction_CO2}}
#'
#' xi. \code{\link{f_val_calc_xi}}
#'
#' SECTION A Photosynthesis
#'
#' A, TDL photosynthesis. \code{\link{f_val_calc_TDL_A_photosynthesis}}
#'
#' 12A, TDL photosynthesis. \code{\link{f_val_calc_TDL_A_photosynthesis}}
#'
#' 13A, TDL photosynthesis. \code{\link{f_val_calc_TDL_A_photosynthesis}}
#'
#' Total A LI6400  Photo LI6400 header, these values listed because I will likely use them in a plot
#'
#' Delta from ratios in and out?, (Re/Ro)-1, is this really the same? \code{\link{f_val_calc_Delta_from_ratios_in_out}}
#'
#' D from A ratio, (Ro/(13A/12A))-1, should be the same as Dobs above. \code{\link{f_val_calc_Delta_from_A_ratio}}
#'
#' Select TDL or Licor based on switch
#'
#' SECTION Delta
#'
#' delta reference and chamber values. \code{\link{f_val_calc_delta_proportion}}
#'
#' delta diff
#'
#' Delta discrim
#'
#' Dobs observed discrimination. \code{\link{f_val_calc_Delta_obs}}
#'
#' Dobs per mil  1000*Dobs. \code{\link{f_val_calc_Delta_obs_permil}}
#'
#' delta13C Assimilated, isotopic composition of assimilated sugars. \code{\link{f_val_calc_delta_13C_Assim}}
#'
#' p (Co - Ce) / Co. \code{\link{f_val_calc_p}}
#'
#' delta13C Respired, isotopic composition of respired CO2. \code{\link{f_val_calc_delta_13C_Resp}}
#'
#' SECTION g conductance
#'
#' gbw BLcond  boundary layer conductance for water, Blcond is LI 6400 header
#'
#' gbc BLcond/1.37 boundary layer conductance for CO2
#'
#' gsw cond  stomatal conductance for water, cond is LI6400 header
#'
#' gsc gsw/1.6 stomatal conductance for CO2
#'
#' gtc, total (stomatal and boundary layer) conductance for CO2
#'
#' SECTION Cx and px (concentrations and pressures)
#'
#' Ca = Co, ppm CO2 concentration above the leaf = concentration leaving the leaf chamber = ambient CO2 concentration
#'
#' Cs, ppm CO2 concentration at the leaf surface, cs calculated from eq 40 Ball 1987 Ch 20 Stomatal Function, eds Zeiger, Farquhar, Cowan. \code{\link{f_val_calc_Cs}}
#'
#' pa, partial pressure of CO2 above the leaf, Press is the atmospheric pressure value from the LI6400. \code{\link{f_val_calc_pp}}
#'
#' ps, partial pressure of CO2 at the leaf surface, Press is the atmospheric pressure value from the LI6400. \code{\link{f_val_calc_pp}}
#' NB: same formula as pa, but using Cs instead of Co
#'
#' Ci, ppm CO2 concentration in the sub-stomatal cavities, ci calculated from eq 35 Ball 1987 Ch 20 Stomatal Function, eds Zeiger, Farquhar, Cowan. \code{\link{f_val_calc_Cs}}
#' NB: same formula as Cs, but using gtc instead of gbc
#'
#' pi, partial pressure of CO2 in the substomatal cavities, Press is the atmospheric pressure value from the LI6400. \code{\link{f_val_calc_pp}}
#' NB: same formula as pa, but using Ci instead of Co
#'
#' pi/pa total pi/pa or total Ci/Ca  ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions
#'
#' SECTION Delta_i predictions
#'
#' Delta_i simple for gm, predicted discrimination including boundary layer effects but not decarboxylation effects. \code{\link{f_val_calc_Delta_i_simple_for_gm}}
#'
#' Delta_i simple for modeling  a + (b-a) pi/pa predicted discrimination including boundary layer effects but using b adjustments to approximate effects of gm and decarboxylations. \code{\link{f_val_calc_Delta_i_simple_for_modeling}}
#'
#' Delta_i complex for gm  CHECK DERIVATION  predicted discrimination including boundary layer effects AND decarboxylation effects. \code{\link{f_val_calc_Delta_i_complex_for_gm}}
#'
#' Delta_i simple for gm-Dobs   (Di simple for gm)-(Dobs per mil)
#'
#' Delta_i complex for gm-Dobs  (Di complex for gm)-(Dobs per mil)
#'
#' SECTION gm mesophyll conductance
#'
#' gm point simple, internal leaf (mesophyll) conductance calculated for every D value ignoring decarboxylation effects. \code{\link{f_val_calc_gm_point_simple}}
#'
#' gm point complex, internal leaf (mesophyll) conductance calculated for every D value estimating decarboxylation effects. \code{\link{f_val_calc_gm_point_complex}}
#'
#' select one of the calculated gm values to use
#'
#' SECTION pc
#'
#' pc using gm,  total partial pressure of CO2 at the site of carboxylation, Press is the atmospheric pressure value from the LI6400. \code{\link{f_val_calc_pc_using_gm}}
#'
#' pc using simple D for gm, includes boundary layer. \code{\link{f_val_calc_pc_using_simple_Delta_for_gm}}
#'
#' pc using simple D for modeling. \code{\link{f_val_calc_pc_using_simple_Delta_for_modeling}}
#'
#' pc using complex D, no decarboxylation  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-Dpa]/(bs+al-b)  is this different from two up? They give different values but both may not be derived properly. \code{\link{f_val_calc_pc_using_complex_Delta_no_decarboxylation}}
#'
#' pc using complex D, full model  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-(eRd/k+fG*)-Dpa]/(bs+al-b)  includes boundary layer and decarboxylation effects. \code{\link{f_val_calc_pc_using_complex_Delta_full_model}}
#'
#' select one of the calculated pc values to use
#'
#' Cc  (pc*10^6)/(Press*1000)  ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol. \code{\link{f_val_calc_pp}}
#' NB: same formula as pa, but using pc instead of Co
#'
#' @param val_TDL
#' @param val_Licor
#' @param val_const
#' @param sw
#'
#' @return val_temp
#'
#' @examples
f_val_calc_all_driver <-
function# This function calls all the \code{f_val_calc.*} files
(val_TDL
###
, val_Licor
###
, val_const
###
, sw
###
)
{
  # DEBUG DEBUG DEBUG DEBUG DEBUG
  # val_TDL   <- val$sum$TDL;
  # val_Licor <- val$sum$Licor;
  # val_const <- val$const;
  # DEBUG DEBUG DEBUG DEBUG DEBUG
  val_temp <- as.list(new.env()); # hold current set of calculated values below

  #-------------------
  ##details<<
  ## SECTION Licor data assign_variables
  # LICOR DATA SECTION (moved from bottom to top 9/5/2012, so Licor_flow_uin can replace flow_adjusted)

    # VPD VpdL  LI6400 header, these values listed because I will likely use them in a plot
  val_temp$VPD                  <- val_Licor$VPD;
    # Transpiration Trmmol  LI6400 header, these values listed because I will likely use them in a plot
  val_temp$E_transpiration      <- val_Licor$E;
    # Leaf Temp Tleaf LI6400 header, these values listed because I will likely use them in a plot
  val_temp$leaf_temp            <- val_Licor$temp_leaf;
    # Air Temp ?  Tair  LI6400 header, these values listed because I will likely use them in a plot
  val_temp$air_temp             <- val_Licor$temp_air;
    # Light in  PARi  LI6400 header, these values listed because I will likely use them in a plot
  val_temp$light_in             <- val_Licor$par_int;
    # Light out PARo  LI6400 header, these values listed because I will likely use them in a plot
  val_temp$light_out            <- val_Licor$par_ext;
    # 7/21/2012 added Licor var to save
  val_temp$Licor_flow_uin       <- val_Licor$uin         ;
  val_temp$Licor_H2OR_xin       <- val_Licor$xin         ;
  val_temp$Licor_La             <- val_Licor$La          ;
  val_temp$Licor_Atm_press      <- val_Licor$Atm_press   ;
  val_temp$Licor_gsc            <- val_Licor$gsc         ;
  val_temp$Licor_Ci             <- val_Licor$Ci          ;
  val_temp$Licor_StmRat         <- val_Licor$StmRat      ;
  val_temp$Licor_gbw            <- val_Licor$gbw         ;
  val_temp$Licor_temp_block     <- val_Licor$temp_block  ;
  val_temp$Licor_Ce             <- val_Licor$Ce          ;
  val_temp$Licor_Co             <- val_Licor$Co          ;
  val_temp$Licor_xout           <- val_Licor$xout        ;
  val_temp$Licor_rh_ref         <- val_Licor$rh_ref      ;
  val_temp$Licor_rh_sam         <- val_Licor$rh_sam      ;
  val_temp$Licor_CsMch          <- val_Licor$CsMch       ;
  val_temp$Licor_HsMch          <- val_Licor$HsMch       ;
  val_temp$Licor_StableF        <- val_Licor$StableF     ;
  val_temp$Licor_Status         <- val_Licor$Status      ;

  # typically not used
  #    ,"VpdA"
  #    ,"Ci_Ca"
  #    ,"pi"
  #    ,"uc_20_mV"
  #    ,"uc_21_mV"
  #    ,"U_S"
  #    ,"Trans"
  #    ,"CndCO2"
  #    ,"Ref_mV"
  #    ,"xTemp1"
  #    ,"xTemp2"


  #-------------------
  ##details<<
  ## SECTION Concentrations.

  # also include interp concentration values "0.1-16" "2012-07-10"
  ##details<<
  ## Calibration tank concentrations
  val_temp$tank_hi_12  <- val_TDL$interp_tank_hi_12 ;
  val_temp$tank_hi_13  <- val_TDL$interp_tank_hi_13 ;
  val_temp$tank_low_12 <- val_TDL$interp_tank_low_12;
  val_temp$tank_low_13 <- val_TDL$interp_tank_low_13;

  ##details<<
  ## Calibration tank gain. \code{\link{f_val_calc_gain}}
  temp_gain <-
    f_val_calc_gain( val_const$true_value_hi_12C, val_const$true_value_hi_13C, val_const$true_value_lo_12C, val_const$true_value_lo_13C
                    ,val_TDL$interp_tank_hi_12, val_TDL$interp_tank_hi_13, val_TDL$interp_tank_low_12, val_TDL$interp_tank_low_13);
  val_temp$gain_12C <- temp_gain$gain_12C;
  val_temp$gain_13C <- temp_gain$gain_13C;

  ##details<<
  ## Calibration tank offset. \code{\link{f_val_calc_offset}}
  temp_offset <-
    f_val_calc_offset( val_const$true_value_hi_12C, val_const$true_value_hi_13C
                      ,val_temp$gain_12C, val_temp$gain_13C
                      ,val_TDL$interp_tank_hi_12, val_TDL$interp_tank_hi_13);
  val_temp$offset_12C <- temp_offset$offset_12C;
  val_temp$offset_13C <- temp_offset$offset_13C;

  # # Plot active gains, offsets, and corrected magnitudes
  #  p_o <- paste("Plot active gains, offsets, and corrected magnitudes\n"); wWw <- write_progress(p_o, time_start);
  #plot_gain_offset(val_temp$gain_12C, val_temp$gain_13C, val_temp$offset_12C, val_temp$offset_13C, val_TDL$time, plot_format_list);

  ##details<<
  ## Corrected reference values. \code{\link{f_val_calc_corrected}}
    # 12Ce  12C Site 1 reference Ce is the ppm CO2 concentration entering the leaf chamber, equivalent terms: reference, Site 1, CO2R
    # 13Ce  13C Site 1 reference
  temp_corrected <-
    f_val_calc_corrected(val_TDL$interp_reference_12, val_TDL$interp_reference_13
                        ,val_temp$gain_12C, val_temp$gain_13C
                        ,val_temp$offset_12C, val_temp$offset_13C)
    val_temp$reference_12Ce <- temp_corrected$corrected_12C;
    val_temp$reference_13Ce <- temp_corrected$corrected_13C;

  ##details<<
  ## Corrected chamber values. \code{\link{f_val_calc_corrected}}
    # 12Co  12C Site 21 chamber Co is the outgoing ppm CO2 concentration from the leaf chamber: sample, Site 21, CO2S, Ca, also air above the leaf
    # 13Co  13C Site 21 chamber
  temp_corrected <-
    f_val_calc_corrected(val_TDL$chamber_12, val_TDL$chamber_13
                        ,val_temp$gain_12C, val_temp$gain_13C
                        ,val_temp$offset_12C, val_temp$offset_13C)
    val_temp$chamber_12Co <- temp_corrected$corrected_12C;
    val_temp$chamber_13Co <- temp_corrected$corrected_13C;


  ##details<<
  ## Total reference and chamber values. \code{\link{f_val_calc_total_mol_fraction_CO2}}
    # Total Ce  Total Site 1
    # Total Co  Total Site 21
  val_temp$reference_TotalCe <-
    f_val_calc_total_mol_fraction_CO2(val_temp$reference_12Ce, val_temp$reference_13Ce, val_const$fo_13C);
  val_temp$chamber_TotalCo   <-
    f_val_calc_total_mol_fraction_CO2(val_temp$chamber_12Co, val_temp$chamber_13Co, val_const$fo_13C);

    # Ce-Co Total Site 1 - Total Site 21  I like having this as an output for diagnostics
  val_temp$chamber_reference_Total_diff_CeCo <- val_temp$reference_TotalCe - val_temp$chamber_TotalCo;
  val_temp$chamber_reference_12_diff_CeCo    <- val_temp$reference_12Ce    - val_temp$chamber_12Co;
  val_temp$chamber_reference_13_diff_CeCo    <- val_temp$reference_13Ce    - val_temp$chamber_13Co;

  ##details<<
  ## xi. \code{\link{f_val_calc_xi}}
    # xi Ce/(Ce-Co)  I like having this as an output for diagnostics
  val_temp$xi <-
    f_val_calc_xi(val_temp$reference_TotalCe, val_temp$chamber_TotalCo)

  # # Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample
  #  p_o <- paste("Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample\n"); wWw <- write_progress(p_o, time_start);
  #plot_corrected_total_diff_xi( val_temp$reference_12Ce, val_temp$reference_13Ce
  #                             ,val_temp$chamber_12Co, val_temp$chamber_13Co
  #                             ,val_temp$reference_TotalCe, val_temp$chamber_TotalCo
  #                             ,val_temp$chamber_reference_Total_diff_CeCo, val_temp$xi, val_TDL$time, plot_format_list);

  #-------------------
  ##details<<
  ## SECTION A Photosynthesis

  # # Deprecated (0.1-20, 9/5/2012) ##
    # Adjusted flow, LI6400 flow includes water in the entering air but the TDL removes this water before measuring and this effectively reduces the flow (use H2OR to correct it)
  #val_temp$flow_adjusted <-                                  # 9/5/2012
  #  f_val_calc_flow_adjusted(val_Licor$uin, val_Licor$xin);  # 9/5/2012

  ##details<<
  ## A, TDL photosynthesis. \code{\link{f_val_calc_TDL_A_photosynthesis}}
  val_temp$TDL_A_photosynthesis <-
    #f_val_calc_TDL_A_photosynthesis( val_temp$flow_adjusted, val_temp$reference_TotalCe  # 9/5/2012
    f_val_calc_TDL_A_photosynthesis( val_temp$Licor_flow_uin, val_temp$reference_TotalCe  # 9/5/2012
                                    ,val_temp$chamber_TotalCo, val_Licor$La);
  ##details<<
  ## 12A, TDL photosynthesis. \code{\link{f_val_calc_TDL_A_photosynthesis}}
  val_temp$TDL_12A_photosynthesis <-
    #f_val_calc_TDL_A_photosynthesis( val_temp$flow_adjusted, val_temp$reference_12Ce  # 9/5/2012
    f_val_calc_TDL_A_photosynthesis( val_temp$Licor_flow_uin, val_temp$reference_12Ce  # 9/5/2012
                                    ,val_temp$chamber_12Co, val_Licor$La);
  ##details<<
  ## 13A, TDL photosynthesis. \code{\link{f_val_calc_TDL_A_photosynthesis}}
  val_temp$TDL_13A_photosynthesis <-
    #f_val_calc_TDL_A_photosynthesis( val_temp$flow_adjusted, val_temp$reference_13Ce  # 9/5/2012
    f_val_calc_TDL_A_photosynthesis( val_temp$Licor_flow_uin, val_temp$reference_13Ce  # 9/5/2012
                                    ,val_temp$chamber_13Co, val_Licor$La);

  ##details<<
  ## Total A LI6400  Photo LI6400 header, these values listed because I will likely use them in a plot
  val_temp$Licor_A_photosynthesis <- val_Licor$A;

  ##details<<
  ## Delta from ratios in and out?, (Re/Ro)-1, is this really the same? \code{\link{f_val_calc_Delta_from_ratios_in_out}}
  val_temp$Delta_from_ratios_in_out <-
    f_val_calc_Delta_from_ratios_in_out( val_temp$reference_12Ce, val_temp$reference_13Ce
                                        ,val_temp$chamber_12Co,   val_temp$chamber_13Co)

  ##details<<
  ## D from A ratio, (Ro/(13A/12A))-1, should be the same as Dobs above. \code{\link{f_val_calc_Delta_from_A_ratio}}
  val_temp$Delta_from_A_ratio <-
    f_val_calc_Delta_from_A_ratio( val_temp$chamber_12Co, val_temp$chamber_13Co
                                  ,val_temp$TDL_12A_photosynthesis, val_temp$TDL_13A_photosynthesis)

  ##details<<
  ## Select TDL or Licor based on switch
  # switch added for A_photosynthesis 9/5/2012
  if (sw$Licor_or_TDL_A_photosynthesis == 0) { # Licor
    # use Licor
    val_temp$selected_A_photosynthesis   <- val_temp$Licor_A_photosynthesis;
    val_temp$selected_12A_photosynthesis <- rep(NA, length(val_temp$TDL_12A_photosynthesis));
    val_temp$selected_13A_photosynthesis <- rep(NA, length(val_temp$TDL_13A_photosynthesis));
    #val_temp$TDL_A_photosynthesis   <- rep(NA, length(val_temp$TDL_A_photosynthesis  ));
    #val_temp$TDL_12A_photosynthesis <- rep(NA, length(val_temp$TDL_12A_photosynthesis));
    #val_temp$TDL_13A_photosynthesis <- rep(NA, length(val_temp$TDL_13A_photosynthesis));
  }
  if (sw$Licor_or_TDL_A_photosynthesis == 1) { # TDL
    # use TDL
    val_temp$selected_A_photosynthesis   <- val_temp$TDL_A_photosynthesis  ;
    val_temp$selected_12A_photosynthesis <- val_temp$TDL_12A_photosynthesis;
    val_temp$selected_13A_photosynthesis <- val_temp$TDL_13A_photosynthesis;
    #val_temp$Licor_A_photosynthesis <- rep(NA, length(val_temp$Licor_A_photosynthesis));
  }


  # # Plot A photosynthesis
  #  p_o <- paste("Plot A photosynthesis\n"); wWw <- write_progress(p_o, time_start);
  #plot_A_photosynthesis( val_temp$TDL_A_photosynthesis, val_temp$TDL_12A_photosynthesis, val_temp$TDL_13A_photosynthesis
  #            ,val_temp$Licor_A_photosynthesis, val_temp$Delta_from_ratios_in_out, val_temp$Delta_from_A_ratio
  #            ,val_TDL$time, plot_format_list)
  #
  # # Plot Licor Temp Light values
  #  p_o <- paste("Plot Licor Temp Light values\n"); wWw <- write_progress(p_o, time_start);
  #plot_Licor_temp_light(val_temp$VPD, val_temp$E_transpiration, val_temp$leaf_temp, val_temp$air_temp
  #                      ,val_temp$light_in, val_temp$light_out, val_temp$flow_adjusted
  #                      ,val_TDL$time, plot_format_list)



  #-------------------
  ##details<<
  ## SECTION Delta

  ##details<<
  ## delta reference and chamber values. \code{\link{f_val_calc_delta_proportion}}
    # de  d13C Site 1
  val_temp$reference_delta_e <-
    f_val_calc_delta_proportion(val_temp$reference_12Ce, val_temp$reference_13Ce, val_const$Rstd_13C);
    # do  d13C Site 21
  val_temp$chamber_delta_o   <-
    f_val_calc_delta_proportion(val_temp$chamber_12Co, val_temp$chamber_13Co, val_const$Rstd_13C);

  ##details<<
  ## delta diff
    # do-de d Site 21 - d Site 1  I like having this as an output for diagnostics
  val_temp$chamber_reference_delta_diff_CoCe <- val_temp$chamber_delta_o - val_temp$reference_delta_e;

  ##details<<
  ## Delta discrim
  ##details<<
  ## Dobs observed discrimination. \code{\link{f_val_calc_Delta_obs}}
  val_temp$Delta_obs <-
    f_val_calc_Delta_obs(val_temp$reference_delta_e, val_temp$chamber_delta_o, val_temp$xi)
  ##details<<
  ## Dobs per mil  1000*Dobs. \code{\link{f_val_calc_Delta_obs_permil}}
  val_temp$Delta_obs_permil <-
    f_val_calc_Delta_obs_permil(val_temp$reference_delta_e, val_temp$chamber_delta_o, val_temp$xi)

  ##details<<
  ## delta13C Assimilated, isotopic composition of assimilated sugars. \code{\link{f_val_calc_delta_13C_Assim}}
  val_temp$delta_13C_Assim <-
    f_val_calc_delta_13C_Assim(val_temp$chamber_delta_o, val_temp$Delta_obs)

  ##details<<
  ## p (Co - Ce) / Co. \code{\link{f_val_calc_p}}
  val_temp$p <-
    f_val_calc_p(val_temp$reference_TotalCe, val_temp$chamber_TotalCo)

  ##details<<
  ## delta13C Respired, isotopic composition of respired CO2. \code{\link{f_val_calc_delta_13C_Resp}}
  val_temp$delta_13C_Resp <-
    f_val_calc_delta_13C_Resp(val_temp$reference_delta_e, val_temp$chamber_delta_o, val_temp$p)

  # # Plot delta, Delta, and p
  #  p_o <- paste("Plot delta, Delta, and p\n"); wWw <- write_progress(p_o, time_start);
  #plot_delta_Delta_p( val_temp$reference_delta_e, val_temp$chamber_delta_o, val_temp$chamber_reference_delta_diff_CoCe
  #                   ,val_temp$Delta_obs, val_temp$p, val_temp$delta_13C_Assim, val_temp$delta_13C_Resp
  #                   , val_TDL$time, plot_format_list)

  #-------------------
  ##details<<
  ## SECTION g conductance
  # 9/6/2012 moved above Cs when fixed it's calculation

  ##details<<
  ## gbw BLcond  boundary layer conductance for water, Blcond is LI 6400 header
  val_temp$chamber_Totalgbw <- val_Licor$gbw;
  ##details<<
  ## gbc BLcond/1.37 boundary layer conductance for CO2
  val_temp$chamber_Totalgbc <- val_Licor$gbw / val_const$gbc_1.37;
  val_temp$chamber_12gbc    <- val_temp$chamber_Totalgbc;
  val_temp$chamber_13gbc    <- val_temp$chamber_Totalgbc / (1 + (val_const$a_b / 1000));

  ##details<<
  ## gsw cond  stomatal conductance for water, cond is LI6400 header
  val_temp$chamber_Totalgsw <- val_Licor$gsc;
  ##details<<
  ## gsc gsw/1.6 stomatal conductance for CO2
  val_temp$chamber_Totalgsc <- val_temp$chamber_Totalgsw / val_const$gsc_1.6;
  val_temp$chamber_12gsc    <- val_temp$chamber_Totalgsc;
  val_temp$chamber_13gsc    <- val_temp$chamber_Totalgsc / (1 + (val_const$a_b / 1000));

  ##details<<
  ## gtc, total (stomatal and boundary layer) conductance for CO2
  val_temp$chamber_Totalgtc <- f_val_calc_gtc( val_temp$chamber_Totalgbc, val_temp$chamber_Totalgsc);
  val_temp$chamber_12gtc    <- val_temp$chamber_Totalgtc;
  val_temp$chamber_13gtc    <- f_val_calc_gtc( val_temp$chamber_13gbc, val_temp$chamber_13gsc);


  # # Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2
  #  p_o <- paste("Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2\n"); wWw <- write_progress(p_o, time_start);
  #plot_gbc_gsc_gtc(val_temp$chamber_Totalgbc, val_temp$chamber_13gbc
  #                ,val_temp$chamber_Totalgsc, val_temp$chamber_13gsc
  #                ,val_temp$chamber_Totalgtc, val_temp$chamber_13gtc
  #                ,val_TDL$time, plot_format_list)


  #-------------------
  ##details<<
  ## SECTION Cx and px (concentrations and pressures)

  ##details<<
  ## Ca = Co, ppm CO2 concentration above the leaf = concentration leaving the leaf chamber = ambient CO2 concentration
  val_temp$chamber_TotalCa <- val_temp$chamber_TotalCo;
  val_temp$chamber_12Ca    <- val_temp$chamber_12Co   ;
  val_temp$chamber_13Ca    <- val_temp$chamber_13Co   ;

  ##details<<
  ## Cs, ppm CO2 concentration at the leaf surface, cs calculated from eq 40 Ball 1987 Ch 20 Stomatal Function, eds Zeiger, Farquhar, Cowan. \code{\link{f_val_calc_Cs}}
  #val_temp$chamber_TotalCs <- f_val_calc_Cs(val_Licor$gbw, val_Licor$E, val_temp$chamber_TotalCo, val_temp$TDL_A_photosynthesis);
  #val_temp$chamber_12Cs    <- f_val_calc_Cs(val_Licor$gbw, val_Licor$E, val_temp$chamber_12Co   , val_temp$TDL_A_photosynthesis);
  #val_temp$chamber_13Cs    <- f_val_calc_Cs(val_Licor$gbw, val_Licor$E, val_temp$chamber_13Co   , val_temp$TDL_A_photosynthesis);
  # 7/28/2012 Using A from Licor until test effect of using A from TDL
  # switch added for A_photosynthesis 9/5/2012
  # fixed calculation of Cs, was using val_Licor$gbc before I changed name to gbw 9/6/2012
  val_temp$chamber_TotalCs <- f_val_calc_Cs(val_temp$chamber_Totalgbc, val_Licor$E, val_temp$chamber_TotalCo, val_temp$selected_A_photosynthesis);
  val_temp$chamber_12Cs    <- f_val_calc_Cs(val_temp$chamber_12gbc   , val_Licor$E, val_temp$chamber_12Co   , val_temp$selected_12A_photosynthesis);
  val_temp$chamber_13Cs    <- f_val_calc_Cs(val_temp$chamber_13gbc   , val_Licor$E, val_temp$chamber_13Co   , val_temp$selected_13A_photosynthesis);

  # # Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface
  #  p_o <- paste("Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface\n"); wWw <- write_progress(p_o, time_start);
  #plot_Ca_Cs(val_temp$chamber_TotalCa, val_temp$chamber_12Ca, val_temp$chamber_13Ca
  #          ,val_temp$chamber_TotalCs, val_temp$chamber_12Cs, val_temp$chamber_13Cs
  #          ,val_TDL$time, plot_format_list)

  ##details<<
  ## pa, partial pressure of CO2 above the leaf, Press is the atmospheric pressure value from the LI6400. \code{\link{f_val_calc_pp}}
  val_temp$chamber_Totalpa <- f_val_calc_pp(val_temp$chamber_TotalCo, val_Licor$Atm_press);
  val_temp$chamber_12pa    <- f_val_calc_pp(val_temp$chamber_12Co   , val_Licor$Atm_press);
  val_temp$chamber_13pa    <- f_val_calc_pp(val_temp$chamber_13Co   , val_Licor$Atm_press);

  ##details<<
  ## ps, partial pressure of CO2 at the leaf surface, Press is the atmospheric pressure value from the LI6400. \code{\link{f_val_calc_pp}}
  ## NB: same formula as pa, but using Cs instead of Co
  val_temp$chamber_Totalps <- f_val_calc_pp(val_temp$chamber_TotalCs, val_Licor$Atm_press);
  val_temp$chamber_12ps    <- f_val_calc_pp(val_temp$chamber_12Cs   , val_Licor$Atm_press);
  val_temp$chamber_13ps    <- f_val_calc_pp(val_temp$chamber_13Cs   , val_Licor$Atm_press);

  # # Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface
  #  p_o <- paste("Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface\n"); wWw <- write_progress(p_o, time_start);
  #plot_pa_ps(val_temp$chamber_Totalpa, val_temp$chamber_12pa, val_temp$chamber_13pa
  #          ,val_temp$chamber_Totalps, val_temp$chamber_12ps, val_temp$chamber_13ps
  #          ,val_TDL$time, plot_format_list)


  ##details<<
  ## Ci, ppm CO2 concentration in the sub-stomatal cavities, ci calculated from eq 35 Ball 1987 Ch 20 Stomatal Function, eds Zeiger, Farquhar, Cowan. \code{\link{f_val_calc_Cs}}
  ## NB: same formula as Cs, but using gtc instead of gbc
  #val_temp$chamber_TotalCi <- f_val_calc_Cs(val_temp$chamber_Totalgtc, val_Licor$E, val_temp$chamber_TotalCo, val_temp$TDL_A_photosynthesis);
  #val_temp$chamber_12Ci    <- f_val_calc_Cs(val_temp$chamber_12gtc,    val_Licor$E, val_temp$chamber_12Co   , val_temp$TDL_A_photosynthesis);
  #val_temp$chamber_13Ci    <- f_val_calc_Cs(val_temp$chamber_13gtc,    val_Licor$E, val_temp$chamber_13Co   , val_temp$TDL_A_photosynthesis);
  # 7/28/2012 Using A from Licor until test effect of using A from TDL
  # switch added for A_photosynthesis 9/5/2012
  val_temp$chamber_TotalCi <- f_val_calc_Cs(val_temp$chamber_Totalgtc, val_Licor$E, val_temp$chamber_TotalCo, val_temp$selected_A_photosynthesis);
  val_temp$chamber_12Ci    <- f_val_calc_Cs(val_temp$chamber_12gtc,    val_Licor$E, val_temp$chamber_12Co   , val_temp$selected_12A_photosynthesis);
  val_temp$chamber_13Ci    <- f_val_calc_Cs(val_temp$chamber_13gtc,    val_Licor$E, val_temp$chamber_13Co   , val_temp$selected_13A_photosynthesis);

  ##details<<
  ## pi, partial pressure of CO2 in the substomatal cavities, Press is the atmospheric pressure value from the LI6400. \code{\link{f_val_calc_pp}}
  ## NB: same formula as pa, but using Ci instead of Co
  val_temp$chamber_Totalpi <- f_val_calc_pp(val_temp$chamber_TotalCi, val_Licor$Atm_press);
  val_temp$chamber_12pi    <- f_val_calc_pp(val_temp$chamber_12Ci   , val_Licor$Atm_press);
  val_temp$chamber_13pi    <- f_val_calc_pp(val_temp$chamber_13Ci   , val_Licor$Atm_press);

  # # Plot Ci, Pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities
  #  p_o <- paste("Plot Ci, pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities\n"); wWw <- write_progress(p_o, time_start);
  #plot_Ci_pi(val_temp$chamber_TotalCi, val_temp$chamber_12Ci, val_temp$chamber_13Ci
  #          ,val_temp$chamber_Totalpi, val_temp$chamber_12pi, val_temp$chamber_13pi
  #          ,val_TDL$time, plot_format_list)

  #-------------------

  ##details<<
  ## pi/pa total pi/pa or total Ci/Ca  ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions
  val_temp$chamber_Totalpi_pa <- val_temp$chamber_Totalpi / val_temp$chamber_Totalpa;

  # # Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions
  #  p_o <- paste("Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf\n"); wWw <- write_progress(p_o, time_start);
  #plot_pi_pa(val_temp$chamber_Totalpi, val_temp$chamber_Totalpa, val_temp$chamber_Totalpi_pa
  #          ,val_TDL$time, plot_format_list)


  #-------------------
  ##details<<
  ## SECTION Delta_i predictions

  ##details<<
  ## Delta_i simple for gm, predicted discrimination including boundary layer effects but not decarboxylation effects. \code{\link{f_val_calc_Delta_i_simple_for_gm}}
  val_temp$chamber_Delta_i_simple_for_gm <-
    f_val_calc_Delta_i_simple_for_gm( val_const$a, val_const$b_gm, val_const$a_b
                                     ,val_temp$chamber_Totalpa, val_temp$chamber_Totalps, val_temp$chamber_Totalpi)

  ##details<<
  ## Delta_i simple for modeling  a + (b-a) pi/pa predicted discrimination including boundary layer effects but using b adjustments to approximate effects of gm and decarboxylations. \code{\link{f_val_calc_Delta_i_simple_for_modeling}}
  val_temp$chamber_Delta_i_simple_for_modeling <-
    f_val_calc_Delta_i_simple_for_modeling( val_const$a, val_const$b_modeling
                                     ,val_temp$chamber_Totalpa, val_temp$chamber_Totalpi)

  ##details<<
  ## Delta_i complex for gm  CHECK DERIVATION  predicted discrimination including boundary layer effects AND decarboxylation effects. \code{\link{f_val_calc_Delta_i_complex_for_gm}}
  val_temp$chamber_Delta_i_complex_for_gm <-
    f_val_calc_Delta_i_complex_for_gm( val_const$a, val_const$b_gm, val_const$a_b
                                      ,val_temp$chamber_Totalpa, val_temp$chamber_Totalps, val_temp$chamber_Totalpi
                                      ,val_const$b_gm
                                      ,val_const$f_photo_respiration, val_const$Gamma_star
                                      ,val_const$e, val_const$Rd, val_const$k);

  # CAN PROBABLY REMOVE THIS COMMENTED CODE TO SELECT DELTA_i
  #  # select one of the calculated Delta_i values to use
  #val_temp$chamber_Delta_i_to_use <- NULL; # as default
  #switch(sw$calc_Delta_i_to_use
  #    # 1 = Delta_i_simple_for_gm
  #        , val_temp$chamber_Delta_i_to_use <- val_temp$chamber_Delta_i_simple_for_gm
  #    # 2 = Delta_i_simple_for_modeling
  #        , val_temp$chamber_Delta_i_to_use <- val_temp$chamber_Delta_i_simple_for_modeling
  #    # 3 = Delta_i_complex_for_gm
  #        , val_temp$chamber_Delta_i_to_use <- val_temp$chamber_Delta_i_complex_for_gm
  #  );

  #-------------------

  ##details<<
  ## Delta_i simple for gm-Dobs   (Di simple for gm)-(Dobs per mil)
  val_temp$chamber_Delta_i_simple_for_gm_Delta_obs <-
    val_temp$chamber_Delta_i_simple_for_gm - val_temp$Delta_obs_permil; # val_temp$Delta_obs_permil;   # 8/19/2011
  ##details<<
  ## Delta_i complex for gm-Dobs  (Di complex for gm)-(Dobs per mil)
  val_temp$chamber_Delta_i_complex_for_gm_Delta_obs <-
    val_temp$chamber_Delta_i_complex_for_gm - val_temp$Delta_obs_permil; # val_temp$Delta_obs_permil;  # 8/19/2011



  # # Plot Delta_i, predicted discrimination
  #  p_o <- paste("Plot Delta_i, predicted discrimination\n"); wWw <- write_progress(p_o, time_start);
  #plot_Delta_i(val_temp$chamber_Delta_i_simple_for_gm, val_temp$chamber_Delta_i_simple_for_modeling, val_temp$chamber_Delta_i_complex_for_gm
  #            ,val_temp$chamber_Delta_i_simple_for_gm_Delta_obs, val_temp$chamber_Delta_i_complex_for_gm_Delta_obs
  #            ,val_TDL$time, plot_format_list)


  #-------------------
  ##details<<
  ## SECTION gm mesophyll conductance

  ##details<<
  ## gm point simple, internal leaf (mesophyll) conductance calculated for every D value ignoring decarboxylation effects. \code{\link{f_val_calc_gm_point_simple}}
  val_temp$chamber_Totalgm_point_simple <-
    # switch added for A_photosynthesis 9/5/2012
    #f_val_calc_gm_point_simple( val_const$b_gm, val_const$b_s, val_const$a_l, val_Licor$A
    f_val_calc_gm_point_simple( val_const$b_gm, val_const$b_s, val_const$a_l, val_temp$selected_A_photosynthesis
                               ,val_temp$chamber_Totalpa, val_temp$chamber_Delta_i_simple_for_gm, val_temp$Delta_obs_permil);  # 8/19/2011  use permil
  # switch added for A_photosynthesis 9/5/2012
  #val_temp$chamber_12gm_point_simple <- val_temp$chamber_Totalgm_point_simple;
  val_temp$chamber_12gm_point_simple <-
    f_val_calc_gm_point_simple( val_const$b_gm, val_const$b_s, val_const$a_l, val_temp$selected_12A_photosynthesis
                               ,val_temp$chamber_12pa, val_temp$chamber_Delta_i_simple_for_gm, val_temp$Delta_obs_permil);
  val_temp$chamber_13gm_point_simple <-
    # switch added for A_photosynthesis 9/5/2012
    #f_val_calc_gm_point_simple( val_const$b_gm, val_const$b_s, val_const$a_l, val_temp$TDL_13A_photosynthesis
    f_val_calc_gm_point_simple( val_const$b_gm, val_const$b_s, val_const$a_l, val_temp$selected_13A_photosynthesis
                               ,val_temp$chamber_13pa, val_temp$chamber_Delta_i_simple_for_gm, val_temp$Delta_obs_permil);

  ##details<<
  ## gm point complex, internal leaf (mesophyll) conductance calculated for every D value estimating decarboxylation effects. \code{\link{f_val_calc_gm_point_complex}}
  val_temp$chamber_Totalgm_point_complex <-
    # switch added for A_photosynthesis 9/5/2012
    #f_val_calc_gm_point_complex( val_const$b_gm, val_const$b_s, val_const$a_l, val_Licor$A
    f_val_calc_gm_point_complex( val_const$b_gm, val_const$b_s, val_const$a_l, val_temp$selected_A_photosynthesis
                                ,val_temp$chamber_Totalpa, val_temp$chamber_Delta_i_complex_for_gm, val_temp$Delta_obs_permil  # 8/19/2011  use permil
                                ,val_const$f_photo_respiration, val_const$Gamma_star
                                ,val_const$e, val_const$Rd, val_const$k);

  ##details<<
  ## select one of the calculated gm values to use
  val_temp$chamber_Totalgm_to_use <- NULL; # as default
  switch(sw$calc_gm_to_use
      # 1 = gm_point_simple
          , val_temp$chamber_Totalgm_to_use <- val_temp$chamber_Totalgm_point_simple
      # 2 = gm_point_complex
          , val_temp$chamber_Totalgm_to_use <- val_temp$chamber_Totalgm_point_complex
    );

  # # Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects
  #  p_o <- paste("Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects\n"); wWw <- write_progress(p_o, time_start);
  #plot_gm(val_temp$chamber_Totalgm_point_simple, val_temp$chamber_12gm_point_simple, val_temp$chamber_13gm_point_simple
  #       ,val_temp$chamber_Totalgm_point_complex
  #       ,val_TDL$time, plot_format_list)



  #-------------------
  ##details<<
  ## SECTION pc

  ##details<<
  ## pc using gm,  total partial pressure of CO2 at the site of carboxylation, Press is the atmospheric pressure value from the LI6400. \code{\link{f_val_calc_pc_using_gm}}
    # switch added for A_photosynthesis 9/5/2012
  #val_temp$chamber_Totalpc_using_gm <- f_val_calc_pc_using_gm(val_temp$chamber_Totalpi, val_Licor$A, val_temp$chamber_Totalgm_to_use);
  #val_temp$chamber_12pc_using_gm    <- f_val_calc_pc_using_gm(val_temp$chamber_12pi   , val_Licor$A, val_temp$chamber_Totalgm_to_use);
  #val_temp$chamber_13pc_using_gm    <- f_val_calc_pc_using_gm(val_temp$chamber_13pi   , val_Licor$A, val_temp$chamber_Totalgm_to_use);
  val_temp$chamber_Totalpc_using_gm <- f_val_calc_pc_using_gm(val_temp$chamber_Totalpi, val_temp$selected_A_photosynthesis, val_temp$chamber_Totalgm_to_use);
  val_temp$chamber_12pc_using_gm    <- f_val_calc_pc_using_gm(val_temp$chamber_12pi   , val_temp$selected_12A_photosynthesis, val_temp$chamber_Totalgm_to_use);
  val_temp$chamber_13pc_using_gm    <- f_val_calc_pc_using_gm(val_temp$chamber_13pi   , val_temp$selected_13A_photosynthesis, val_temp$chamber_Totalgm_to_use);

  ##details<<
  ## pc using simple D for gm, includes boundary layer. \code{\link{f_val_calc_pc_using_simple_Delta_for_gm}}
  val_temp$chamber_Totalpc_using_simple_Delta_for_gm <-
    f_val_calc_pc_using_simple_Delta_for_gm( val_temp$chamber_Delta_i_simple_for_gm, val_temp$chamber_Totalpa
                                            ,val_temp$chamber_Totalps, val_const$a, val_const$b_gm, val_const$a_b);

  ##details<<
  ## pc using simple D for modeling. \code{\link{f_val_calc_pc_using_simple_Delta_for_modeling}}
  val_temp$chamber_Totalpc_using_simple_Delta_for_modeling <-
    f_val_calc_pc_using_simple_Delta_for_modeling( val_temp$chamber_Delta_i_simple_for_modeling, val_temp$chamber_Totalpa
                                                  ,val_const$a, val_const$b_modeling);

  # # # What value of b to use here?
  ##details<<
  ## pc using complex D, no decarboxylation  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-Dpa]/(bs+al-b)  is this different from two up? They give different values but both may not be derived properly. \code{\link{f_val_calc_pc_using_complex_Delta_no_decarboxylation}}
  val_temp$chamber_Totalpc_using_complex_Delta_no_decarboxylation <-
    f_val_calc_pc_using_complex_Delta_no_decarboxylation( val_temp$chamber_Delta_i_complex_for_gm, val_temp$chamber_Totalpa
                                                         ,val_temp$chamber_Totalps, val_temp$chamber_Totalpi
                                                         ,val_const$a, val_const$a_b, val_const$a_l, val_const$b_modeling, val_const$b_s)

  # # # What value of b to use here?
  ##details<<
  ## pc using complex D, full model  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-(eRd/k+fG*)-Dpa]/(bs+al-b)  includes boundary layer and decarboxylation effects. \code{\link{f_val_calc_pc_using_complex_Delta_full_model}}
  val_temp$chamber_Totalpc_using_complex_Delta_full_model <-
    f_val_calc_pc_using_complex_Delta_full_model( val_temp$chamber_Delta_i_complex_for_gm, val_temp$chamber_Totalpa
                                                 ,val_temp$chamber_Totalps, val_temp$chamber_Totalpi
                                                 ,val_const$a, val_const$a_b, val_const$a_l, val_const$b_modeling, val_const$b_s
                                                 ,val_const$e, val_const$Rd, val_const$k, val_const$Gamma_star, val_const$f_photo_respiration)


  # # Plot pc, total partial pressure of CO2 at the site of carboxylation
  #  p_o <- paste("Plot pc, total partial pressure of CO2 at the site of carboxylation\n"); wWw <- write_progress(p_o, time_start);
  #plot_pc_total( val_temp$chamber_Totalpc_using_gm, val_temp$chamber_12pc_using_gm, val_temp$chamber_13pc_using_gm
  #              ,val_TDL$time, plot_format_list)
  #
  # # Plot pc, simple and complex
  #  p_o <- paste("Plot pc, simple and complex\n"); wWw <- write_progress(p_o, time_start);
  #plot_pc_simple_complex(val_temp$chamber_Totalpc_using_simple_Delta_for_gm, val_temp$chamber_Totalpc_using_simple_Delta_for_modeling
  #                      ,val_temp$chamber_Totalpc_using_complex_Delta_no_decarboxylation, val_temp$chamber_Totalpc_using_complex_Delta_full_model
  #                      ,val_TDL$time, plot_format_list)


  ##details<<
  ## select one of the calculated pc values to use
  val_temp$chamber_Totalpc_to_use <- NULL; # as default
  switch(sw$calc_pc_to_use
      # 1 = pc_using_gm
          , val_temp$chamber_Totalpc_to_use <- val_temp$chamber_Totalpc_using_gm
      # 2 = pc_using_simple_Delta_for_gm
          , val_temp$chamber_Totalpc_to_use <- val_temp$chamber_Totalpc_using_simple_Delta_for_gm
      # 3 = pc_using_simple_Delta_for_modeling
          , val_temp$chamber_Totalpc_to_use <- val_temp$chamber_Totalpc_using_simple_Delta_for_modeling
      # 4 = pc_using_complex_Delta_no_decarboxylation
          , val_temp$chamber_Totalpc_to_use <- val_temp$chamber_Totalpc_using_complex_Delta_no_decarboxylation
      # 5 = pc_using_complex_Delta_full_model
          , val_temp$chamber_Totalpc_to_use <- val_temp$chamber_Totalpc_using_complex_Delta_full_model
    );

  # (after pc -- one of the several pc's)
  ##details<<
  ## Cc  (pc*10^6)/(Press*1000)  ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol. \code{\link{f_val_calc_pp}}
  ## NB: same formula as pa, but using pc instead of Co
  val_temp$chamber_TotalCc <- f_val_calc_pp(val_temp$chamber_Totalpc_using_gm, val_Licor$Atm_press);
  val_temp$chamber_12Cc    <- f_val_calc_pp(val_temp$chamber_12pc_using_gm   , val_Licor$Atm_press);
  val_temp$chamber_13Cc    <- f_val_calc_pp(val_temp$chamber_13pc_using_gm   , val_Licor$Atm_press);


  # # Plot Cc, ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol
  #  p_o <- paste("Plot Cc, ppm CO2 concentration at the site of carboxylation\n"); wWw <- write_progress(p_o, time_start);
  #plot_Cc_total(val_temp$chamber_TotalCc, val_temp$chamber_12Cc, val_temp$chamber_13Cc
  #             ,val_TDL$time, plot_format_list)


  return( val_temp );
  ### val_temp
}

