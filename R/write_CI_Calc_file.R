#' write output section
#'
#' Write all observed and calculated values with BS CIs for all time points.
#'
#' @param val
#' @param TDL_cycle
#' @param output_CI_Calc_fn
#' @param output_CI_Calc_last_fn
#'
#' @return val$write
#'
#' @examples
write_CI_Calc_file <-
function# write_output section
### Write all observed and calculated values with BS CIs for all time points.
(val
###
, TDL_cycle
###
, output_CI_Calc_fn
###
, output_CI_Calc_last_fn
###
)
{

  val$write$CI_Calc <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site_name"
    ,"first_ind"
    ,"ind"
    ,"gain_12C_CI_L"                                                ,"gain_12C.CI.U"                                                  #,"gain_12C"
    ,"gain_13C_CI_L"                                                ,"gain_13C.CI.U"                                                  #,"gain_13C"
    ,"offset_12C_CI_L"                                              ,"offset_12C.CI.U"                                                #,"offset_12C"
    ,"offset_13C_CI_L"                                              ,"offset_13C.CI.U"                                                #,"offset_13C"
    ,"reference_12Ce_CI_L"                                          ,"reference_12Ce.CI.U"                                            #,"reference_12Ce"
    ,"reference_13Ce_CI_L"                                          ,"reference_13Ce.CI.U"                                            #,"reference_13Ce"
    ,"chamber_12Co_CI_L"                                            ,"chamber_12Co.CI.U"                                              #,"chamber_12Co"
    ,"chamber_13Co_CI_L"                                            ,"chamber_13Co.CI.U"                                              #,"chamber_13Co"
    ,"reference_TotalCe_CI_L"                                       ,"reference_TotalCe.CI.U"                                         #,"reference_TotalCe"
    ,"chamber_TotalCo_CI_L"                                         ,"chamber_TotalCo.CI.U"                                           #,"chamber_TotalCo"
    ,"chamber_reference_Total_diff_CeCo_CI_L"                       ,"chamber_reference_Total_diff_CeCo.CI.U"                         #,"chamber_reference_Total_diff_CeCo"
    ,"chamber_reference_12_diff_CeCo_CI_L"                          ,"chamber_reference_12_diff_CeCo.CI.U"                            #,"chamber_reference_12_diff_CeCo"
    ,"chamber_reference_13_diff_CeCo_CI_L"                          ,"chamber_reference_13_diff_CeCo.CI.U"                            #,"chamber_reference_13_diff_CeCo"
    ,"xi_CI_L"                                                      ,"xi.CI.U"                                                        #,"xi"
    #,"flow_adjusted_CI_L"                                           ,"flow_adjusted.CI.U"                                             #,"flow_adjusted"              # 9/5/2012
    ,"TDL_A_photosynthesis_CI_L"                                    ,"TDL_A_photosynthesis.CI.U"                                      #,"TDL_A_photosynthesis"
    ,"TDL_12A_photosynthesis_CI_L"                                  ,"TDL_12A_photosynthesis.CI.U"                                    #,"TDL_12A_photosynthesis"
    ,"TDL_13A_photosynthesis_CI_L"                                  ,"TDL_13A_photosynthesis.CI.U"                                    #,"TDL_13A_photosynthesis"
    ,"Licor_A_photosynthesis_CI_L"                                  ,"Licor_A_photosynthesis.CI.U"                                    #,"Licor_A_photosynthesis"
    ,"Delta_from_ratios_in_out_CI_L"                                ,"Delta_from_ratios_in_out.CI.U"                                  #,"Delta_from_ratios_in_out"
    ,"Delta_from_A_ratio_CI_L"                                      ,"Delta_from_A_ratio.CI.U"                                        #,"Delta_from_A_ratio"
    ,"VPD_CI_L"                                                     ,"VPD.CI.U"                                                       #,"VPD"
    ,"E_transpiration_CI_L"                                         ,"E_transpiration.CI.U"                                           #,"E_transpiration"
    ,"leaf_temp_CI_L"                                               ,"leaf_temp.CI.U"                                                 #,"leaf_temp"
    ,"air_temp_CI_L"                                                ,"air_temp.CI.U"                                                  #,"air_temp"
    ,"light_in_CI_L"                                                ,"light_in.CI.U"                                                  #,"light_in"
    ,"light_out_CI_L"                                               ,"light_out.CI.U"                                                 #,"light_out"
    ,"reference_delta_e_CI_L"                                       ,"reference_delta_e.CI.U"                                         #,"reference_delta_e"
    ,"chamber_delta_o_CI_L"                                         ,"chamber_delta_o.CI.U"                                           #,"chamber_delta_o"
    ,"chamber_reference_delta_diff_CoCe_CI_L"                       ,"chamber_reference_delta_diff_CoCe.CI.U"                         #,"chamber_reference_delta_diff_CoCe"
    ,"Delta_obs_CI_L"                                               ,"Delta_obs.CI.U"                                                 #,"Delta_obs"
    ,"Delta_obs_permil_CI_L"                                        ,"Delta_obs_permil.CI.U"                                          #,"Delta_obs_permil"
    ,"delta_13C_Assim_CI_L"                                         ,"delta_13C_Assim.CI.U"                                           #,"delta_13C_Assim"
    ,"p_CI_L"                                                       ,"p.CI.U"                                                         #,"p"
    ,"delta_13C_Resp_CI_L"                                          ,"delta_13C_Resp.CI.U"                                            #,"delta_13C_Resp"
    ,"chamber_TotalCa_CI_L"                                         ,"chamber_TotalCa.CI.U"                                           #,"chamber_TotalCa"
    ,"chamber_12Ca_CI_L"                                            ,"chamber_12Ca.CI.U"                                              #,"chamber_12Ca"
    ,"chamber_13Ca_CI_L"                                            ,"chamber_13Ca.CI.U"                                              #,"chamber_13Ca"
    ,"chamber_TotalCs_CI_L"                                         ,"chamber_TotalCs.CI.U"                                           #,"chamber_TotalCs"
    ,"chamber_12Cs_CI_L"                                            ,"chamber_12Cs.CI.U"                                              #,"chamber_12Cs"
    ,"chamber_13Cs_CI_L"                                            ,"chamber_13Cs.CI.U"                                              #,"chamber_13Cs"
    ,"chamber_Totalpa_CI_L"                                         ,"chamber_Totalpa.CI.U"                                           #,"chamber_Totalpa"
    ,"chamber_12pa_CI_L"                                            ,"chamber_12pa.CI.U"                                              #,"chamber_12pa"
    ,"chamber_13pa_CI_L"                                            ,"chamber_13pa.CI.U"                                              #,"chamber_13pa"
    ,"chamber_Totalps_CI_L"                                         ,"chamber_Totalps.CI.U"                                           #,"chamber_Totalps"
    ,"chamber_12ps_CI_L"                                            ,"chamber_12ps.CI.U"                                              #,"chamber_12ps"
    ,"chamber_13ps_CI_L"                                            ,"chamber_13ps.CI.U"                                              #,"chamber_13ps"
    ,"chamber_Totalgbw_CI_L"                                        ,"chamber_Totalgbw.CI.U"                                          #,"chamber_Totalgbw"
    ,"chamber_Totalgbc_CI_L"                                        ,"chamber_Totalgbc.CI.U"                                          #,"chamber_Totalgbc"
    ,"chamber_12gbc_CI_L"                                           ,"chamber_12gbc.CI.U"                                             #,"chamber_12gbc"
    ,"chamber_13gbc_CI_L"                                           ,"chamber_13gbc.CI.U"                                             #,"chamber_13gbc"
    ,"chamber_Totalgsw_CI_L"                                        ,"chamber_Totalgsw.CI.U"                                          #,"chamber_Totalgsw"
    ,"chamber_Totalgsc_CI_L"                                        ,"chamber_Totalgsc.CI.U"                                          #,"chamber_Totalgsc"
    ,"chamber_12gsc_CI_L"                                           ,"chamber_12gsc.CI.U"                                             #,"chamber_12gsc"
    ,"chamber_13gsc_CI_L"                                           ,"chamber_13gsc.CI.U"                                             #,"chamber_13gsc"
    ,"chamber_Totalgtc_CI_L"                                        ,"chamber_Totalgtc.CI.U"                                          #,"chamber_Totalgtc"
    ,"chamber_12gtc_CI_L"                                           ,"chamber_12gtc.CI.U"                                             #,"chamber_12gtc"
    ,"chamber_13gtc_CI_L"                                           ,"chamber_13gtc.CI.U"                                             #,"chamber_13gtc"
    ,"chamber_TotalCi_CI_L"                                         ,"chamber_TotalCi.CI.U"                                           #,"chamber_TotalCi"
    ,"chamber_12Ci_CI_L"                                            ,"chamber_12Ci.CI.U"                                              #,"chamber_12Ci"
    ,"chamber_13Ci_CI_L"                                            ,"chamber_13Ci.CI.U"                                              #,"chamber_13Ci"
    ,"chamber_Totalpi_CI_L"                                         ,"chamber_Totalpi.CI.U"                                           #,"chamber_Totalpi"
    ,"chamber_12pi_CI_L"                                            ,"chamber_12pi.CI.U"                                              #,"chamber_12pi"
    ,"chamber_13pi_CI_L"                                            ,"chamber_13pi.CI.U"                                              #,"chamber_13pi"
    ,"chamber_Totalpi_pa_CI_L"                                      ,"chamber_Totalpi_pa.CI.U"                                        #,"chamber_Totalpi_pa"
    ,"chamber_Delta_i_simple_for_gm_CI_L"                           ,"chamber_Delta_i_simple_for_gm.CI.U"                             #,"chamber_Delta_i_simple_for_gm"
    ,"chamber_Delta_i_simple_for_modeling_CI_L"                     ,"chamber_Delta_i_simple_for_modeling.CI.U"                       #,"chamber_Delta_i_simple_for_modeling"
    ,"chamber_Delta_i_complex_for_gm_CI_L"                          ,"chamber_Delta_i_complex_for_gm.CI.U"                            #,"chamber_Delta_i_complex_for_gm"
    ,"chamber_Delta_i_simple_for_gm_Delta_obs_CI_L"                 ,"chamber_Delta_i_simple_for_gm_Delta_obs.CI.U"                   #,"chamber_Delta_i_simple_for_gm_Delta_obs"
    ,"chamber_Delta_i_complex_for_gm_Delta_obs_CI_L"                ,"chamber_Delta_i_complex_for_gm_Delta_obs.CI.U"                  #,"chamber_Delta_i_complex_for_gm_Delta_obs"
    ,"chamber_Totalgm_point_simple_CI_L"                            ,"chamber_Totalgm_point_simple.CI.U"                              #,"chamber_Totalgm_point_simple"
    ,"chamber_12gm_point_simple_CI_L"                               ,"chamber_12gm_point_simple.CI.U"                                 #,"chamber_12gm_point_simple"
    ,"chamber_13gm_point_simple_CI_L"                               ,"chamber_13gm_point_simple.CI.U"                                 #,"chamber_13gm_point_simple"
    ,"chamber_Totalgm_point_complex_CI_L"                           ,"chamber_Totalgm_point_complex_CI.U"                             #,"chamber_Totalgm_point_complex"
    ,"chamber_Totalgm_to_use_CI_L"                                  ,"chamber_Totalgm_to_use.CI.U"                                    #,"chamber_Totalgm_to_use"
    ,"chamber_Totalpc_using_gm_CI_L"                                ,"chamber_Totalpc_using_gm.CI.U"                                  #,"chamber_Totalpc_using_gm"
    ,"chamber_12pc_using_gm_CI_L"                                   ,"chamber_12pc_using_gm.CI.U"                                     #,"chamber_12pc_using_gm"
    ,"chamber_13pc_using_gm_CI_L"                                   ,"chamber_13pc_using_gm.CI.U"                                     #,"chamber_13pc_using_gm"
    ,"chamber_Totalpc_using_simple_Delta_for_gm_CI_L"               ,"chamber_Totalpc_using_simple_Delta_for_gm.CI.U"                 #,"chamber_Totalpc_using_simple_Delta_for_gm"
    ,"chamber_Totalpc_using_simple_Delta_for_modeling_CI_L"         ,"chamber_Totalpc_using_simple_Delta_for_modeling.CI.U"           #,"chamber_Totalpc_using_simple_Delta_for_modeling"
    ,"chamber_Totalpc_using_complex_Delta_no_decarboxylation_CI_L"  ,"chamber_Totalpc_using_complex_Delta_no_decarboxylation.CI.U"    #,"chamber_Totalpc_using_complex_Delta_no_decarboxylation"
    ,"chamber_Totalpc_using_complex_Delta_full_model_CI_L"          ,"chamber_Totalpc_using_complex_Delta_full_model.CI.U"            #,"chamber_Totalpc_using_complex_Delta_full_model"
    ,"chamber_Totalpc_to_use_CI_L"                                  ,"chamber_Totalpc_to_use.CI.U"                                    #,"chamber_Totalpc_to_use"
    ,"chamber_TotalCc_CI_L"                                         ,"chamber_TotalCc.CI.U"                                           #,"chamber_TotalCc"
    ,"chamber_12Cc_CI_L"                                            ,"chamber_12Cc.CI.U"                                              #,"chamber_12Cc"
    ,"chamber_13Cc_CI_L"                                            ,"chamber_13Cc.CI.U"                                              #,"chamber_13Cc"
    ,"Licor_flow_uin_CI_L"                                          ,"Licor_flow_uin.CI.U"                                            #,"Licor_flow_uin"
    ,"Licor_H2OR_xin_CI_L"                                          ,"Licor_H2OR_xin.CI.U"                                            #,"Licor_H2OR_xin"
    ,"Licor_La_CI_L"                                                ,"Licor_La.CI.U"                                                  #,"Licor_La"
    ,"Licor_Atm_press_CI_L"                                               ,"Licor_Atm_press.CI.U"                                                 #,"Licor_Atm_press"
    ,"Licor_gsc_CI_L"                                                     ,"Licor_gsc.CI.U"                                                       #,"Licor_gsc"
    ,"Licor_Ci_CI_L"                                                      ,"Licor_Ci.CI.U"                                                        #,"Licor_Ci"
    ,"Licor_StmRat_CI_L"                                                  ,"Licor_StmRat.CI.U"                                                    #,"Licor_StmRat"
    ,"Licor_gbw_CI_L"                                                     ,"Licor_gbw.CI.U"                                                       #,"Licor_gbw"
    ,"Licor_temp_block_CI_L"                                              ,"Licor_temp_block.CI.U"                                                #,"Licor_temp_block"
    ,"Licor_Ce_CI_L"                                                      ,"Licor_Ce.CI.U"                                                        #,"Licor_Ce"
    ,"Licor_Co_CI_L"                                                      ,"Licor_Co.CI.U"                                                        #,"Licor_Co"
    ,"Licor_xout_CI_L"                                                    ,"Licor_xout.CI.U"                                                      #,"Licor_xout"
    ,"Licor_rh_ref"                                                       ,"Licor_rh_ref"                                                         #,"Licor_rh_ref"
    ,"Licor_rh_sam_CI_L"                                                  ,"Licor_rh_sam.CI.U"                                                    #,"Licor_rh_sam"
    ,"Licor_CsMch_CI_L"                                                   ,"Licor_CsMch.CI.U"                                                     #,"Licor_CsMch"
    ,"Licor_HsMch_CI_L"                                                   ,"Licor_HsMch.CI.U"                                                     #,"Licor_HsMch"
    ,"Licor_StableF_CI_L"                                                 ,"Licor_StableF.CI.U"                                                   #,"Licor_StableF"
    ,"Licor_Status_CI_L"                                                  ,"Licor_Status.CI.U"                                                    #,"Licor_Status"
    # typically not used
    #,"VpdA_CI_L"        ,"VpdA.CI.U"         #,"VpdA"
    #,"Ci_Ca_CI_L"       ,"Ci_Ca.CI.U"        #,"Ci_Ca"
    #,"pi_CI_L"          ,"pi.CI.U"           #,"pi"
    #,"uc_20_mV_CI_L"    ,"uc_20_mV.CI.U"     #,"uc_20_mV"
    #,"uc_21_mV_CI_L"    ,"uc_21_mV.CI.U"     #,"uc_21_mV"
    #,"U_S_CI_L"         ,"U_S.CI.U"          #,"U_S"
    #,"Trans_CI_L"       ,"Trans.CI.U"        #,"Trans"
    #,"CndCO2_CI_L"      ,"CndCO2.CI.U"       #,"CndCO2"
    #,"Ref_mV_CI_L"      ,"Ref_mV.CI.U"       #,"Ref_mV"
    #,"xTemp1_CI_L"      ,"xTemp1.CI.U"       #,"xTemp1"
    #,"xTemp2_CI_L"      ,"xTemp2.CI.U"       #,"xTemp2"
    , sep=",");

  for (i_time in 1:val$sum$TDL$n) {
    val$write$CI_Calc <-
      rbind( val$write$CI_Calc
        ,paste(
           format(val$sum$TDL$time[i_time],format="%Y-%m-%d")
          ,val$sum$TDL$site[i_time]
          ,TDL_cycle$table_name[(TDL_cycle$table[,1] == val$sum$TDL$site[i_time])]
          ,val$sum$TDL$first_ind[i_time]
          ,val$sum$TDL$ind[i_time]
          ,val$calc$CI$gain_12C[i_time,1]                                                ,val$calc$CI$gain_12C[i_time,2]                                                   #,val$calc$sum$gain_12C[i_time]
          ,val$calc$CI$gain_13C[i_time,1]                                                ,val$calc$CI$gain_13C[i_time,2]                                                   #,val$calc$sum$gain_13C[i_time]
          ,val$calc$CI$offset_12C[i_time,1]                                              ,val$calc$CI$offset_12C[i_time,2]                                                 #,val$calc$sum$offset_12C[i_time]
          ,val$calc$CI$offset_13C[i_time,1]                                              ,val$calc$CI$offset_13C[i_time,2]                                                 #,val$calc$sum$offset_13C[i_time]
          ,val$calc$CI$reference_12Ce[i_time,1]                                          ,val$calc$CI$reference_12Ce[i_time,2]                                             #,val$calc$sum$reference_12Ce[i_time]
          ,val$calc$CI$reference_13Ce[i_time,1]                                          ,val$calc$CI$reference_13Ce[i_time,2]                                             #,val$calc$sum$reference_13Ce[i_time]
          ,val$calc$CI$chamber_12Co[i_time,1]                                            ,val$calc$CI$chamber_12Co[i_time,2]                                               #,val$calc$sum$chamber_12Co[i_time]
          ,val$calc$CI$chamber_13Co[i_time,1]                                            ,val$calc$CI$chamber_13Co[i_time,2]                                               #,val$calc$sum$chamber_13Co[i_time]
          ,val$calc$CI$reference_TotalCe[i_time,1]                                       ,val$calc$CI$reference_TotalCe[i_time,2]                                          #,val$calc$sum$reference_TotalCe[i_time]
          ,val$calc$CI$chamber_TotalCo[i_time,1]                                         ,val$calc$CI$chamber_TotalCo[i_time,2]                                            #,val$calc$sum$chamber_TotalCo[i_time]
          ,val$calc$CI$chamber_reference_Total_diff_CeCo[i_time,1]                       ,val$calc$CI$chamber_reference_Total_diff_CeCo[i_time,2]                          #,val$calc$sum$chamber_reference_Total_diff_CeCo[i_time]
          ,val$calc$CI$chamber_reference_12_diff_CeCo[i_time,1]                          ,val$calc$CI$chamber_reference_12_diff_CeCo[i_time,2]                             #,val$calc$sum$chamber_reference_12_diff_CeCo[i_time]
          ,val$calc$CI$chamber_reference_13_diff_CeCo[i_time,1]                          ,val$calc$CI$chamber_reference_13_diff_CeCo[i_time,2]                             #,val$calc$sum$chamber_reference_13_diff_CeCo[i_time]
          ,val$calc$CI$xi[i_time,1]                                                      ,val$calc$CI$xi[i_time,2]                                                         #,val$calc$sum$xi[i_time]
          #,val$calc$CI$flow_adjusted[i_time,1]                                           ,val$calc$CI$flow_adjusted[i_time,2]                                              #,val$calc$sum$flow_adjusted[i_time]        # 9/5/2012
          ,val$calc$CI$TDL_A_photosynthesis[i_time,1]                                    ,val$calc$CI$TDL_A_photosynthesis[i_time,2]                                       #,val$calc$sum$TDL_A_photosynthesis[i_time]
          ,val$calc$CI$TDL_12A_photosynthesis[i_time,1]                                  ,val$calc$CI$TDL_12A_photosynthesis[i_time,2]                                     #,val$calc$sum$TDL_12A_photosynthesis[i_time]
          ,val$calc$CI$TDL_13A_photosynthesis[i_time,1]                                  ,val$calc$CI$TDL_13A_photosynthesis[i_time,2]                                     #,val$calc$sum$TDL_13A_photosynthesis[i_time]
          ,val$calc$CI$Licor_A_photosynthesis[i_time,1]                                  ,val$calc$CI$Licor_A_photosynthesis[i_time,2]                                     #,val$calc$sum$Licor_A_photosynthesis[i_time]
          ,val$calc$CI$Delta_from_ratios_in_out[i_time,1]                                ,val$calc$CI$Delta_from_ratios_in_out[i_time,2]                                   #,val$calc$sum$Delta_from_ratios_in_out[i_time]
          ,val$calc$CI$Delta_from_A_ratio[i_time,1]                                      ,val$calc$CI$Delta_from_A_ratio[i_time,2]                                         #,val$calc$sum$Delta_from_A_ratio[i_time]
          ,val$calc$CI$VPD[i_time,1]                                                     ,val$calc$CI$VPD[i_time,2]                                                        #,val$calc$sum$VPD[i_time]
          ,val$calc$CI$E_transpiration[i_time,1]                                         ,val$calc$CI$E_transpiration[i_time,2]                                            #,val$calc$sum$E_transpiration[i_time]
          ,val$calc$CI$leaf_temp[i_time,1]                                               ,val$calc$CI$leaf_temp[i_time,2]                                                  #,val$calc$sum$leaf_temp[i_time]
          ,val$calc$CI$air_temp[i_time,1]                                                ,val$calc$CI$air_temp[i_time,2]                                                   #,val$calc$sum$air_temp[i_time]
          ,val$calc$CI$light_in[i_time,1]                                                ,val$calc$CI$light_in[i_time,2]                                                   #,val$calc$sum$light_in[i_time]
          ,val$calc$CI$light_out[i_time,1]                                               ,val$calc$CI$light_out[i_time,2]                                                  #,val$calc$sum$light_out[i_time]
          ,val$calc$CI$reference_delta_e[i_time,1]                                       ,val$calc$CI$reference_delta_e[i_time,2]                                          #,val$calc$sum$reference_delta_e[i_time]
          ,val$calc$CI$chamber_delta_o[i_time,1]                                         ,val$calc$CI$chamber_delta_o[i_time,2]                                            #,val$calc$sum$chamber_delta_o[i_time]
          ,val$calc$CI$chamber_reference_delta_diff_CoCe[i_time,1]                       ,val$calc$CI$chamber_reference_delta_diff_CoCe[i_time,2]                          #,val$calc$sum$chamber_reference_delta_diff_CoCe[i_time]
          ,val$calc$CI$Delta_obs[i_time,1]                                               ,val$calc$CI$Delta_obs[i_time,2]                                                  #,val$calc$sum$Delta_obs[i_time]
          ,val$calc$CI$Delta_obs_permil[i_time,1]                                        ,val$calc$CI$Delta_obs_permil[i_time,2]                                           #,val$calc$sum$Delta_obs_permil[i_time]
          ,val$calc$CI$delta_13C_Assim[i_time,1]                                         ,val$calc$CI$delta_13C_Assim[i_time,2]                                            #,val$calc$sum$delta_13C_Assim[i_time]
          ,val$calc$CI$p[i_time,1]                                                       ,val$calc$CI$p[i_time,2]                                                          #,val$calc$sum$p[i_time]
          ,val$calc$CI$delta_13C_Resp[i_time,1]                                          ,val$calc$CI$delta_13C_Resp[i_time,2]                                             #,val$calc$sum$delta_13C_Resp[i_time]
          ,val$calc$CI$chamber_TotalCa[i_time,1]                                         ,val$calc$CI$chamber_TotalCa[i_time,2]                                            #,val$calc$sum$chamber_TotalCa[i_time]
          ,val$calc$CI$chamber_12Ca[i_time,1]                                            ,val$calc$CI$chamber_12Ca[i_time,2]                                               #,val$calc$sum$chamber_12Ca[i_time]
          ,val$calc$CI$chamber_13Ca[i_time,1]                                            ,val$calc$CI$chamber_13Ca[i_time,2]                                               #,val$calc$sum$chamber_13Ca[i_time]
          ,val$calc$CI$chamber_TotalCs[i_time,1]                                         ,val$calc$CI$chamber_TotalCs[i_time,2]                                            #,val$calc$sum$chamber_TotalCs[i_time]
          ,val$calc$CI$chamber_12Cs[i_time,1]                                            ,val$calc$CI$chamber_12Cs[i_time,2]                                               #,val$calc$sum$chamber_12Cs[i_time]
          ,val$calc$CI$chamber_13Cs[i_time,1]                                            ,val$calc$CI$chamber_13Cs[i_time,2]                                               #,val$calc$sum$chamber_13Cs[i_time]
          ,val$calc$CI$chamber_Totalpa[i_time,1]                                         ,val$calc$CI$chamber_Totalpa[i_time,2]                                            #,val$calc$sum$chamber_Totalpa[i_time]
          ,val$calc$CI$chamber_12pa[i_time,1]                                            ,val$calc$CI$chamber_12pa[i_time,2]                                               #,val$calc$sum$chamber_12pa[i_time]
          ,val$calc$CI$chamber_13pa[i_time,1]                                            ,val$calc$CI$chamber_13pa[i_time,2]                                               #,val$calc$sum$chamber_13pa[i_time]
          ,val$calc$CI$chamber_Totalps[i_time,1]                                         ,val$calc$CI$chamber_Totalps[i_time,2]                                            #,val$calc$sum$chamber_Totalps[i_time]
          ,val$calc$CI$chamber_12ps[i_time,1]                                            ,val$calc$CI$chamber_12ps[i_time,2]                                               #,val$calc$sum$chamber_12ps[i_time]
          ,val$calc$CI$chamber_13ps[i_time,1]                                            ,val$calc$CI$chamber_13ps[i_time,2]                                               #,val$calc$sum$chamber_13ps[i_time]
          ,val$calc$CI$chamber_Totalgbw[i_time,1]                                        ,val$calc$CI$chamber_Totalgbw[i_time,2]                                           #,val$calc$sum$chamber_Totalgbw[i_time]
          ,val$calc$CI$chamber_Totalgbc[i_time,1]                                        ,val$calc$CI$chamber_Totalgbc[i_time,2]                                           #,val$calc$sum$chamber_Totalgbc[i_time]
          ,val$calc$CI$chamber_12gbc[i_time,1]                                           ,val$calc$CI$chamber_12gbc[i_time,2]                                              #,val$calc$sum$chamber_12gbc[i_time]
          ,val$calc$CI$chamber_13gbc[i_time,1]                                           ,val$calc$CI$chamber_13gbc[i_time,2]                                              #,val$calc$sum$chamber_13gbc[i_time]
          ,val$calc$CI$chamber_Totalgsw[i_time,1]                                        ,val$calc$CI$chamber_Totalgsw[i_time,2]                                           #,val$calc$sum$chamber_Totalgsw[i_time]
          ,val$calc$CI$chamber_Totalgsc[i_time,1]                                        ,val$calc$CI$chamber_Totalgsc[i_time,2]                                           #,val$calc$sum$chamber_Totalgsc[i_time]
          ,val$calc$CI$chamber_12gsc[i_time,1]                                           ,val$calc$CI$chamber_12gsc[i_time,2]                                              #,val$calc$sum$chamber_12gsc[i_time]
          ,val$calc$CI$chamber_13gsc[i_time,1]                                           ,val$calc$CI$chamber_13gsc[i_time,2]                                              #,val$calc$sum$chamber_13gsc[i_time]
          ,val$calc$CI$chamber_Totalgtc[i_time,1]                                        ,val$calc$CI$chamber_Totalgtc[i_time,2]                                           #,val$calc$sum$chamber_Totalgtc[i_time]
          ,val$calc$CI$chamber_12gtc[i_time,1]                                           ,val$calc$CI$chamber_12gtc[i_time,2]                                              #,val$calc$sum$chamber_12gtc[i_time]
          ,val$calc$CI$chamber_13gtc[i_time,1]                                           ,val$calc$CI$chamber_13gtc[i_time,2]                                              #,val$calc$sum$chamber_13gtc[i_time]
          ,val$calc$CI$chamber_TotalCi[i_time,1]                                         ,val$calc$CI$chamber_TotalCi[i_time,2]                                            #,val$calc$sum$chamber_TotalCi[i_time]
          ,val$calc$CI$chamber_12Ci[i_time,1]                                            ,val$calc$CI$chamber_12Ci[i_time,2]                                               #,val$calc$sum$chamber_12Ci[i_time]
          ,val$calc$CI$chamber_13Ci[i_time,1]                                            ,val$calc$CI$chamber_13Ci[i_time,2]                                               #,val$calc$sum$chamber_13Ci[i_time]
          ,val$calc$CI$chamber_Totalpi[i_time,1]                                         ,val$calc$CI$chamber_Totalpi[i_time,2]                                            #,val$calc$sum$chamber_Totalpi[i_time]
          ,val$calc$CI$chamber_12pi[i_time,1]                                            ,val$calc$CI$chamber_12pi[i_time,2]                                               #,val$calc$sum$chamber_12pi[i_time]
          ,val$calc$CI$chamber_13pi[i_time,1]                                            ,val$calc$CI$chamber_13pi[i_time,2]                                               #,val$calc$sum$chamber_13pi[i_time]
          ,val$calc$CI$chamber_Totalpi_pa[i_time,1]                                      ,val$calc$CI$chamber_Totalpi_pa[i_time,2]                                         #,val$calc$sum$chamber_Totalpi_pa[i_time]
          ,val$calc$CI$chamber_Delta_i_simple_for_gm[i_time,1]                           ,val$calc$CI$chamber_Delta_i_simple_for_gm[i_time,2]                              #,val$calc$sum$chamber_Delta_i_simple_for_gm[i_time]
          ,val$calc$CI$chamber_Delta_i_simple_for_modeling[i_time,1]                     ,val$calc$CI$chamber_Delta_i_simple_for_modeling[i_time,2]                        #,val$calc$sum$chamber_Delta_i_simple_for_modeling[i_time]
          ,val$calc$CI$chamber_Delta_i_complex_for_gm[i_time,1]                          ,val$calc$CI$chamber_Delta_i_complex_for_gm[i_time,2]                             #,val$calc$sum$chamber_Delta_i_complex_for_gm[i_time]
          ,val$calc$CI$chamber_Delta_i_simple_for_gm_Delta_obs[i_time,1]                 ,val$calc$CI$chamber_Delta_i_simple_for_gm_Delta_obs[i_time,2]                    #,val$calc$sum$chamber_Delta_i_simple_for_gm_Delta_obs[i_time]
          ,val$calc$CI$chamber_Delta_i_complex_for_gm_Delta_obs[i_time,1]                ,val$calc$CI$chamber_Delta_i_complex_for_gm_Delta_obs[i_time,2]                   #,val$calc$sum$chamber_Delta_i_complex_for_gm_Delta_obs[i_time]
          ,val$calc$CI$chamber_Totalgm_point_simple[i_time,1]                            ,val$calc$CI$chamber_Totalgm_point_simple[i_time,2]                               #,val$calc$sum$chamber_Totalgm_point_simple[i_time]
          ,val$calc$CI$chamber_12gm_point_simple[i_time,1]                               ,val$calc$CI$chamber_12gm_point_simple[i_time,2]                                  #,val$calc$sum$chamber_12gm_point_simple[i_time]
          ,val$calc$CI$chamber_13gm_point_simple[i_time,1]                               ,val$calc$CI$chamber_13gm_point_simple[i_time,2]                                  #,val$calc$sum$chamber_13gm_point_simple[i_time]
          ,val$calc$CI$chamber_Totalgm_point_complex[i_time,1]                           ,val$calc$CI$chamber_Totalgm_point_complex[i_time,2]                              #,val$calc$sum$chamber_Totalgm_point_complex[i_time]
          ,val$calc$CI$chamber_Totalgm_to_use[i_time,1]                                  ,val$calc$CI$chamber_Totalgm_to_use[i_time,2]                                     #,val$calc$sum$chamber_Totalgm_to_use[i_time]
          ,val$calc$CI$chamber_Totalpc_using_gm[i_time,1]                                ,val$calc$CI$chamber_Totalpc_using_gm[i_time,2]                                   #,val$calc$sum$chamber_Totalpc_using_gm[i_time]
          ,val$calc$CI$chamber_12pc_using_gm[i_time,1]                                   ,val$calc$CI$chamber_12pc_using_gm[i_time,2]                                      #,val$calc$sum$chamber_12pc_using_gm[i_time]
          ,val$calc$CI$chamber_13pc_using_gm[i_time,1]                                   ,val$calc$CI$chamber_13pc_using_gm[i_time,2]                                      #,val$calc$sum$chamber_13pc_using_gm[i_time]
          ,val$calc$CI$chamber_Totalpc_using_simple_Delta_for_gm[i_time,1]               ,val$calc$CI$chamber_Totalpc_using_simple_Delta_for_gm[i_time,2]                  #,val$calc$sum$chamber_Totalpc_using_simple_Delta_for_gm[i_time]
          ,val$calc$CI$chamber_Totalpc_using_simple_Delta_for_modeling[i_time,1]         ,val$calc$CI$chamber_Totalpc_using_simple_Delta_for_modeling[i_time,2]            #,val$calc$sum$chamber_Totalpc_using_simple_Delta_for_modeling[i_time]
          ,val$calc$CI$chamber_Totalpc_using_complex_Delta_no_decarboxylation[i_time,1]  ,val$calc$CI$chamber_Totalpc_using_complex_Delta_no_decarboxylation[i_time,2]     #,val$calc$sum$chamber_Totalpc_using_complex_Delta_no_decarboxylation[i_time]
          ,val$calc$CI$chamber_Totalpc_using_complex_Delta_full_model[i_time,1]          ,val$calc$CI$chamber_Totalpc_using_complex_Delta_full_model[i_time,2]             #,val$calc$sum$chamber_Totalpc_using_complex_Delta_full_model[i_time]
          ,val$calc$CI$chamber_Totalpc_to_use[i_time,1]                                  ,val$calc$CI$chamber_Totalpc_to_use[i_time,2]                                     #,val$calc$sum$chamber_Totalpc_to_use[i_time]
          ,val$calc$CI$chamber_TotalCc[i_time,1]                                         ,val$calc$CI$chamber_TotalCc[i_time,2]                                            #,val$calc$sum$chamber_TotalCc[i_time]
          ,val$calc$CI$chamber_12Cc[i_time,1]                                            ,val$calc$CI$chamber_12Cc[i_time,2]                                               #,val$calc$sum$chamber_12Cc[i_time]
          ,val$calc$CI$chamber_13Cc[i_time,1]                                            ,val$calc$CI$chamber_13Cc[i_time,2]                                               #,val$calc$sum$chamber_13Cc[i_time]
          ,val$calc$CI$Licor_flow_uin[i_time,1]                                          ,val$calc$CI$Licor_flow_uin[i_time,2]                                             #,val$calc$sum$Licor_flow_uin[i_time]
          ,val$calc$CI$Licor_H2OR_xin[i_time,1]                                          ,val$calc$CI$Licor_H2OR_xin[i_time,2]                                             #,val$calc$sum$Licor_H2OR_xin[i_time]
          ,val$calc$CI$Licor_La[i_time,1]                                                ,val$calc$CI$Licor_La[i_time,2]                                                   #,val$calc$sum$Licor_La[i_time]
          ,val$calc$CI$Licor_Atm_press[i_time,1]                                         ,val$calc$CI$Licor_Atm_press[i_time,2]                                            #,"Licor_Atm_press[i_time]
          ,val$calc$CI$Licor_gsc_CI_L[i_time,1]                                          ,val$calc$CI$Licor_gsc.CI.U[i_time,2]                                             #,"Licor_gsc"
          ,val$calc$CI$Licor_Ci_CI_L[i_time,1]                                           ,val$calc$CI$Licor_Ci.CI.U[i_time,2]                                              #,"Licor_Ci"
          ,val$calc$CI$Licor_StmRat_CI_L[i_time,1]                                       ,val$calc$CI$Licor_StmRat.CI.U[i_time,2]                                          #,"Licor_StmRat"
          ,val$calc$CI$Licor_gbw_CI_L[i_time,1]                                          ,val$calc$CI$Licor_gbw.CI.U[i_time,2]                                             #,"Licor_gbw"
          ,val$calc$CI$Licor_temp_block_CI_L[i_time,1]                                   ,val$calc$CI$Licor_temp_block.CI.U[i_time,2]                                      #,"Licor_temp_block"
          ,val$calc$CI$Licor_Ce_CI_L[i_time,1]                                           ,val$calc$CI$Licor_Ce.CI.U[i_time,2]                                              #,"Licor_Ce"
          ,val$calc$CI$Licor_Co_CI_L[i_time,1]                                           ,val$calc$CI$Licor_Co.CI.U[i_time,2]                                              #,"Licor_Co"
          ,val$calc$CI$Licor_xout[i_time,1]                                              ,val$calc$CI$Licor_xout[i_time,2]                                                 #,"Licor_xout"
          ,val$calc$CI$Licor_rh_ref[i_time,1]                                            ,val$calc$CI$Licor_rh_ref[i_time,2]                                               #,"Licor_rh_ref"
          ,val$calc$CI$Licor_rh_sam_CI_L[i_time,1]                                       ,val$calc$CI$Licor_rh_sam.CI.U[i_time,2]                                          #,"Licor_rh_sam"
          ,val$calc$CI$Licor_CsMch_CI_L[i_time,1]                                        ,val$calc$CI$Licor_CsMch.CI.U[i_time,2]                                           #,"Licor_CsMch"
          ,val$calc$CI$Licor_HsMch_CI_L[i_time,1]                                        ,val$calc$CI$Licor_HsMch.CI.U[i_time,2]                                           #,"Licor_HsMch"
          ,val$calc$CI$Licor_StableF_CI_L[i_time,1]                                      ,val$calc$CI$Licor_StableF.CI.U[i_time,2]                                         #,"Licor_StableF"
          ,val$calc$CI$Licor_Status_CI_L[i_time,1]                                       ,val$calc$CI$Licor_Status.CI.U[i_time,2]                                          #,"Licor_Status"
          , sep=",")
      )
  }

  write(val$write$CI_Calc, file=output_CI_Calc_fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  # last file, last measurement of each cycle file, remove rows that are not last measurements
  n <- val$sum$TDL$n;
  sites <- val$sum$TDL$site;
  last_sites <- rep(0,n);
  for (i_site in 1:n) {
    # if last record, then last by definition
    if (i_site == n) { last_sites[i_site] <- i_site; }
    else {
      # if current different from next, then last
      if (sites[i_site] != sites[i_site+1]) { last_sites[i_site] <- i_site; }
    }
  }
  last_sites <- last_sites[last_sites > 0]; # include only last sites
  last_sites <- c(1,last_sites+1); # include header row

  val$write$CI_Calc_last <- val$write$CI_Calc[last_sites];

  write(val$write$CI_Calc, file=output_CI_Calc_fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)
  write(val$write$CI_Calc_last, file=output_CI_Calc_last_fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

