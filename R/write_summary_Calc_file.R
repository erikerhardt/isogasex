#' write output section
#'
#' Write summary values for all time points.
#'
#' @param val xxxPARAMxxx
#' @param TDL_cycle xxxPARAMxxx
#' @param output_summary_Calc_fn xxxPARAMxxx
#' @param output_summary_Calc_last_fn xxxPARAMxxx
#'
#' @return val$write xxxRETURNxxx
#'
write_summary_Calc_file <-
function# write_output section
### Write summary values for all time points.
(val
###
, TDL_cycle
###
, output_summary_Calc_fn
###
, output_summary_Calc_last_fn
###
)
{

  val$write$summary_Calc <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site_name"
    ,"first_ind"
    ,"ind"
    ,"tank_hi_12"
    ,"tank_hi_13"
    ,"tank_low_12"
    ,"tank_low_13"
    ,"gain_12C"
    ,"gain_13C"
    ,"offset_12C"
    ,"offset_13C"
    ,"reference_12Ce"
    ,"reference_13Ce"
    ,"chamber_12Co"
    ,"chamber_13Co"
    ,"reference_TotalCe"
    ,"chamber_TotalCo"
    ,"chamber_reference_Total_diff_CeCo"
    ,"chamber_reference_12_diff_CeCo"
    ,"chamber_reference_13_diff_CeCo"
    ,"xi"
    #,"flow_adjusted"    # 9/5/2012
    ,"TDL_A_photosynthesis"
    ,"TDL_12A_photosynthesis"
    ,"TDL_13A_photosynthesis"
    ,"Licor_A_photosynthesis"
    ,"Delta_from_ratios_in_out"
    ,"Delta_from_A_ratio"
    ,"VPD"
    ,"E_transpiration"
    ,"leaf_temp"
    ,"air_temp"
    ,"light_in"
    ,"light_out"
    ,"reference_delta_e"
    ,"chamber_delta_o"
    ,"chamber_reference_delta_diff_CoCe"
    ,"Delta_obs"
    ,"Delta_obs_permil"
    ,"delta_13C_Assim"
    ,"p"
    ,"delta_13C_Resp"
    ,"chamber_TotalCa"
    ,"chamber_12Ca"
    ,"chamber_13Ca"
    ,"chamber_TotalCs"
    ,"chamber_12Cs"
    ,"chamber_13Cs"
    ,"chamber_Totalpa"
    ,"chamber_12pa"
    ,"chamber_13pa"
    ,"chamber_Totalps"
    ,"chamber_12ps"
    ,"chamber_13ps"
    ,"chamber_Totalgbw"
    ,"chamber_Totalgbc"
    ,"chamber_12gbc"
    ,"chamber_13gbc"
    ,"chamber_Totalgsw"
    ,"chamber_Totalgsc"
    ,"chamber_12gsc"
    ,"chamber_13gsc"
    ,"chamber_Totalgtc"
    ,"chamber_12gtc"
    ,"chamber_13gtc"
    ,"chamber_TotalCi"
    ,"chamber_12Ci"
    ,"chamber_13Ci"
    ,"chamber_Totalpi"
    ,"chamber_12pi"
    ,"chamber_13pi"
    ,"chamber_Totalpi_pa"
    ,"chamber_Delta_i_simple_for_gm"
    ,"chamber_Delta_i_simple_for_modeling"
    ,"chamber_Delta_i_complex_for_gm"
    ,"chamber_Delta_i_simple_for_gm_Delta_obs"
    ,"chamber_Delta_i_complex_for_gm_Delta_obs"
    ,"chamber_Totalgm_point_simple"
    ,"chamber_12gm_point_simple"
    ,"chamber_13gm_point_simple"
    ,"chamber_Totalgm_point_complex"
    ,"chamber_Totalgm_to_use"
    ,"chamber_Totalpc_using_gm"
    ,"chamber_12pc_using_gm"
    ,"chamber_13pc_using_gm"
    ,"chamber_Totalpc_using_simple_Delta_for_gm"
    ,"chamber_Totalpc_using_simple_Delta_for_modeling"
    ,"chamber_Totalpc_using_complex_Delta_no_decarboxylation"
    ,"chamber_Totalpc_using_complex_Delta_full_model"
    ,"chamber_Totalpc_to_use"
    ,"chamber_TotalCc"
    ,"chamber_12Cc"
    ,"chamber_13Cc"
    ,"Licor_flow_uin"
    ,"Licor_H2OR_xin"
    ,"Licor_La"
    ,"Licor_Atm_press"
    ,"Licor_gsc"
    ,"Licor_Ci"
    ,"Licor_StmRat"
    ,"Licor_gbw"
    ,"Licor_temp_block"
    ,"Licor_Ce"
    ,"Licor_Co"
    ,"Licor_xout"
    ,"Licor_rh_ref"
    ,"Licor_rh_sam"
    ,"Licor_CsMch"
    ,"Licor_HsMch"
    ,"Licor_StableF"
    ,"Licor_Status"
    , sep=",");

  for (i_time in 1:val$sum$TDL$n) {
    val$write$summary_Calc <-
      rbind( val$write$summary_Calc
        ,paste(
           format(val$sum$TDL$time[i_time],format="%Y-%m-%d")
          ,format(val$sum$TDL$time[i_time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$sum$TDL$site[i_time]
          ,TDL_cycle$table_name[(TDL_cycle$table[,1] == val$sum$TDL$site[i_time])]
          ,val$sum$TDL$first_ind[i_time]
          ,val$sum$TDL$ind[i_time]
          ,val$calc$sum$tank_hi_12[i_time]
          ,val$calc$sum$tank_hi_13[i_time]
          ,val$calc$sum$tank_low_12[i_time]
          ,val$calc$sum$tank_low_13[i_time]
          ,val$calc$sum$gain_12C[i_time]
          ,val$calc$sum$gain_13C[i_time]
          ,val$calc$sum$offset_12C[i_time]
          ,val$calc$sum$offset_13C[i_time]
          ,val$calc$sum$reference_12Ce[i_time]
          ,val$calc$sum$reference_13Ce[i_time]
          ,val$calc$sum$chamber_12Co[i_time]
          ,val$calc$sum$chamber_13Co[i_time]
          ,val$calc$sum$reference_TotalCe[i_time]
          ,val$calc$sum$chamber_TotalCo[i_time]
          ,val$calc$sum$chamber_reference_Total_diff_CeCo[i_time]
          ,val$calc$sum$chamber_reference_12_diff_CeCo[i_time]
          ,val$calc$sum$chamber_reference_13_diff_CeCo[i_time]
          ,val$calc$sum$xi[i_time]
          #,val$calc$sum$flow_adjusted[i_time]    # 9/5/2012
          ,val$calc$sum$TDL_A_photosynthesis[i_time]
          ,val$calc$sum$TDL_12A_photosynthesis[i_time]
          ,val$calc$sum$TDL_13A_photosynthesis[i_time]
          ,val$calc$sum$Licor_A_photosynthesis[i_time]
          ,val$calc$sum$Delta_from_ratios_in_out[i_time]
          ,val$calc$sum$Delta_from_A_ratio[i_time]
          ,val$calc$sum$VPD[i_time]
          ,val$calc$sum$E_transpiration[i_time]
          ,val$calc$sum$leaf_temp[i_time]
          ,val$calc$sum$air_temp[i_time]
          ,val$calc$sum$light_in[i_time]
          ,val$calc$sum$light_out[i_time]
          ,val$calc$sum$reference_delta_e[i_time]
          ,val$calc$sum$chamber_delta_o[i_time]
          ,val$calc$sum$chamber_reference_delta_diff_CoCe[i_time]
          ,val$calc$sum$Delta_obs[i_time]
          ,val$calc$sum$Delta_obs_permil[i_time]
          ,val$calc$sum$delta_13C_Assim[i_time]
          ,val$calc$sum$p[i_time]
          ,val$calc$sum$delta_13C_Resp[i_time]
          ,val$calc$sum$chamber_TotalCa[i_time]
          ,val$calc$sum$chamber_12Ca[i_time]
          ,val$calc$sum$chamber_13Ca[i_time]
          ,val$calc$sum$chamber_TotalCs[i_time]
          ,val$calc$sum$chamber_12Cs[i_time]
          ,val$calc$sum$chamber_13Cs[i_time]
          ,val$calc$sum$chamber_Totalpa[i_time]
          ,val$calc$sum$chamber_12pa[i_time]
          ,val$calc$sum$chamber_13pa[i_time]
          ,val$calc$sum$chamber_Totalps[i_time]
          ,val$calc$sum$chamber_12ps[i_time]
          ,val$calc$sum$chamber_13ps[i_time]
          ,val$calc$sum$chamber_Totalgbw[i_time]
          ,val$calc$sum$chamber_Totalgbc[i_time]
          ,val$calc$sum$chamber_12gbc[i_time]
          ,val$calc$sum$chamber_13gbc[i_time]
          ,val$calc$sum$chamber_Totalgsw[i_time]
          ,val$calc$sum$chamber_Totalgsc[i_time]
          ,val$calc$sum$chamber_12gsc[i_time]
          ,val$calc$sum$chamber_13gsc[i_time]
          ,val$calc$sum$chamber_Totalgtc[i_time]
          ,val$calc$sum$chamber_12gtc[i_time]
          ,val$calc$sum$chamber_13gtc[i_time]
          ,val$calc$sum$chamber_TotalCi[i_time]
          ,val$calc$sum$chamber_12Ci[i_time]
          ,val$calc$sum$chamber_13Ci[i_time]
          ,val$calc$sum$chamber_Totalpi[i_time]
          ,val$calc$sum$chamber_12pi[i_time]
          ,val$calc$sum$chamber_13pi[i_time]
          ,val$calc$sum$chamber_Totalpi_pa[i_time]
          ,val$calc$sum$chamber_Delta_i_simple_for_gm[i_time]
          ,val$calc$sum$chamber_Delta_i_simple_for_modeling[i_time]
          ,val$calc$sum$chamber_Delta_i_complex_for_gm[i_time]
          ,val$calc$sum$chamber_Delta_i_simple_for_gm_Delta_obs[i_time]
          ,val$calc$sum$chamber_Delta_i_complex_for_gm_Delta_obs[i_time]
          ,val$calc$sum$chamber_Totalgm_point_simple[i_time]
          ,val$calc$sum$chamber_12gm_point_simple[i_time]
          ,val$calc$sum$chamber_13gm_point_simple[i_time]
          ,val$calc$sum$chamber_Totalgm_point_complex[i_time]
          ,val$calc$sum$chamber_Totalgm_to_use[i_time]
          ,val$calc$sum$chamber_Totalpc_using_gm[i_time]
          ,val$calc$sum$chamber_12pc_using_gm[i_time]
          ,val$calc$sum$chamber_13pc_using_gm[i_time]
          ,val$calc$sum$chamber_Totalpc_using_simple_Delta_for_gm[i_time]
          ,val$calc$sum$chamber_Totalpc_using_simple_Delta_for_modeling[i_time]
          ,val$calc$sum$chamber_Totalpc_using_complex_Delta_no_decarboxylation[i_time]
          ,val$calc$sum$chamber_Totalpc_using_complex_Delta_full_model[i_time]
          ,val$calc$sum$chamber_Totalpc_to_use[i_time]
          ,val$calc$sum$chamber_TotalCc[i_time]
          ,val$calc$sum$chamber_12Cc[i_time]
          ,val$calc$sum$chamber_13Cc[i_time]
          ,val$calc$sum$Licor_flow_uin[i_time]
          ,val$calc$sum$Licor_H2OR_xin[i_time]
          ,val$calc$sum$Licor_La[i_time]
          ,val$calc$sum$Licor_Atm_press[i_time]
          ,val$calc$sum$Licor_gsc[i_time]
          ,val$calc$sum$Licor_Ci[i_time]
          ,val$calc$sum$Licor_StmRat[i_time]
          ,val$calc$sum$Licor_gbw[i_time]
          ,val$calc$sum$Licor_temp_block[i_time]
          ,val$calc$sum$Licor_Ce[i_time]
          ,val$calc$sum$Licor_Co[i_time]
          ,val$calc$sum$Licor_xout[i_time]
          ,val$calc$sum$Licor_rh_ref[i_time]
          ,val$calc$sum$Licor_rh_sam[i_time]
          ,val$calc$sum$Licor_CsMch[i_time]
          ,val$calc$sum$Licor_HsMch[i_time]
          ,val$calc$sum$Licor_StableF[i_time]
          ,val$calc$sum$Licor_Status[i_time]
          , sep=",")
      )
  }

  write(val$write$summary_Calc, file=output_summary_Calc_fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

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

  val$write$summary_Calc_last <- val$write$summary_Calc[last_sites];

  write(val$write$summary_Calc_last, file=output_summary_Calc_last_fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

