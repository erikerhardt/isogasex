#' write output section: Licor file
#'
#' Write observed Licor values with BS CIs for all time points.
#'
#' @param val
#' @param TDL_cycle
#' @param output_CI_Licor_fn
#'
#' @return val$write
#' @export
#'
#' @examples
write_CI_Licor_file <-
function# write_output section: Licor file
### Write observed Licor values with BS CIs for all time points.
(val
###
, TDL_cycle
###
, output_CI_Licor_fn
###
)
{

  warning("CI for Licor may not work -- not tested...")

  val$write$CI_Licor <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site_name"
    ,"first_ind"
    ,"ind"
    ,"FTime_CI_L"       ,"FTime.CI.U"        #,"FTime"
    ,"A_CI_L"           ,"A.CI.U"            #,"A"
    ,"gsc_CI_L"         ,"gsc.CI.U"          #,"gsc"
    ,"Ci_CI_L"          ,"Ci.CI.U"           #,"Ci"
    ,"E_CI_L"           ,"E.CI.U"            #,"E"
    ,"VPD_CI_L"         ,"VPD.CI.U"          #,"VPD"
    ,"La_CI_L"          ,"La.CI.U"           #,"La"
    ,"StmRat_CI_L"      ,"StmRat.CI.U"       #,"StmRat"
    ,"gbw_CI_L"         ,"gbw.CI.U"          #,"gbw"
    ,"temp_air_CI_L"    ,"temp_air.CI.U"     #,"temp_air"
    ,"temp_leaf_CI_L"   ,"temp_leaf.CI.U"    #,"temp_leaf"
    ,"temp_block_CI_L"  ,"temp_block.CI.U"   #,"temp_block"
    ,"Ce_CI_L"          ,"Ce.CI.U"           #,"Ce"
    ,"Co_CI_L"          ,"Co.CI.U"           #,"Co"
    ,"xin_CI_L"         ,"xin.CI.U"          #,"xin"
    ,"xout_CI_L"        ,"xout.CI.U"         #,"xout"
    ,"rh_ref"           ,"rh_ref"            #,"rh_ref"
    ,"rh_sam_CI_L"      ,"rh_sam.CI.U"       #,"rh_sam"
    ,"uin_CI_L"         ,"uin.CI.U"          #,"uin"
    ,"par_int_CI_L"     ,"par_int.CI.U"      #,"par_int"
    ,"par_ext_CI_L"     ,"par_ext.CI.U"      #,"par_ext"
    ,"Atm_press_CI_L"   ,"Atm_press.CI.U"    #,"Atm_press"
    ,"CsMch_CI_L"       ,"CsMch.CI.U"        #,"CsMch"
    ,"HsMch_CI_L"       ,"HsMch.CI.U"        #,"HsMch"
    ,"StableF_CI_L"     ,"StableF.CI.U"      #,"StableF"
    ,"Status_CI_L"      ,"Status.CI.U"       #,"Status"
    ,"VpdA_CI_L"        ,"VpdA.CI.U"         #,"VpdA"
    ,"Ci_Ca_CI_L"       ,"Ci_Ca.CI.U"        #,"Ci_Ca"
    ,"pi_CI_L"          ,"pi.CI.U"           #,"pi"
    ,"uc_20_mV_CI_L"    ,"uc_20_mV.CI.U"     #,"uc_20_mV"
    ,"uc_21_mV_CI_L"    ,"uc_21_mV.CI.U"     #,"uc_21_mV"
    ,"U_S_CI_L"         ,"U_S.CI.U"          #,"U_S"
    ,"Trans_CI_L"       ,"Trans.CI.U"        #,"Trans"
    ,"CndCO2_CI_L"      ,"CndCO2.CI.U"       #,"CndCO2"
    ,"Ref_mV_CI_L"      ,"Ref_mV.CI.U"       #,"Ref_mV"
    ,"xTemp1_CI_L"      ,"xTemp1.CI.U"       #,"xTemp1"
    ,"xTemp2_CI_L"      ,"xTemp2.CI.U"       #,"xTemp2"
    , sep=",");

  for (i_time in 1:val$sum$Licor$n) {
    val$write$CI_Licor <-
      rbind( val$write$CI_Licor
        ,paste(
           format(val$sum$Licor$time[i_time],format="%Y-%m-%d")
          ,val$sum$Licor$site[i_time]
          ,TDL_cycle$table_name[(TDL_cycle$table[,1] == val$sum$Licor$site[i_time])]
          ,val$sum$Licor$first_ind[i_time]
          ,val$sum$Licor$ind[i_time]
          ,val$CI$Licor$FTime[i_time,1]      ,val$CI$Licor$FTime[i_time,2]                    #,val$sum$Licor$FTime[i_time]
          ,val$CI$Licor$A[i_time,1]          ,val$CI$Licor$A[i_time,2]                        #,val$sum$Licor$A[i_time]
          ,val$CI$Licor$gsc[i_time,1]        ,val$CI$Licor$gsc[i_time,2]                      #,val$sum$Licor$gsc[i_time]
          ,val$CI$Licor$Ci[i_time,1]         ,val$CI$Licor$Ci[i_time,2]                       #,val$sum$Licor$Ci[i_time]
          ,val$CI$Licor$E[i_time,1]          ,val$CI$Licor$E[i_time,2]                        #,val$sum$Licor$E[i_time]
          ,val$CI$Licor$VPD[i_time,1]        ,val$CI$Licor$VPD[i_time,2]                      #,val$sum$Licor$VPD[i_time]
          ,val$CI$Licor$La[i_time,1]         ,val$CI$Licor$La[i_time,2]                       #,val$sum$Licor$La[i_time]
          ,val$CI$Licor$StmRat[i_time,1]     ,val$CI$Licor$StmRat[i_time,2]                   #,val$sum$Licor$StmRat[i_time]
          ,val$CI$Licor$gbw[i_time,1]        ,val$CI$Licor$gbw[i_time,2]                      #,val$sum$Licor$gbw[i_time]
          ,val$CI$Licor$temp_air[i_time,1]   ,val$CI$Licor$temp_air[i_time,2]                 #,val$sum$Licor$temp_air[i_time]
          ,val$CI$Licor$temp_leaf[i_time,1]  ,val$CI$Licor$temp_leaf[i_time,2]                #,val$sum$Licor$temp_leaf[i_time]
          ,val$CI$Licor$temp_block[i_time,1] ,val$CI$Licor$temp_block[i_time,2]               #,val$sum$Licor$temp_block[i_time]
          ,val$CI$Licor$Ce[i_time,1]         ,val$CI$Licor$Ce[i_time,2]                       #,val$sum$Licor$Ce[i_time]
          ,val$CI$Licor$Co[i_time,1]         ,val$CI$Licor$Co[i_time,2]                       #,val$sum$Licor$Co[i_time]
          ,val$CI$Licor$xin[i_time,1]        ,val$CI$Licor$xin[i_time,2]                      #,val$sum$Licor$xin[i_time]
          ,val$CI$Licor$xout[i_time,1]       ,val$CI$Licor$xout[i_time,2]                     #,val$sum$Licor$xout[i_time]
          ,val$CI$Licor$rh_ref[i_time,1]     ,val$CI$Licor$rh_ref[i_time,2]                   #,val$sum$Licor$rh_ref[i_time]
          ,val$CI$Licor$rh_sam[i_time,1]     ,val$CI$Licor$rh_sam[i_time,2]                   #,val$sum$Licor$rh_sam[i_time]
          ,val$CI$Licor$uin[i_time,1]        ,val$CI$Licor$uin[i_time,2]                      #,val$sum$Licor$uin[i_time]
          ,val$CI$Licor$par_int[i_time,1]    ,val$CI$Licor$par_int[i_time,2]                  #,val$sum$Licor$par_int[i_time]
          ,val$CI$Licor$par_ext[i_time,1]    ,val$CI$Licor$par_ext[i_time,2]                  #,val$sum$Licor$par_ext[i_time]
          ,val$CI$Licor$Atm_press[i_time,1]  ,val$CI$Licor$Atm_press[i_time,2]                #,val$sum$Licor$Atm_press[i_time]
          ,val$CI$Licor$CsMch[i_time,1]      ,val$CI$Licor$CsMch[i_time,2]                    #,val$sum$Licor$CsMch[i_time]
          ,val$CI$Licor$HsMch[i_time,1]      ,val$CI$Licor$HsMch[i_time,2]                    #,val$sum$Licor$HsMch[i_time]
          ,val$CI$Licor$StableF[i_time,1]    ,val$CI$Licor$StableF[i_time,2]                  #,val$sum$Licor$StableF[i_time]
          ,val$CI$Licor$Status[i_time,1]     ,val$CI$Licor$Status[i_time,2]                   #,val$sum$Licor$Status[i_time]
          ,val$CI$Licor$VpdA[i_time,1]       ,val$CI$Licor$VpdA[i_time,2]                     #,val$sum$Licor$VpdA[i_time]
          ,val$CI$Licor$Ci_Ca[i_time,1]      ,val$CI$Licor$Ci_Ca[i_time,2]                    #,val$sum$Licor$Ci_Ca[i_time]
          ,val$CI$Licor$pi[i_time,1]         ,val$CI$Licor$pi[i_time,2]                       #,val$sum$Licor$pi[i_time]
          ,val$CI$Licor$uc_20_mV[i_time,1]   ,val$CI$Licor$uc_20_mV[i_time,2]                 #,val$sum$Licor$uc_20_mV[i_time]
          ,val$CI$Licor$uc_21_mV[i_time,1]   ,val$CI$Licor$uc_21_mV[i_time,2]                 #,val$sum$Licor$uc_21_mV[i_time]
          ,val$CI$Licor$U_S[i_time,1]        ,val$CI$Licor$U_S[i_time,2]                      #,val$sum$Licor$U_S[i_time]
          ,val$CI$Licor$Trans[i_time,1]      ,val$CI$Licor$Trans[i_time,2]                    #,val$sum$Licor$Trans[i_time]
          ,val$CI$Licor$CndCO2[i_time,1]     ,val$CI$Licor$CndCO2[i_time,2]                   #,val$sum$Licor$CndCO2[i_time]
          ,val$CI$Licor$Ref_mV[i_time,1]     ,val$CI$Licor$Ref_mV[i_time,2]                   #,val$sum$Licor$Ref_mV[i_time]
          ,val$CI$Licor$xTemp1[i_time,1]     ,val$CI$Licor$xTemp1[i_time,2]                   #,val$sum$Licor$xTemp1[i_time]
          ,val$CI$Licor$xTemp2[i_time,1]     ,val$CI$Licor$xTemp2[i_time,2]                   #,val$sum$Licor$xTemp2[i_time]
          , sep=",")
      )
  }

  write(val$write$CI_Licor, file=output_CI_Licor_fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

