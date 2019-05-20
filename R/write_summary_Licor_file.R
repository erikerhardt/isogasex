#' Title
#'
#' @param val
#' @param TDL_cycle
#' @param output_summary_Licor_fn
#'
#' @return
#' @export
#'
#' @examples
write_summary_Licor_file <-
function# write_output section: Licor file
### Write Licor summary values for all time points.
(val
###
, TDL_cycle
###
, output_summary_Licor_fn
###
)
{

  val$write$summary_Licor <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site_name"
    ,"first_ind"
    ,"ind"
    ,"FTime"
    ,"A"
    ,"gsc"
    ,"Ci"
    ,"E"
    ,"VPD"
    ,"La"
    ,"StmRat"
    ,"gbw"
    ,"temp_air"
    ,"temp_leaf"
    ,"temp_block"
    ,"Ce"
    ,"Co"
    ,"xin"
    ,"xout"
    ,"rh_ref"
    ,"rh_sam"
    ,"uin"
    ,"par_int"
    ,"par_ext"
    ,"Atm_press"
    ,"CsMch"
    ,"HsMch"
    ,"StableF"
    ,"Status"
    ,"VpdA"
    ,"Ci_Ca"
    ,"pi"
    ,"uc_20_mV"
    ,"uc_21_mV"
    ,"U_S"
    ,"Trans"
    ,"CndCO2"
    ,"Ref_mV"
    ,"xTemp1"
    ,"xTemp2"
    , sep=",");

  for (i_time in 1:val$sum$Licor$n) {
    val$write$summary_Licor <-
      rbind( val$write$summary_Licor
        ,paste(
           format(val$sum$Licor$time[i_time],format="%Y-%m-%d")
          ,format(val$sum$Licor$time[i_time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$sum$Licor$site[i_time]
          ,TDL_cycle$table_name[(TDL_cycle$table[,1] == val$sum$Licor$site[i_time])]
          ,val$sum$Licor$first_ind[i_time]
          ,val$sum$Licor$ind[i_time]
          ,val$sum$Licor$FTime[i_time]
          ,val$sum$Licor$A[i_time]
          ,val$sum$Licor$gsc[i_time]
          ,val$sum$Licor$Ci[i_time]
          ,val$sum$Licor$E[i_time]
          ,val$sum$Licor$VPD[i_time]
          ,val$sum$Licor$La[i_time]
          ,val$sum$Licor$StmRat[i_time]
          ,val$sum$Licor$gbw[i_time]
          ,val$sum$Licor$temp_air[i_time]
          ,val$sum$Licor$temp_leaf[i_time]
          ,val$sum$Licor$temp_block[i_time]
          ,val$sum$Licor$Ce[i_time]
          ,val$sum$Licor$Co[i_time]
          ,val$sum$Licor$xin[i_time]
          ,val$sum$Licor$xout[i_time]
          ,val$sum$Licor$rh_ref[i_time]
          ,val$sum$Licor$rh_sam[i_time]
          ,val$sum$Licor$uin[i_time]
          ,val$sum$Licor$par_int[i_time]
          ,val$sum$Licor$par_ext[i_time]
          ,val$sum$Licor$Atm_press[i_time]
          ,val$sum$Licor$CsMch[i_time]
          ,val$sum$Licor$HsMch[i_time]
          ,val$sum$Licor$StableF[i_time]
          ,val$sum$Licor$Status[i_time]
          ,val$sum$Licor$VpdA[i_time]
          ,val$sum$Licor$Ci_Ca[i_time]
          ,val$sum$Licor$pi[i_time]
          ,val$sum$Licor$uc_20_mV[i_time]
          ,val$sum$Licor$uc_21_mV[i_time]
          ,val$sum$Licor$U_S[i_time]
          ,val$sum$Licor$Trans[i_time]
          ,val$sum$Licor$CndCO2[i_time]
          ,val$sum$Licor$Ref_mV[i_time]
          ,val$sum$Licor$xTemp1[i_time]
          ,val$sum$Licor$xTemp2[i_time]
          , sep=",")
      )
  }

  write(val$write$summary_Licor, file=output_summary_Licor_fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

### 9/7/2012 NEW VERSION, later ##
#write_summary_Licor_file <-
#function (val, TDL_cycle, output_summary_Licor_fn)
#{
#
#  # variable names
#  names_Licor <- names(val$sum$Licor);
#  # create a data_frame from the list so can refer to column names by names_Licor
#  val_sum_Licor <- as.data_frame(val$sum$Licor);
#
#  # get site_name from TDL file
#  temp_site_name <- NULL
#  for (i_time in 1:val$sum$Licor$n) {
#    temp_site_name <- rbind(temp_site_name
#      ,TDL_cycle$table_name[(TDL_cycle$table[,1] == val_sum_Licor[i_time,"site"])]
#      )
#  }
#
#  # include key columns
#  val$write$summary_Licor <-
#    cbind(
#       format(val_sum_Licor$time,format="%Y-%m-%d")
#      ,format(val_sum_Licor$time,format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
#      ,val_sum_Licor$site
#      ,temp_site_name
#      ,val_sum_Licor$first_ind
#      ,val_sum_Licor$ind
#      ,val_sum_Licor[,names_Licor[6:length(names_Licor)]]  # FTime : TChamAir
#    )
#
#  # column names
#  names(val$write$summary_Licor) <-
#    c("date"
#     ,"time"
#     ,"site"
#     ,"site_name"
#     ,"first_ind"
#     ,"ind"
#     ,names_Licor[6:length(names_Licor)])
#
#  # write data_frame
#  write.csv(val$write$summary_Licor, file=output_summary_Licor_fn, row.names=FALSE);
#
#  return( val$write );
#
#}


