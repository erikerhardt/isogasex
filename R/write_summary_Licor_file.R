#' Title
#'
#' @param val
#' @param TDL.cycle
#' @param output.summary.Licor.fn
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
, TDL.cycle
###
, output.summary.Licor.fn
###
)
{

  val$write$summary_Licor <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site.name"
    ,"first.ind"
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
    ,"temp.air"
    ,"temp.leaf"
    ,"temp.block"
    ,"Ce"
    ,"Co"
    ,"xin"
    ,"xout"
    ,"rh.ref"
    ,"rh.sam"
    ,"uin"
    ,"par.int"
    ,"par.ext"
    ,"Atm.press"
    ,"CsMch"
    ,"HsMch"
    ,"StableF"
    ,"Status"
    ,"VpdA"
    ,"Ci.Ca"
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

  for (i.time in 1:val$sum$Licor$n) {
    val$write$summary_Licor <-
      rbind( val$write$summary_Licor
        ,paste(
           format(val$sum$Licor$time[i.time],format="%Y-%m-%d")
          ,format(val$sum$Licor$time[i.time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$sum$Licor$site[i.time]
          ,TDL.cycle$table.name[(TDL.cycle$table[,1] == val$sum$Licor$site[i.time])]
          ,val$sum$Licor$first.ind[i.time]
          ,val$sum$Licor$ind[i.time]
          ,val$sum$Licor$FTime[i.time]
          ,val$sum$Licor$A[i.time]
          ,val$sum$Licor$gsc[i.time]
          ,val$sum$Licor$Ci[i.time]
          ,val$sum$Licor$E[i.time]
          ,val$sum$Licor$VPD[i.time]
          ,val$sum$Licor$La[i.time]
          ,val$sum$Licor$StmRat[i.time]
          ,val$sum$Licor$gbw[i.time]
          ,val$sum$Licor$temp.air[i.time]
          ,val$sum$Licor$temp.leaf[i.time]
          ,val$sum$Licor$temp.block[i.time]
          ,val$sum$Licor$Ce[i.time]
          ,val$sum$Licor$Co[i.time]
          ,val$sum$Licor$xin[i.time]
          ,val$sum$Licor$xout[i.time]
          ,val$sum$Licor$rh.ref[i.time]
          ,val$sum$Licor$rh.sam[i.time]
          ,val$sum$Licor$uin[i.time]
          ,val$sum$Licor$par.int[i.time]
          ,val$sum$Licor$par.ext[i.time]
          ,val$sum$Licor$Atm.press[i.time]
          ,val$sum$Licor$CsMch[i.time]
          ,val$sum$Licor$HsMch[i.time]
          ,val$sum$Licor$StableF[i.time]
          ,val$sum$Licor$Status[i.time]
          ,val$sum$Licor$VpdA[i.time]
          ,val$sum$Licor$Ci.Ca[i.time]
          ,val$sum$Licor$pi[i.time]
          ,val$sum$Licor$uc_20_mV[i.time]
          ,val$sum$Licor$uc_21_mV[i.time]
          ,val$sum$Licor$U_S[i.time]
          ,val$sum$Licor$Trans[i.time]
          ,val$sum$Licor$CndCO2[i.time]
          ,val$sum$Licor$Ref_mV[i.time]
          ,val$sum$Licor$xTemp1[i.time]
          ,val$sum$Licor$xTemp2[i.time]
          , sep=",")
      )
  }

  write(val$write$summary_Licor, file=output.summary.Licor.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

### 9/7/2012 NEW VERSION, later ##
#write_summary_Licor_file <-
#function (val, TDL.cycle, output.summary.Licor.fn)
#{
#
#  # variable names
#  names.Licor <- names(val$sum$Licor);
#  # create a data.frame from the list so can refer to column names by names.Licor
#  val.sum.Licor <- as.data.frame(val$sum$Licor);
#
#  # get site.name from TDL file
#  temp.site.name <- NULL
#  for (i.time in 1:val$sum$Licor$n) {
#    temp.site.name <- rbind(temp.site.name
#      ,TDL.cycle$table.name[(TDL.cycle$table[,1] == val.sum.Licor[i.time,"site"])]
#      )
#  }
#
#  # include key columns
#  val$write$summary_Licor <-
#    cbind(
#       format(val.sum.Licor$time,format="%Y-%m-%d")
#      ,format(val.sum.Licor$time,format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
#      ,val.sum.Licor$site
#      ,temp.site.name
#      ,val.sum.Licor$first.ind
#      ,val.sum.Licor$ind
#      ,val.sum.Licor[,names.Licor[6:length(names.Licor)]]  # FTime : TChamAir
#    )
#
#  # column names
#  names(val$write$summary_Licor) <-
#    c("date"
#     ,"time"
#     ,"site"
#     ,"site.name"
#     ,"first.ind"
#     ,"ind"
#     ,names.Licor[6:length(names.Licor)])
#
#  # write data.frame
#  write.csv(val$write$summary_Licor, file=output.summary.Licor.fn, row.names=FALSE);
#
#  return( val$write );
#
#}


