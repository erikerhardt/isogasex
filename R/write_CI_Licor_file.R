#' Title
#'
#' @param val
#' @param TDL.cycle
#' @param output.CI.Licor.fn
#'
#' @return
#' @export
#'
#' @examples
write_CI_Licor_file <-
function# write_output section: Licor file
### Write observed Licor values with BS CIs for all time points.
(val
###
, TDL.cycle
###
, output.CI.Licor.fn
###
)
{

  warning("CI for Licor may not work -- not tested...")

  val$write$CI_Licor <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site.name"
    ,"first.ind"
    ,"ind"
    ,"FTime.CI.L"       ,"FTime.CI.U"        #,"FTime"
    ,"A.CI.L"           ,"A.CI.U"            #,"A"
    ,"gsc.CI.L"         ,"gsc.CI.U"          #,"gsc"
    ,"Ci.CI.L"          ,"Ci.CI.U"           #,"Ci"
    ,"E.CI.L"           ,"E.CI.U"            #,"E"
    ,"VPD.CI.L"         ,"VPD.CI.U"          #,"VPD"
    ,"La.CI.L"          ,"La.CI.U"           #,"La"
    ,"StmRat.CI.L"      ,"StmRat.CI.U"       #,"StmRat"
    ,"gbw.CI.L"         ,"gbw.CI.U"          #,"gbw"
    ,"temp.air.CI.L"    ,"temp.air.CI.U"     #,"temp.air"
    ,"temp.leaf.CI.L"   ,"temp.leaf.CI.U"    #,"temp.leaf"
    ,"temp.block.CI.L"  ,"temp.block.CI.U"   #,"temp.block"
    ,"Ce.CI.L"          ,"Ce.CI.U"           #,"Ce"
    ,"Co.CI.L"          ,"Co.CI.U"           #,"Co"
    ,"xin.CI.L"         ,"xin.CI.U"          #,"xin"
    ,"xout.CI.L"        ,"xout.CI.U"         #,"xout"
    ,"rh.ref"           ,"rh.ref"            #,"rh.ref"
    ,"rh.sam.CI.L"      ,"rh.sam.CI.U"       #,"rh.sam"
    ,"uin.CI.L"         ,"uin.CI.U"          #,"uin"
    ,"par.int.CI.L"     ,"par.int.CI.U"      #,"par.int"
    ,"par.ext.CI.L"     ,"par.ext.CI.U"      #,"par.ext"
    ,"Atm.press.CI.L"   ,"Atm.press.CI.U"    #,"Atm.press"
    ,"CsMch.CI.L"       ,"CsMch.CI.U"        #,"CsMch"
    ,"HsMch.CI.L"       ,"HsMch.CI.U"        #,"HsMch"
    ,"StableF.CI.L"     ,"StableF.CI.U"      #,"StableF"
    ,"Status.CI.L"      ,"Status.CI.U"       #,"Status"
    ,"VpdA.CI.L"        ,"VpdA.CI.U"         #,"VpdA"
    ,"Ci.Ca.CI.L"       ,"Ci.Ca.CI.U"        #,"Ci.Ca"
    ,"pi.CI.L"          ,"pi.CI.U"           #,"pi"
    ,"uc_20_mV.CI.L"    ,"uc_20_mV.CI.U"     #,"uc_20_mV"
    ,"uc_21_mV.CI.L"    ,"uc_21_mV.CI.U"     #,"uc_21_mV"
    ,"U_S.CI.L"         ,"U_S.CI.U"          #,"U_S"
    ,"Trans.CI.L"       ,"Trans.CI.U"        #,"Trans"
    ,"CndCO2.CI.L"      ,"CndCO2.CI.U"       #,"CndCO2"
    ,"Ref_mV.CI.L"      ,"Ref_mV.CI.U"       #,"Ref_mV"
    ,"xTemp1.CI.L"      ,"xTemp1.CI.U"       #,"xTemp1"
    ,"xTemp2.CI.L"      ,"xTemp2.CI.U"       #,"xTemp2"
    , sep=",");

  for (i.time in 1:val$sum$Licor$n) {
    val$write$CI_Licor <-
      rbind( val$write$CI_Licor
        ,paste(
           format(val$sum$Licor$time[i.time],format="%Y-%m-%d")
          ,format(val$sum$Licor$time[i.time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$sum$Licor$site[i.time]
          ,TDL.cycle$table.name[(TDL.cycle$table[,1] == val$sum$Licor$site[i.time])]
          ,val$sum$Licor$first.ind[i.time]
          ,val$sum$Licor$ind[i.time]
          ,val$CI$Licor$FTime[i.time,1]      ,val$CI$Licor$FTime[i.time,2]                    #,val$sum$Licor$FTime[i.time]
          ,val$CI$Licor$A[i.time,1]          ,val$CI$Licor$A[i.time,2]                        #,val$sum$Licor$A[i.time]
          ,val$CI$Licor$gsc[i.time,1]        ,val$CI$Licor$gsc[i.time,2]                      #,val$sum$Licor$gsc[i.time]
          ,val$CI$Licor$Ci[i.time,1]         ,val$CI$Licor$Ci[i.time,2]                       #,val$sum$Licor$Ci[i.time]
          ,val$CI$Licor$E[i.time,1]          ,val$CI$Licor$E[i.time,2]                        #,val$sum$Licor$E[i.time]
          ,val$CI$Licor$VPD[i.time,1]        ,val$CI$Licor$VPD[i.time,2]                      #,val$sum$Licor$VPD[i.time]
          ,val$CI$Licor$La[i.time,1]         ,val$CI$Licor$La[i.time,2]                       #,val$sum$Licor$La[i.time]
          ,val$CI$Licor$StmRat[i.time,1]     ,val$CI$Licor$StmRat[i.time,2]                   #,val$sum$Licor$StmRat[i.time]
          ,val$CI$Licor$gbw[i.time,1]        ,val$CI$Licor$gbw[i.time,2]                      #,val$sum$Licor$gbw[i.time]
          ,val$CI$Licor$temp.air[i.time,1]   ,val$CI$Licor$temp.air[i.time,2]                 #,val$sum$Licor$temp.air[i.time]
          ,val$CI$Licor$temp.leaf[i.time,1]  ,val$CI$Licor$temp.leaf[i.time,2]                #,val$sum$Licor$temp.leaf[i.time]
          ,val$CI$Licor$temp.block[i.time,1] ,val$CI$Licor$temp.block[i.time,2]               #,val$sum$Licor$temp.block[i.time]
          ,val$CI$Licor$Ce[i.time,1]         ,val$CI$Licor$Ce[i.time,2]                       #,val$sum$Licor$Ce[i.time]
          ,val$CI$Licor$Co[i.time,1]         ,val$CI$Licor$Co[i.time,2]                       #,val$sum$Licor$Co[i.time]
          ,val$CI$Licor$xin[i.time,1]        ,val$CI$Licor$xin[i.time,2]                      #,val$sum$Licor$xin[i.time]
          ,val$CI$Licor$xout[i.time,1]       ,val$CI$Licor$xout[i.time,2]                     #,val$sum$Licor$xout[i.time]
          ,val$CI$Licor$rh.ref[i.time,1]     ,val$CI$Licor$rh.ref[i.time,2]                   #,val$sum$Licor$rh.ref[i.time]
          ,val$CI$Licor$rh.sam[i.time,1]     ,val$CI$Licor$rh.sam[i.time,2]                   #,val$sum$Licor$rh.sam[i.time]
          ,val$CI$Licor$uin[i.time,1]        ,val$CI$Licor$uin[i.time,2]                      #,val$sum$Licor$uin[i.time]
          ,val$CI$Licor$par.int[i.time,1]    ,val$CI$Licor$par.int[i.time,2]                  #,val$sum$Licor$par.int[i.time]
          ,val$CI$Licor$par.ext[i.time,1]    ,val$CI$Licor$par.ext[i.time,2]                  #,val$sum$Licor$par.ext[i.time]
          ,val$CI$Licor$Atm.press[i.time,1]  ,val$CI$Licor$Atm.press[i.time,2]                #,val$sum$Licor$Atm.press[i.time]
          ,val$CI$Licor$CsMch[i.time,1]      ,val$CI$Licor$CsMch[i.time,2]                    #,val$sum$Licor$CsMch[i.time]
          ,val$CI$Licor$HsMch[i.time,1]      ,val$CI$Licor$HsMch[i.time,2]                    #,val$sum$Licor$HsMch[i.time]
          ,val$CI$Licor$StableF[i.time,1]    ,val$CI$Licor$StableF[i.time,2]                  #,val$sum$Licor$StableF[i.time]
          ,val$CI$Licor$Status[i.time,1]     ,val$CI$Licor$Status[i.time,2]                   #,val$sum$Licor$Status[i.time]
          ,val$CI$Licor$VpdA[i.time,1]       ,val$CI$Licor$VpdA[i.time,2]                     #,val$sum$Licor$VpdA[i.time]
          ,val$CI$Licor$Ci.Ca[i.time,1]      ,val$CI$Licor$Ci.Ca[i.time,2]                    #,val$sum$Licor$Ci.Ca[i.time]
          ,val$CI$Licor$pi[i.time,1]         ,val$CI$Licor$pi[i.time,2]                       #,val$sum$Licor$pi[i.time]
          ,val$CI$Licor$uc_20_mV[i.time,1]   ,val$CI$Licor$uc_20_mV[i.time,2]                 #,val$sum$Licor$uc_20_mV[i.time]
          ,val$CI$Licor$uc_21_mV[i.time,1]   ,val$CI$Licor$uc_21_mV[i.time,2]                 #,val$sum$Licor$uc_21_mV[i.time]
          ,val$CI$Licor$U_S[i.time,1]        ,val$CI$Licor$U_S[i.time,2]                      #,val$sum$Licor$U_S[i.time]
          ,val$CI$Licor$Trans[i.time,1]      ,val$CI$Licor$Trans[i.time,2]                    #,val$sum$Licor$Trans[i.time]
          ,val$CI$Licor$CndCO2[i.time,1]     ,val$CI$Licor$CndCO2[i.time,2]                   #,val$sum$Licor$CndCO2[i.time]
          ,val$CI$Licor$Ref_mV[i.time,1]     ,val$CI$Licor$Ref_mV[i.time,2]                   #,val$sum$Licor$Ref_mV[i.time]
          ,val$CI$Licor$xTemp1[i.time,1]     ,val$CI$Licor$xTemp1[i.time,2]                   #,val$sum$Licor$xTemp1[i.time]
          ,val$CI$Licor$xTemp2[i.time,1]     ,val$CI$Licor$xTemp2[i.time,2]                   #,val$sum$Licor$xTemp2[i.time]
          , sep=",")
      )
  }

  write(val$write$CI_Licor, file=output.CI.Licor.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

