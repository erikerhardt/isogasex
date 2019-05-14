#' Title
#'
#' @param val
#' @param TDL.cycle
#' @param output.all.Calc.fn
#'
#' @return
#' @export
#'
#' @examples
write_all_Calc_file <-
function# write_output section
### Write all observed and calculated values for all time points.
(val
###
, TDL.cycle
###
, output.all.Calc.fn
###
)
{

  val$write$all_Calc <- as.character(matrix(NA,nrow=1+val$calc$all$n,ncol=1)); # init first

  val$write$all_Calc[1] <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site.name"
    #,"first.ind"
    ,"ind"
    ,"tank.hi.12"
    ,"tank.hi.13"
    ,"tank.low.12"
    ,"tank.low.13"
    ,"gain.12C"
    ,"gain.13C"
    ,"offset.12C"
    ,"offset.13C"
    ,"reference.12Ce"
    ,"reference.13Ce"
    ,"chamber.12Co"
    ,"chamber.13Co"
    ,"reference.TotalCe"
    ,"chamber.TotalCo"
    ,"chamber.reference.Total.diff.CeCo"
    ,"chamber.reference.12.diff.CeCo"
    ,"chamber.reference.13.diff.CeCo"
    ,"xi"
    # ,"flow.adjusted" # 9/5/2012
    ,"TDL.A.photosynthesis"
    ,"TDL.12A.photosynthesis"
    ,"TDL.13A.photosynthesis"
    ,"Licor.A.photosynthesis"
    ,"Delta.from.ratios.in.out"
    ,"Delta.from.A.ratio"
    ,"VPD"
    ,"E.transpiration"
    ,"leaf.temp"
    ,"air.temp"
    ,"light.in"
    ,"light.out"
    ,"reference.delta.e"
    ,"chamber.delta.o"
    ,"chamber.reference.delta.diff.CoCe"
    ,"Delta.obs"
    ,"Delta.obs.permil"
    ,"delta.13C.Assim"
    ,"p"
    ,"delta.13C.Resp"
    ,"chamber.TotalCa"
    ,"chamber.12Ca"
    ,"chamber.13Ca"
    ,"chamber.TotalCs"
    ,"chamber.12Cs"
    ,"chamber.13Cs"
    ,"chamber.Totalpa"
    ,"chamber.12pa"
    ,"chamber.13pa"
    ,"chamber.Totalps"
    ,"chamber.12ps"
    ,"chamber.13ps"
    ,"chamber.Totalgbw"
    ,"chamber.Totalgbc"
    ,"chamber.12gbc"
    ,"chamber.13gbc"
    ,"chamber.Totalgsw"
    ,"chamber.Totalgsc"
    ,"chamber.12gsc"
    ,"chamber.13gsc"
    ,"chamber.Totalgtc"
    ,"chamber.12gtc"
    ,"chamber.13gtc"
    ,"chamber.TotalCi"
    ,"chamber.12Ci"
    ,"chamber.13Ci"
    ,"chamber.Totalpi"
    ,"chamber.12pi"
    ,"chamber.13pi"
    ,"chamber.Totalpi_pa"
    ,"chamber.Delta.i.simple.for.gm"
    ,"chamber.Delta.i.simple.for.modeling"
    ,"chamber.Delta.i.complex.for.gm"
    ,"chamber.Delta.i.simple.for.gm_Delta.obs"
    ,"chamber.Delta.i.complex.for.gm_Delta.obs"
    ,"chamber.Totalgm.point.simple"
    ,"chamber.12gm.point.simple"
    ,"chamber.13gm.point.simple"
    ,"chamber.Totalgm.point.complex"
    ,"chamber.Totalgm.to.use"
    ,"chamber.Totalpc.using.gm"
    ,"chamber.12pc.using.gm"
    ,"chamber.13pc.using.gm"
    ,"chamber.Totalpc.using.simple.Delta.for.gm"
    ,"chamber.Totalpc.using.simple.Delta.for.modeling"
    ,"chamber.Totalpc.using.complex.Delta.no.decarboxylation"
    ,"chamber.Totalpc.using.complex.Delta.full.model"
    ,"chamber.Totalpc.to.use"
    ,"chamber.TotalCc"
    ,"chamber.12Cc"
    ,"chamber.13Cc"
    ,"Licor.flow.uin"
    ,"Licor.H2OR.xin"
    ,"Licor.La"
    ,"Licor.Atm.press"
    ,"Licor.gsc"
    ,"Licor.Ci"
    ,"Licor.StmRat"
    ,"Licor.gbw"
    ,"Licor.temp.block"
    ,"Licor.Ce"
    ,"Licor.Co"
    ,"Licor.xout"
    ,"Licor.rh.ref"
    ,"Licor.rh.sam"
    ,"Licor.CsMch"
    ,"Licor.HsMch"
    ,"Licor.StableF"
    ,"Licor.Status"
    , sep=",");

  for (i.time in 1:val$calc$all$n) {
    #val$write$all_Calc <-
    #  rbind( val$write$all_Calc
    val$write$all_Calc[i.time+1] <-
        paste(
           format(val$calc$all$time[i.time],format="%Y-%m-%d")
          ,format(val$calc$all$time[i.time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$calc$all$site[i.time]
          #,TDL.cycle$table.name[(TDL.cycle$table[,1] == val$calc$all$site[i.time])]
          # In the case of NAs, this vector was 4 NAs. So either use the single value, or a single NA.
          , { if (sum(is.na((TDL.cycle$table[,1] == val$calc$all$site[i.time]))) == 4) { NA }
            else { TDL.cycle$table.name[(TDL.cycle$table[,1] == val$calc$all$site[i.time])] } }
          #,val$sum$TDL$first.ind[i.time]
          ,val$calc$all$ind[i.time]
          ,val$calc$all$tank.hi.12[i.time]
          ,val$calc$all$tank.hi.13[i.time]
          ,val$calc$all$tank.low.12[i.time]
          ,val$calc$all$tank.low.13[i.time]
          ,val$calc$all$gain.12C[i.time]
          ,val$calc$all$gain.13C[i.time]
          ,val$calc$all$offset.12C[i.time]
          ,val$calc$all$offset.13C[i.time]
          ,val$calc$all$reference.12Ce[i.time]
          ,val$calc$all$reference.13Ce[i.time]
          ,val$calc$all$chamber.12Co[i.time]
          ,val$calc$all$chamber.13Co[i.time]
          ,val$calc$all$reference.TotalCe[i.time]
          ,val$calc$all$chamber.TotalCo[i.time]
          ,val$calc$all$chamber.reference.Total.diff.CeCo[i.time]
          ,val$calc$all$chamber.reference.12.diff.CeCo[i.time]
          ,val$calc$all$chamber.reference.13.diff.CeCo[i.time]
          ,val$calc$all$xi[i.time]
          #,val$calc$all$flow.adjusted[i.time] #  9/5/2012
          ,val$calc$all$TDL.A.photosynthesis[i.time]
          ,val$calc$all$TDL.12A.photosynthesis[i.time]
          ,val$calc$all$TDL.13A.photosynthesis[i.time]
          ,val$calc$all$Licor.A.photosynthesis[i.time]
          ,val$calc$all$Delta.from.ratios.in.out[i.time]
          ,val$calc$all$Delta.from.A.ratio[i.time]
          ,val$calc$all$VPD[i.time]
          ,val$calc$all$E.transpiration[i.time]
          ,val$calc$all$leaf.temp[i.time]
          ,val$calc$all$air.temp[i.time]
          ,val$calc$all$light.in[i.time]
          ,val$calc$all$light.out[i.time]
          ,val$calc$all$reference.delta.e[i.time]
          ,val$calc$all$chamber.delta.o[i.time]
          ,val$calc$all$chamber.reference.delta.diff.CoCe[i.time]
          ,val$calc$all$Delta.obs[i.time]
          ,val$calc$all$Delta.obs.permil[i.time]
          ,val$calc$all$delta.13C.Assim[i.time]
          ,val$calc$all$p[i.time]
          ,val$calc$all$delta.13C.Resp[i.time]
          ,val$calc$all$chamber.TotalCa[i.time]
          ,val$calc$all$chamber.12Ca[i.time]
          ,val$calc$all$chamber.13Ca[i.time]
          ,val$calc$all$chamber.TotalCs[i.time]
          ,val$calc$all$chamber.12Cs[i.time]
          ,val$calc$all$chamber.13Cs[i.time]
          ,val$calc$all$chamber.Totalpa[i.time]
          ,val$calc$all$chamber.12pa[i.time]
          ,val$calc$all$chamber.13pa[i.time]
          ,val$calc$all$chamber.Totalps[i.time]
          ,val$calc$all$chamber.12ps[i.time]
          ,val$calc$all$chamber.13ps[i.time]
          ,val$calc$all$chamber.Totalgbw[i.time]
          ,val$calc$all$chamber.Totalgbc[i.time]
          ,val$calc$all$chamber.12gbc[i.time]
          ,val$calc$all$chamber.13gbc[i.time]
          ,val$calc$all$chamber.Totalgsw[i.time]
          ,val$calc$all$chamber.Totalgsc[i.time]
          ,val$calc$all$chamber.12gsc[i.time]
          ,val$calc$all$chamber.13gsc[i.time]
          ,val$calc$all$chamber.Totalgtc[i.time]
          ,val$calc$all$chamber.12gtc[i.time]
          ,val$calc$all$chamber.13gtc[i.time]
          ,val$calc$all$chamber.TotalCi[i.time]
          ,val$calc$all$chamber.12Ci[i.time]
          ,val$calc$all$chamber.13Ci[i.time]
          ,val$calc$all$chamber.Totalpi[i.time]
          ,val$calc$all$chamber.12pi[i.time]
          ,val$calc$all$chamber.13pi[i.time]
          ,val$calc$all$chamber.Totalpi_pa[i.time]
          ,val$calc$all$chamber.Delta.i.simple.for.gm[i.time]
          ,val$calc$all$chamber.Delta.i.simple.for.modeling[i.time]
          ,val$calc$all$chamber.Delta.i.complex.for.gm[i.time]
          ,val$calc$all$chamber.Delta.i.simple.for.gm_Delta.obs[i.time]
          ,val$calc$all$chamber.Delta.i.complex.for.gm_Delta.obs[i.time]
          ,val$calc$all$chamber.Totalgm.point.simple[i.time]
          ,val$calc$all$chamber.12gm.point.simple[i.time]
          ,val$calc$all$chamber.13gm.point.simple[i.time]
          ,val$calc$all$chamber.Totalgm.point.complex[i.time]
          ,val$calc$all$chamber.Totalgm.to.use[i.time]
          ,val$calc$all$chamber.Totalpc.using.gm[i.time]
          ,val$calc$all$chamber.12pc.using.gm[i.time]
          ,val$calc$all$chamber.13pc.using.gm[i.time]
          ,val$calc$all$chamber.Totalpc.using.simple.Delta.for.gm[i.time]
          ,val$calc$all$chamber.Totalpc.using.simple.Delta.for.modeling[i.time]
          ,val$calc$all$chamber.Totalpc.using.complex.Delta.no.decarboxylation[i.time]
          ,val$calc$all$chamber.Totalpc.using.complex.Delta.full.model[i.time]
          ,val$calc$all$chamber.Totalpc.to.use[i.time]
          ,val$calc$all$chamber.TotalCc[i.time]
          ,val$calc$all$chamber.12Cc[i.time]
          ,val$calc$all$chamber.13Cc[i.time]
          ,val$calc$all$Licor.flow.uin[i.time]
          ,val$calc$all$Licor.H2OR.xin[i.time]
          ,val$calc$all$Licor.La[i.time]
          ,val$calc$all$Licor.Atm.press[i.time]
          ,val$calc$all$Licor.gsc[i.time]
          ,val$calc$all$Licor.Ci[i.time]
          ,val$calc$all$Licor.StmRat[i.time]
          ,val$calc$all$Licor.gbw[i.time]
          ,val$calc$all$Licor.temp.block[i.time]
          ,val$calc$all$Licor.Ce[i.time]
          ,val$calc$all$Licor.Co[i.time]
          ,val$calc$all$Licor.xout[i.time]
          ,val$calc$all$Licor.rh.ref[i.time]
          ,val$calc$all$Licor.rh.sam[i.time]
          ,val$calc$all$Licor.CsMch[i.time]
          ,val$calc$all$Licor.HsMch[i.time]
          ,val$calc$all$Licor.StableF[i.time]
          ,val$calc$all$Licor.Status[i.time]
          , sep=",")
      #)
  }

  write(val$write$all_Calc, file=output.all.Calc.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

