#' Title
#'
#' @param val
#' @param TDL.cycle
#' @param output.summary.Calc.fn
#' @param output.summary.Calc.last.fn
#'
#' @return
#' @export
#'
#' @examples
write_summary_Calc_file <-
function# write_output section
### Write summary values for all time points.
(val
###
, TDL.cycle
###
, output.summary.Calc.fn
###
, output.summary.Calc.last.fn
###
)
{

  val$write$summary_Calc <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site.name"
    ,"first.ind"
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
    #,"flow.adjusted"    # 9/5/2012
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

  for (i.time in 1:val$sum$TDL$n) {
    val$write$summary_Calc <-
      rbind( val$write$summary_Calc
        ,paste(
           format(val$sum$TDL$time[i.time],format="%Y-%m-%d")
          ,format(val$sum$TDL$time[i.time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$sum$TDL$site[i.time]
          ,TDL.cycle$table.name[(TDL.cycle$table[,1] == val$sum$TDL$site[i.time])]
          ,val$sum$TDL$first.ind[i.time]
          ,val$sum$TDL$ind[i.time]
          ,val$calc$sum$tank.hi.12[i.time]
          ,val$calc$sum$tank.hi.13[i.time]
          ,val$calc$sum$tank.low.12[i.time]
          ,val$calc$sum$tank.low.13[i.time]
          ,val$calc$sum$gain.12C[i.time]
          ,val$calc$sum$gain.13C[i.time]
          ,val$calc$sum$offset.12C[i.time]
          ,val$calc$sum$offset.13C[i.time]
          ,val$calc$sum$reference.12Ce[i.time]
          ,val$calc$sum$reference.13Ce[i.time]
          ,val$calc$sum$chamber.12Co[i.time]
          ,val$calc$sum$chamber.13Co[i.time]
          ,val$calc$sum$reference.TotalCe[i.time]
          ,val$calc$sum$chamber.TotalCo[i.time]
          ,val$calc$sum$chamber.reference.Total.diff.CeCo[i.time]
          ,val$calc$sum$chamber.reference.12.diff.CeCo[i.time]
          ,val$calc$sum$chamber.reference.13.diff.CeCo[i.time]
          ,val$calc$sum$xi[i.time]
          #,val$calc$sum$flow.adjusted[i.time]    # 9/5/2012
          ,val$calc$sum$TDL.A.photosynthesis[i.time]
          ,val$calc$sum$TDL.12A.photosynthesis[i.time]
          ,val$calc$sum$TDL.13A.photosynthesis[i.time]
          ,val$calc$sum$Licor.A.photosynthesis[i.time]
          ,val$calc$sum$Delta.from.ratios.in.out[i.time]
          ,val$calc$sum$Delta.from.A.ratio[i.time]
          ,val$calc$sum$VPD[i.time]
          ,val$calc$sum$E.transpiration[i.time]
          ,val$calc$sum$leaf.temp[i.time]
          ,val$calc$sum$air.temp[i.time]
          ,val$calc$sum$light.in[i.time]
          ,val$calc$sum$light.out[i.time]
          ,val$calc$sum$reference.delta.e[i.time]
          ,val$calc$sum$chamber.delta.o[i.time]
          ,val$calc$sum$chamber.reference.delta.diff.CoCe[i.time]
          ,val$calc$sum$Delta.obs[i.time]
          ,val$calc$sum$Delta.obs.permil[i.time]
          ,val$calc$sum$delta.13C.Assim[i.time]
          ,val$calc$sum$p[i.time]
          ,val$calc$sum$delta.13C.Resp[i.time]
          ,val$calc$sum$chamber.TotalCa[i.time]
          ,val$calc$sum$chamber.12Ca[i.time]
          ,val$calc$sum$chamber.13Ca[i.time]
          ,val$calc$sum$chamber.TotalCs[i.time]
          ,val$calc$sum$chamber.12Cs[i.time]
          ,val$calc$sum$chamber.13Cs[i.time]
          ,val$calc$sum$chamber.Totalpa[i.time]
          ,val$calc$sum$chamber.12pa[i.time]
          ,val$calc$sum$chamber.13pa[i.time]
          ,val$calc$sum$chamber.Totalps[i.time]
          ,val$calc$sum$chamber.12ps[i.time]
          ,val$calc$sum$chamber.13ps[i.time]
          ,val$calc$sum$chamber.Totalgbw[i.time]
          ,val$calc$sum$chamber.Totalgbc[i.time]
          ,val$calc$sum$chamber.12gbc[i.time]
          ,val$calc$sum$chamber.13gbc[i.time]
          ,val$calc$sum$chamber.Totalgsw[i.time]
          ,val$calc$sum$chamber.Totalgsc[i.time]
          ,val$calc$sum$chamber.12gsc[i.time]
          ,val$calc$sum$chamber.13gsc[i.time]
          ,val$calc$sum$chamber.Totalgtc[i.time]
          ,val$calc$sum$chamber.12gtc[i.time]
          ,val$calc$sum$chamber.13gtc[i.time]
          ,val$calc$sum$chamber.TotalCi[i.time]
          ,val$calc$sum$chamber.12Ci[i.time]
          ,val$calc$sum$chamber.13Ci[i.time]
          ,val$calc$sum$chamber.Totalpi[i.time]
          ,val$calc$sum$chamber.12pi[i.time]
          ,val$calc$sum$chamber.13pi[i.time]
          ,val$calc$sum$chamber.Totalpi_pa[i.time]
          ,val$calc$sum$chamber.Delta.i.simple.for.gm[i.time]
          ,val$calc$sum$chamber.Delta.i.simple.for.modeling[i.time]
          ,val$calc$sum$chamber.Delta.i.complex.for.gm[i.time]
          ,val$calc$sum$chamber.Delta.i.simple.for.gm_Delta.obs[i.time]
          ,val$calc$sum$chamber.Delta.i.complex.for.gm_Delta.obs[i.time]
          ,val$calc$sum$chamber.Totalgm.point.simple[i.time]
          ,val$calc$sum$chamber.12gm.point.simple[i.time]
          ,val$calc$sum$chamber.13gm.point.simple[i.time]
          ,val$calc$sum$chamber.Totalgm.point.complex[i.time]
          ,val$calc$sum$chamber.Totalgm.to.use[i.time]
          ,val$calc$sum$chamber.Totalpc.using.gm[i.time]
          ,val$calc$sum$chamber.12pc.using.gm[i.time]
          ,val$calc$sum$chamber.13pc.using.gm[i.time]
          ,val$calc$sum$chamber.Totalpc.using.simple.Delta.for.gm[i.time]
          ,val$calc$sum$chamber.Totalpc.using.simple.Delta.for.modeling[i.time]
          ,val$calc$sum$chamber.Totalpc.using.complex.Delta.no.decarboxylation[i.time]
          ,val$calc$sum$chamber.Totalpc.using.complex.Delta.full.model[i.time]
          ,val$calc$sum$chamber.Totalpc.to.use[i.time]
          ,val$calc$sum$chamber.TotalCc[i.time]
          ,val$calc$sum$chamber.12Cc[i.time]
          ,val$calc$sum$chamber.13Cc[i.time]
          ,val$calc$sum$Licor.flow.uin[i.time]
          ,val$calc$sum$Licor.H2OR.xin[i.time]
          ,val$calc$sum$Licor.La[i.time]
          ,val$calc$sum$Licor.Atm.press[i.time]
          ,val$calc$sum$Licor.gsc[i.time]
          ,val$calc$sum$Licor.Ci[i.time]
          ,val$calc$sum$Licor.StmRat[i.time]
          ,val$calc$sum$Licor.gbw[i.time]
          ,val$calc$sum$Licor.temp.block[i.time]
          ,val$calc$sum$Licor.Ce[i.time]
          ,val$calc$sum$Licor.Co[i.time]
          ,val$calc$sum$Licor.xout[i.time]
          ,val$calc$sum$Licor.rh.ref[i.time]
          ,val$calc$sum$Licor.rh.sam[i.time]
          ,val$calc$sum$Licor.CsMch[i.time]
          ,val$calc$sum$Licor.HsMch[i.time]
          ,val$calc$sum$Licor.StableF[i.time]
          ,val$calc$sum$Licor.Status[i.time]
          , sep=",")
      )
  }

  write(val$write$summary_Calc, file=output.summary.Calc.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  # last file, last measurement of each cycle file, remove rows that are not last measurements
  n <- val$sum$TDL$n;
  sites <- val$sum$TDL$site;
  last.sites <- rep(0,n);
  for (i.site in 1:n) {
    # if last record, then last by definition
    if (i.site == n) { last.sites[i.site] <- i.site; }
    else {
      # if current different from next, then last
      if (sites[i.site] != sites[i.site+1]) { last.sites[i.site] <- i.site; }
    }
  }
  last.sites <- last.sites[last.sites > 0]; # include only last sites
  last.sites <- c(1,last.sites+1); # include header row

  val$write$summary_Calc_last <- val$write$summary_Calc[last.sites];

  write(val$write$summary_Calc_last, file=output.summary.Calc.last.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

