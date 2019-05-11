plot.val.calc.sum <-
function# This function plots all the calculated values
### most plots are not created when not using Licor file
(val.TDL
###
, val.Licor
###
, val.const
###
, val.temp
###
, plot.format.list
###
, output.fn.prefix
###
, sw
###
)
{
  ##details<<
  ## Plots many variables by calling the \code{plot.*} functions.

  ## DEBUG
  # val.TDL          <- val$sum$TDL      ;
  # val.Licor        <- val$sum$Licor    ;
  # val.const        <- val$const        ;
  # val.temp         <- val$calc$sum     ;
  # plot.format.list <- plot.format.list ;
  # output.fn.prefix <- output.fn.prefix ;

  ##details<<
  ## Plot active gains, offsets, and corrected magnitudes. \code{\link{plot.gain.offset}}
    p.o <- paste("      Plot active gains, offsets, and corrected magnitudes\n"); wWw <- write.out(p.o);
  plot.gain.offset(val.temp$gain.12C, val.temp$gain.13C, val.temp$offset.12C, val.temp$offset.13C, val.TDL$time, plot.format.list, output.fn.prefix);

  ##details<<
  ## Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample. \code{\link{plot.corrected.total.diff.xi}}
    p.o <- paste("      Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample\n"); wWw <- write.out(p.o);
  plot.corrected.total.diff.xi( val.temp$reference.12Ce, val.temp$reference.13Ce
                               ,val.temp$chamber.12Co, val.temp$chamber.13Co
                               ,val.temp$reference.TotalCe, val.temp$chamber.TotalCo
                               ,val.temp$chamber.reference.Total.diff.CeCo, val.temp$xi, val.TDL$time, plot.format.list, output.fn.prefix);

 if (sw$use.Licor ) {
  ##details<<
  ## Plot A photosynthesis. \code{\link{plot.A.photosynthesis}}
    p.o <- paste("      Plot A photosynthesis\n"); wWw <- write.out(p.o);
  plot.A.photosynthesis( val.temp$TDL.A.photosynthesis, val.temp$TDL.12A.photosynthesis, val.temp$TDL.13A.photosynthesis
              ,val.temp$Licor.A.photosynthesis, val.temp$Delta.from.ratios.in.out, val.temp$Delta.from.A.ratio
              ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot Licor Temp Light values. \code{\link{plot.Licor.temp.light}}
    p.o <- paste("      Plot Licor Temp Light values\n"); wWw <- write.out(p.o);
  plot.Licor.temp.light(val.temp$VPD, val.temp$E.transpiration, val.temp$leaf.temp, val.temp$air.temp
                        ,val.temp$light.in, val.temp$light.out
                        #, val.temp$flow.adjusted # 9/5/2012
                        ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot Licor uin, xin, La, Atm.press values 7/21/2012. \code{\link{plot.Licor.flow.press}}
    p.o <- paste("      Plot Licor uin, xin, La, Atm.press values\n"); wWw <- write.out(p.o);
  plot.Licor.flow.press(val.temp$Licor.flow.uin, val.temp$Licor.H2OR.xin, val.temp$Licor.La, val.temp$Licor.Atm.press
                        ,val.TDL$time, plot.format.list, output.fn.prefix)
 } # if (sw$use.Licor )

  ##details<<
  ## Plot delta, Delta, and p. \code{\link{plot.delta.Delta.p}}
    p.o <- paste("      Plot delta, Delta, and p\n"); wWw <- write.out(p.o);
  plot.delta.Delta.p( val.temp$reference.delta.e, val.temp$chamber.delta.o, val.temp$chamber.reference.delta.diff.CoCe
                     ,val.temp$Delta.obs, val.temp$p, val.temp$delta.13C.Assim, val.temp$delta.13C.Resp
                     , val.TDL$time, plot.format.list, output.fn.prefix)

 if (sw$use.Licor ) {
  ##details<<
  ## Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface. \code{\link{plot.Ca.Cs}}
    p.o <- paste("      Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface\n"); wWw <- write.out(p.o);
  plot.Ca.Cs(val.temp$chamber.TotalCa, val.temp$chamber.12Ca, val.temp$chamber.13Ca
            ,val.temp$chamber.TotalCs, val.temp$chamber.12Cs, val.temp$chamber.13Cs
            ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface. \code{\link{plot.pa.ps}}
    p.o <- paste("      Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface\n"); wWw <- write.out(p.o);
  plot.pa.ps(val.temp$chamber.Totalpa, val.temp$chamber.12pa, val.temp$chamber.13pa
            ,val.temp$chamber.Totalps, val.temp$chamber.12ps, val.temp$chamber.13ps
            ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2. \code{\link{plot.gbc.gsc.gtc}}
    p.o <- paste("      Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2\n"); wWw <- write.out(p.o);
  plot.gbc.gsc.gtc(val.temp$chamber.Totalgbc, val.temp$chamber.13gbc
                  ,val.temp$chamber.Totalgsc, val.temp$chamber.13gsc
                  ,val.temp$chamber.Totalgtc, val.temp$chamber.13gtc
                  ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot Ci, Pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities. \code{\link{plot.Ci.pi}}
    p.o <- paste("      Plot Ci, pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities\n"); wWw <- write.out(p.o);
  plot.Ci.pi(val.temp$chamber.TotalCi, val.temp$chamber.12Ci, val.temp$chamber.13Ci
            ,val.temp$chamber.Totalpi, val.temp$chamber.12pi, val.temp$chamber.13pi
            ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions. \code{\link{plot.pi_pa}}
    p.o <- paste("      Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf\n"); wWw <- write.out(p.o);
  plot.pi_pa(val.temp$chamber.Totalpi, val.temp$chamber.Totalpa, val.temp$chamber.Totalpi_pa
            ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot Delta.i, predicted discrimination. \code{\link{plot.Delta.i}}
    p.o <- paste("      Plot Delta.i, predicted discrimination\n"); wWw <- write.out(p.o);
  plot.Delta.i(val.temp$chamber.Delta.i.simple.for.gm, val.temp$chamber.Delta.i.simple.for.modeling, val.temp$chamber.Delta.i.complex.for.gm
              ,val.temp$chamber.Delta.i.simple.for.gm_Delta.obs, val.temp$chamber.Delta.i.complex.for.gm_Delta.obs
              ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects. \code{\link{plot.gm}}
    p.o <- paste("      Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects\n"); wWw <- write.out(p.o);
  plot.gm(val.temp$chamber.Totalgm.point.simple, val.temp$chamber.12gm.point.simple, val.temp$chamber.13gm.point.simple
         ,val.temp$chamber.Totalgm.point.complex
         ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot pc, total partial pressure of CO2 at the site of carboxylation. \code{\link{plot.pc.total}}
    p.o <- paste("      Plot pc, total partial pressure of CO2 at the site of carboxylation\n"); wWw <- write.out(p.o);
  plot.pc.total( val.temp$chamber.Totalpc.using.gm, val.temp$chamber.12pc.using.gm, val.temp$chamber.13pc.using.gm
                ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot pc, simple and complex. \code{\link{plot.pc.simple.complex}}
    p.o <- paste("      Plot pc, simple and complex\n"); wWw <- write.out(p.o);
  plot.pc.simple.complex(val.temp$chamber.Totalpc.using.simple.Delta.for.gm, val.temp$chamber.Totalpc.using.simple.Delta.for.modeling
                        ,val.temp$chamber.Totalpc.using.complex.Delta.no.decarboxylation, val.temp$chamber.Totalpc.using.complex.Delta.full.model
                        ,val.TDL$time, plot.format.list, output.fn.prefix)

  ##details<<
  ## Plot Cc, ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol. \code{\link{plot.Cc.total}}
    p.o <- paste("      Plot Cc, ppm CO2 concentration at the site of carboxylation\n"); wWw <- write.out(p.o);
  plot.Cc.total(val.temp$chamber.TotalCc, val.temp$chamber.12Cc, val.temp$chamber.13Cc
               ,val.TDL$time, plot.format.list, output.fn.prefix)
 } # if (sw$use.Licor )

  return( NULL );
  ### NULL
}

