write.CI.Calc.file <-
function# write output section
### Write all observed and calculated values with BS CIs for all time points.
(val
###
, TDL.cycle
###
, output.CI.Calc.fn
###
, output.CI.Calc.last.fn
###
)
{

  val$write$CI_Calc <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site.name"
    ,"first.ind"
    ,"ind"
    ,"gain.12C.CI.L"                                                ,"gain.12C.CI.U"                                                  #,"gain.12C"
    ,"gain.13C.CI.L"                                                ,"gain.13C.CI.U"                                                  #,"gain.13C"
    ,"offset.12C.CI.L"                                              ,"offset.12C.CI.U"                                                #,"offset.12C"
    ,"offset.13C.CI.L"                                              ,"offset.13C.CI.U"                                                #,"offset.13C"
    ,"reference.12Ce.CI.L"                                          ,"reference.12Ce.CI.U"                                            #,"reference.12Ce"
    ,"reference.13Ce.CI.L"                                          ,"reference.13Ce.CI.U"                                            #,"reference.13Ce"
    ,"chamber.12Co.CI.L"                                            ,"chamber.12Co.CI.U"                                              #,"chamber.12Co"
    ,"chamber.13Co.CI.L"                                            ,"chamber.13Co.CI.U"                                              #,"chamber.13Co"
    ,"reference.TotalCe.CI.L"                                       ,"reference.TotalCe.CI.U"                                         #,"reference.TotalCe"
    ,"chamber.TotalCo.CI.L"                                         ,"chamber.TotalCo.CI.U"                                           #,"chamber.TotalCo"
    ,"chamber.reference.Total.diff.CeCo.CI.L"                       ,"chamber.reference.Total.diff.CeCo.CI.U"                         #,"chamber.reference.Total.diff.CeCo"
    ,"chamber.reference.12.diff.CeCo.CI.L"                          ,"chamber.reference.12.diff.CeCo.CI.U"                            #,"chamber.reference.12.diff.CeCo"
    ,"chamber.reference.13.diff.CeCo.CI.L"                          ,"chamber.reference.13.diff.CeCo.CI.U"                            #,"chamber.reference.13.diff.CeCo"
    ,"xi.CI.L"                                                      ,"xi.CI.U"                                                        #,"xi"
    #,"flow.adjusted.CI.L"                                           ,"flow.adjusted.CI.U"                                             #,"flow.adjusted"              # 9/5/2012
    ,"TDL.A.photosynthesis.CI.L"                                    ,"TDL.A.photosynthesis.CI.U"                                      #,"TDL.A.photosynthesis"
    ,"TDL.12A.photosynthesis.CI.L"                                  ,"TDL.12A.photosynthesis.CI.U"                                    #,"TDL.12A.photosynthesis"
    ,"TDL.13A.photosynthesis.CI.L"                                  ,"TDL.13A.photosynthesis.CI.U"                                    #,"TDL.13A.photosynthesis"
    ,"Licor.A.photosynthesis.CI.L"                                  ,"Licor.A.photosynthesis.CI.U"                                    #,"Licor.A.photosynthesis"
    ,"Delta.from.ratios.in.out.CI.L"                                ,"Delta.from.ratios.in.out.CI.U"                                  #,"Delta.from.ratios.in.out"
    ,"Delta.from.A.ratio.CI.L"                                      ,"Delta.from.A.ratio.CI.U"                                        #,"Delta.from.A.ratio"
    ,"VPD.CI.L"                                                     ,"VPD.CI.U"                                                       #,"VPD"
    ,"E.transpiration.CI.L"                                         ,"E.transpiration.CI.U"                                           #,"E.transpiration"
    ,"leaf.temp.CI.L"                                               ,"leaf.temp.CI.U"                                                 #,"leaf.temp"
    ,"air.temp.CI.L"                                                ,"air.temp.CI.U"                                                  #,"air.temp"
    ,"light.in.CI.L"                                                ,"light.in.CI.U"                                                  #,"light.in"
    ,"light.out.CI.L"                                               ,"light.out.CI.U"                                                 #,"light.out"
    ,"reference.delta.e.CI.L"                                       ,"reference.delta.e.CI.U"                                         #,"reference.delta.e"
    ,"chamber.delta.o.CI.L"                                         ,"chamber.delta.o.CI.U"                                           #,"chamber.delta.o"
    ,"chamber.reference.delta.diff.CoCe.CI.L"                       ,"chamber.reference.delta.diff.CoCe.CI.U"                         #,"chamber.reference.delta.diff.CoCe"
    ,"Delta.obs.CI.L"                                               ,"Delta.obs.CI.U"                                                 #,"Delta.obs"
    ,"Delta.obs.permil.CI.L"                                        ,"Delta.obs.permil.CI.U"                                          #,"Delta.obs.permil"
    ,"delta.13C.Assim.CI.L"                                         ,"delta.13C.Assim.CI.U"                                           #,"delta.13C.Assim"
    ,"p.CI.L"                                                       ,"p.CI.U"                                                         #,"p"
    ,"delta.13C.Resp.CI.L"                                          ,"delta.13C.Resp.CI.U"                                            #,"delta.13C.Resp"
    ,"chamber.TotalCa.CI.L"                                         ,"chamber.TotalCa.CI.U"                                           #,"chamber.TotalCa"
    ,"chamber.12Ca.CI.L"                                            ,"chamber.12Ca.CI.U"                                              #,"chamber.12Ca"
    ,"chamber.13Ca.CI.L"                                            ,"chamber.13Ca.CI.U"                                              #,"chamber.13Ca"
    ,"chamber.TotalCs.CI.L"                                         ,"chamber.TotalCs.CI.U"                                           #,"chamber.TotalCs"
    ,"chamber.12Cs.CI.L"                                            ,"chamber.12Cs.CI.U"                                              #,"chamber.12Cs"
    ,"chamber.13Cs.CI.L"                                            ,"chamber.13Cs.CI.U"                                              #,"chamber.13Cs"
    ,"chamber.Totalpa.CI.L"                                         ,"chamber.Totalpa.CI.U"                                           #,"chamber.Totalpa"
    ,"chamber.12pa.CI.L"                                            ,"chamber.12pa.CI.U"                                              #,"chamber.12pa"
    ,"chamber.13pa.CI.L"                                            ,"chamber.13pa.CI.U"                                              #,"chamber.13pa"
    ,"chamber.Totalps.CI.L"                                         ,"chamber.Totalps.CI.U"                                           #,"chamber.Totalps"
    ,"chamber.12ps.CI.L"                                            ,"chamber.12ps.CI.U"                                              #,"chamber.12ps"
    ,"chamber.13ps.CI.L"                                            ,"chamber.13ps.CI.U"                                              #,"chamber.13ps"
    ,"chamber.Totalgbw.CI.L"                                        ,"chamber.Totalgbw.CI.U"                                          #,"chamber.Totalgbw"
    ,"chamber.Totalgbc.CI.L"                                        ,"chamber.Totalgbc.CI.U"                                          #,"chamber.Totalgbc"
    ,"chamber.12gbc.CI.L"                                           ,"chamber.12gbc.CI.U"                                             #,"chamber.12gbc"
    ,"chamber.13gbc.CI.L"                                           ,"chamber.13gbc.CI.U"                                             #,"chamber.13gbc"
    ,"chamber.Totalgsw.CI.L"                                        ,"chamber.Totalgsw.CI.U"                                          #,"chamber.Totalgsw"
    ,"chamber.Totalgsc.CI.L"                                        ,"chamber.Totalgsc.CI.U"                                          #,"chamber.Totalgsc"
    ,"chamber.12gsc.CI.L"                                           ,"chamber.12gsc.CI.U"                                             #,"chamber.12gsc"
    ,"chamber.13gsc.CI.L"                                           ,"chamber.13gsc.CI.U"                                             #,"chamber.13gsc"
    ,"chamber.Totalgtc.CI.L"                                        ,"chamber.Totalgtc.CI.U"                                          #,"chamber.Totalgtc"
    ,"chamber.12gtc.CI.L"                                           ,"chamber.12gtc.CI.U"                                             #,"chamber.12gtc"
    ,"chamber.13gtc.CI.L"                                           ,"chamber.13gtc.CI.U"                                             #,"chamber.13gtc"
    ,"chamber.TotalCi.CI.L"                                         ,"chamber.TotalCi.CI.U"                                           #,"chamber.TotalCi"
    ,"chamber.12Ci.CI.L"                                            ,"chamber.12Ci.CI.U"                                              #,"chamber.12Ci"
    ,"chamber.13Ci.CI.L"                                            ,"chamber.13Ci.CI.U"                                              #,"chamber.13Ci"
    ,"chamber.Totalpi.CI.L"                                         ,"chamber.Totalpi.CI.U"                                           #,"chamber.Totalpi"
    ,"chamber.12pi.CI.L"                                            ,"chamber.12pi.CI.U"                                              #,"chamber.12pi"
    ,"chamber.13pi.CI.L"                                            ,"chamber.13pi.CI.U"                                              #,"chamber.13pi"
    ,"chamber.Totalpi_pa.CI.L"                                      ,"chamber.Totalpi_pa.CI.U"                                        #,"chamber.Totalpi_pa"
    ,"chamber.Delta.i.simple.for.gm.CI.L"                           ,"chamber.Delta.i.simple.for.gm.CI.U"                             #,"chamber.Delta.i.simple.for.gm"
    ,"chamber.Delta.i.simple.for.modeling.CI.L"                     ,"chamber.Delta.i.simple.for.modeling.CI.U"                       #,"chamber.Delta.i.simple.for.modeling"
    ,"chamber.Delta.i.complex.for.gm.CI.L"                          ,"chamber.Delta.i.complex.for.gm.CI.U"                            #,"chamber.Delta.i.complex.for.gm"
    ,"chamber.Delta.i.simple.for.gm_Delta.obs.CI.L"                 ,"chamber.Delta.i.simple.for.gm_Delta.obs.CI.U"                   #,"chamber.Delta.i.simple.for.gm_Delta.obs"
    ,"chamber.Delta.i.complex.for.gm_Delta.obs.CI.L"                ,"chamber.Delta.i.complex.for.gm_Delta.obs.CI.U"                  #,"chamber.Delta.i.complex.for.gm_Delta.obs"
    ,"chamber.Totalgm.point.simple.CI.L"                            ,"chamber.Totalgm.point.simple.CI.U"                              #,"chamber.Totalgm.point.simple"
    ,"chamber.12gm.point.simple.CI.L"                               ,"chamber.12gm.point.simple.CI.U"                                 #,"chamber.12gm.point.simple"
    ,"chamber.13gm.point.simple.CI.L"                               ,"chamber.13gm.point.simple.CI.U"                                 #,"chamber.13gm.point.simple"
    ,"chamber.Totalgm.point.complex.CI.L"                           ,"chamber.Totalgm.point.complex.CI.U"                             #,"chamber.Totalgm.point.complex"
    ,"chamber.Totalgm.to.use.CI.L"                                  ,"chamber.Totalgm.to.use.CI.U"                                    #,"chamber.Totalgm.to.use"
    ,"chamber.Totalpc.using.gm.CI.L"                                ,"chamber.Totalpc.using.gm.CI.U"                                  #,"chamber.Totalpc.using.gm"
    ,"chamber.12pc.using.gm.CI.L"                                   ,"chamber.12pc.using.gm.CI.U"                                     #,"chamber.12pc.using.gm"
    ,"chamber.13pc.using.gm.CI.L"                                   ,"chamber.13pc.using.gm.CI.U"                                     #,"chamber.13pc.using.gm"
    ,"chamber.Totalpc.using.simple.Delta.for.gm.CI.L"               ,"chamber.Totalpc.using.simple.Delta.for.gm.CI.U"                 #,"chamber.Totalpc.using.simple.Delta.for.gm"
    ,"chamber.Totalpc.using.simple.Delta.for.modeling.CI.L"         ,"chamber.Totalpc.using.simple.Delta.for.modeling.CI.U"           #,"chamber.Totalpc.using.simple.Delta.for.modeling"
    ,"chamber.Totalpc.using.complex.Delta.no.decarboxylation.CI.L"  ,"chamber.Totalpc.using.complex.Delta.no.decarboxylation.CI.U"    #,"chamber.Totalpc.using.complex.Delta.no.decarboxylation"
    ,"chamber.Totalpc.using.complex.Delta.full.model.CI.L"          ,"chamber.Totalpc.using.complex.Delta.full.model.CI.U"            #,"chamber.Totalpc.using.complex.Delta.full.model"
    ,"chamber.Totalpc.to.use.CI.L"                                  ,"chamber.Totalpc.to.use.CI.U"                                    #,"chamber.Totalpc.to.use"
    ,"chamber.TotalCc.CI.L"                                         ,"chamber.TotalCc.CI.U"                                           #,"chamber.TotalCc"
    ,"chamber.12Cc.CI.L"                                            ,"chamber.12Cc.CI.U"                                              #,"chamber.12Cc"
    ,"chamber.13Cc.CI.L"                                            ,"chamber.13Cc.CI.U"                                              #,"chamber.13Cc"
    ,"Licor.flow.uin.CI.L"                                          ,"Licor.flow.uin.CI.U"                                            #,"Licor.flow.uin"
    ,"Licor.H2OR.xin.CI.L"                                          ,"Licor.H2OR.xin.CI.U"                                            #,"Licor.H2OR.xin"
    ,"Licor.La.CI.L"                                                ,"Licor.La.CI.U"                                                  #,"Licor.La"
    ,"Licor.Atm.press.CI.L"                                               ,"Licor.Atm.press.CI.U"                                                 #,"Licor.Atm.press"
    ,"Licor.gsc.CI.L"                                                     ,"Licor.gsc.CI.U"                                                       #,"Licor.gsc"
    ,"Licor.Ci.CI.L"                                                      ,"Licor.Ci.CI.U"                                                        #,"Licor.Ci"
    ,"Licor.StmRat.CI.L"                                                  ,"Licor.StmRat.CI.U"                                                    #,"Licor.StmRat"
    ,"Licor.gbw.CI.L"                                                     ,"Licor.gbw.CI.U"                                                       #,"Licor.gbw"
    ,"Licor.temp.block.CI.L"                                              ,"Licor.temp.block.CI.U"                                                #,"Licor.temp.block"
    ,"Licor.Ce.CI.L"                                                      ,"Licor.Ce.CI.U"                                                        #,"Licor.Ce"
    ,"Licor.Co.CI.L"                                                      ,"Licor.Co.CI.U"                                                        #,"Licor.Co"
    ,"Licor.xout.CI.L"                                                    ,"Licor.xout.CI.U"                                                      #,"Licor.xout"
    ,"Licor.rh.ref"                                                       ,"Licor.rh.ref"                                                         #,"Licor.rh.ref"
    ,"Licor.rh.sam.CI.L"                                                  ,"Licor.rh.sam.CI.U"                                                    #,"Licor.rh.sam"
    ,"Licor.CsMch.CI.L"                                                   ,"Licor.CsMch.CI.U"                                                     #,"Licor.CsMch"
    ,"Licor.HsMch.CI.L"                                                   ,"Licor.HsMch.CI.U"                                                     #,"Licor.HsMch"
    ,"Licor.StableF.CI.L"                                                 ,"Licor.StableF.CI.U"                                                   #,"Licor.StableF"
    ,"Licor.Status.CI.L"                                                  ,"Licor.Status.CI.U"                                                    #,"Licor.Status"
    # typically not used
    #,"VpdA.CI.L"        ,"VpdA.CI.U"         #,"VpdA"
    #,"Ci.Ca.CI.L"       ,"Ci.Ca.CI.U"        #,"Ci.Ca"
    #,"pi.CI.L"          ,"pi.CI.U"           #,"pi"
    #,"uc_20_mV.CI.L"    ,"uc_20_mV.CI.U"     #,"uc_20_mV"
    #,"uc_21_mV.CI.L"    ,"uc_21_mV.CI.U"     #,"uc_21_mV"
    #,"U_S.CI.L"         ,"U_S.CI.U"          #,"U_S"
    #,"Trans.CI.L"       ,"Trans.CI.U"        #,"Trans"
    #,"CndCO2.CI.L"      ,"CndCO2.CI.U"       #,"CndCO2"
    #,"Ref_mV.CI.L"      ,"Ref_mV.CI.U"       #,"Ref_mV"
    #,"xTemp1.CI.L"      ,"xTemp1.CI.U"       #,"xTemp1"
    #,"xTemp2.CI.L"      ,"xTemp2.CI.U"       #,"xTemp2"
    , sep=",");

  for (i.time in 1:val$sum$TDL$n) {
    val$write$CI_Calc <-
      rbind( val$write$CI_Calc
        ,paste(
           format(val$sum$TDL$time[i.time],format="%Y-%m-%d")
          ,format(val$sum$TDL$time[i.time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$sum$TDL$site[i.time]
          ,TDL.cycle$table.name[(TDL.cycle$table[,1] == val$sum$TDL$site[i.time])]
          ,val$sum$TDL$first.ind[i.time]
          ,val$sum$TDL$ind[i.time]
          ,val$calc$CI$gain.12C[i.time,1]                                                ,val$calc$CI$gain.12C[i.time,2]                                                   #,val$calc$sum$gain.12C[i.time]
          ,val$calc$CI$gain.13C[i.time,1]                                                ,val$calc$CI$gain.13C[i.time,2]                                                   #,val$calc$sum$gain.13C[i.time]
          ,val$calc$CI$offset.12C[i.time,1]                                              ,val$calc$CI$offset.12C[i.time,2]                                                 #,val$calc$sum$offset.12C[i.time]
          ,val$calc$CI$offset.13C[i.time,1]                                              ,val$calc$CI$offset.13C[i.time,2]                                                 #,val$calc$sum$offset.13C[i.time]
          ,val$calc$CI$reference.12Ce[i.time,1]                                          ,val$calc$CI$reference.12Ce[i.time,2]                                             #,val$calc$sum$reference.12Ce[i.time]
          ,val$calc$CI$reference.13Ce[i.time,1]                                          ,val$calc$CI$reference.13Ce[i.time,2]                                             #,val$calc$sum$reference.13Ce[i.time]
          ,val$calc$CI$chamber.12Co[i.time,1]                                            ,val$calc$CI$chamber.12Co[i.time,2]                                               #,val$calc$sum$chamber.12Co[i.time]
          ,val$calc$CI$chamber.13Co[i.time,1]                                            ,val$calc$CI$chamber.13Co[i.time,2]                                               #,val$calc$sum$chamber.13Co[i.time]
          ,val$calc$CI$reference.TotalCe[i.time,1]                                       ,val$calc$CI$reference.TotalCe[i.time,2]                                          #,val$calc$sum$reference.TotalCe[i.time]
          ,val$calc$CI$chamber.TotalCo[i.time,1]                                         ,val$calc$CI$chamber.TotalCo[i.time,2]                                            #,val$calc$sum$chamber.TotalCo[i.time]
          ,val$calc$CI$chamber.reference.Total.diff.CeCo[i.time,1]                       ,val$calc$CI$chamber.reference.Total.diff.CeCo[i.time,2]                          #,val$calc$sum$chamber.reference.Total.diff.CeCo[i.time]
          ,val$calc$CI$chamber.reference.12.diff.CeCo[i.time,1]                          ,val$calc$CI$chamber.reference.12.diff.CeCo[i.time,2]                             #,val$calc$sum$chamber.reference.12.diff.CeCo[i.time]
          ,val$calc$CI$chamber.reference.13.diff.CeCo[i.time,1]                          ,val$calc$CI$chamber.reference.13.diff.CeCo[i.time,2]                             #,val$calc$sum$chamber.reference.13.diff.CeCo[i.time]
          ,val$calc$CI$xi[i.time,1]                                                      ,val$calc$CI$xi[i.time,2]                                                         #,val$calc$sum$xi[i.time]
          #,val$calc$CI$flow.adjusted[i.time,1]                                           ,val$calc$CI$flow.adjusted[i.time,2]                                              #,val$calc$sum$flow.adjusted[i.time]        # 9/5/2012
          ,val$calc$CI$TDL.A.photosynthesis[i.time,1]                                    ,val$calc$CI$TDL.A.photosynthesis[i.time,2]                                       #,val$calc$sum$TDL.A.photosynthesis[i.time]
          ,val$calc$CI$TDL.12A.photosynthesis[i.time,1]                                  ,val$calc$CI$TDL.12A.photosynthesis[i.time,2]                                     #,val$calc$sum$TDL.12A.photosynthesis[i.time]
          ,val$calc$CI$TDL.13A.photosynthesis[i.time,1]                                  ,val$calc$CI$TDL.13A.photosynthesis[i.time,2]                                     #,val$calc$sum$TDL.13A.photosynthesis[i.time]
          ,val$calc$CI$Licor.A.photosynthesis[i.time,1]                                  ,val$calc$CI$Licor.A.photosynthesis[i.time,2]                                     #,val$calc$sum$Licor.A.photosynthesis[i.time]
          ,val$calc$CI$Delta.from.ratios.in.out[i.time,1]                                ,val$calc$CI$Delta.from.ratios.in.out[i.time,2]                                   #,val$calc$sum$Delta.from.ratios.in.out[i.time]
          ,val$calc$CI$Delta.from.A.ratio[i.time,1]                                      ,val$calc$CI$Delta.from.A.ratio[i.time,2]                                         #,val$calc$sum$Delta.from.A.ratio[i.time]
          ,val$calc$CI$VPD[i.time,1]                                                     ,val$calc$CI$VPD[i.time,2]                                                        #,val$calc$sum$VPD[i.time]
          ,val$calc$CI$E.transpiration[i.time,1]                                         ,val$calc$CI$E.transpiration[i.time,2]                                            #,val$calc$sum$E.transpiration[i.time]
          ,val$calc$CI$leaf.temp[i.time,1]                                               ,val$calc$CI$leaf.temp[i.time,2]                                                  #,val$calc$sum$leaf.temp[i.time]
          ,val$calc$CI$air.temp[i.time,1]                                                ,val$calc$CI$air.temp[i.time,2]                                                   #,val$calc$sum$air.temp[i.time]
          ,val$calc$CI$light.in[i.time,1]                                                ,val$calc$CI$light.in[i.time,2]                                                   #,val$calc$sum$light.in[i.time]
          ,val$calc$CI$light.out[i.time,1]                                               ,val$calc$CI$light.out[i.time,2]                                                  #,val$calc$sum$light.out[i.time]
          ,val$calc$CI$reference.delta.e[i.time,1]                                       ,val$calc$CI$reference.delta.e[i.time,2]                                          #,val$calc$sum$reference.delta.e[i.time]
          ,val$calc$CI$chamber.delta.o[i.time,1]                                         ,val$calc$CI$chamber.delta.o[i.time,2]                                            #,val$calc$sum$chamber.delta.o[i.time]
          ,val$calc$CI$chamber.reference.delta.diff.CoCe[i.time,1]                       ,val$calc$CI$chamber.reference.delta.diff.CoCe[i.time,2]                          #,val$calc$sum$chamber.reference.delta.diff.CoCe[i.time]
          ,val$calc$CI$Delta.obs[i.time,1]                                               ,val$calc$CI$Delta.obs[i.time,2]                                                  #,val$calc$sum$Delta.obs[i.time]
          ,val$calc$CI$Delta.obs.permil[i.time,1]                                        ,val$calc$CI$Delta.obs.permil[i.time,2]                                           #,val$calc$sum$Delta.obs.permil[i.time]
          ,val$calc$CI$delta.13C.Assim[i.time,1]                                         ,val$calc$CI$delta.13C.Assim[i.time,2]                                            #,val$calc$sum$delta.13C.Assim[i.time]
          ,val$calc$CI$p[i.time,1]                                                       ,val$calc$CI$p[i.time,2]                                                          #,val$calc$sum$p[i.time]
          ,val$calc$CI$delta.13C.Resp[i.time,1]                                          ,val$calc$CI$delta.13C.Resp[i.time,2]                                             #,val$calc$sum$delta.13C.Resp[i.time]
          ,val$calc$CI$chamber.TotalCa[i.time,1]                                         ,val$calc$CI$chamber.TotalCa[i.time,2]                                            #,val$calc$sum$chamber.TotalCa[i.time]
          ,val$calc$CI$chamber.12Ca[i.time,1]                                            ,val$calc$CI$chamber.12Ca[i.time,2]                                               #,val$calc$sum$chamber.12Ca[i.time]
          ,val$calc$CI$chamber.13Ca[i.time,1]                                            ,val$calc$CI$chamber.13Ca[i.time,2]                                               #,val$calc$sum$chamber.13Ca[i.time]
          ,val$calc$CI$chamber.TotalCs[i.time,1]                                         ,val$calc$CI$chamber.TotalCs[i.time,2]                                            #,val$calc$sum$chamber.TotalCs[i.time]
          ,val$calc$CI$chamber.12Cs[i.time,1]                                            ,val$calc$CI$chamber.12Cs[i.time,2]                                               #,val$calc$sum$chamber.12Cs[i.time]
          ,val$calc$CI$chamber.13Cs[i.time,1]                                            ,val$calc$CI$chamber.13Cs[i.time,2]                                               #,val$calc$sum$chamber.13Cs[i.time]
          ,val$calc$CI$chamber.Totalpa[i.time,1]                                         ,val$calc$CI$chamber.Totalpa[i.time,2]                                            #,val$calc$sum$chamber.Totalpa[i.time]
          ,val$calc$CI$chamber.12pa[i.time,1]                                            ,val$calc$CI$chamber.12pa[i.time,2]                                               #,val$calc$sum$chamber.12pa[i.time]
          ,val$calc$CI$chamber.13pa[i.time,1]                                            ,val$calc$CI$chamber.13pa[i.time,2]                                               #,val$calc$sum$chamber.13pa[i.time]
          ,val$calc$CI$chamber.Totalps[i.time,1]                                         ,val$calc$CI$chamber.Totalps[i.time,2]                                            #,val$calc$sum$chamber.Totalps[i.time]
          ,val$calc$CI$chamber.12ps[i.time,1]                                            ,val$calc$CI$chamber.12ps[i.time,2]                                               #,val$calc$sum$chamber.12ps[i.time]
          ,val$calc$CI$chamber.13ps[i.time,1]                                            ,val$calc$CI$chamber.13ps[i.time,2]                                               #,val$calc$sum$chamber.13ps[i.time]
          ,val$calc$CI$chamber.Totalgbw[i.time,1]                                        ,val$calc$CI$chamber.Totalgbw[i.time,2]                                           #,val$calc$sum$chamber.Totalgbw[i.time]
          ,val$calc$CI$chamber.Totalgbc[i.time,1]                                        ,val$calc$CI$chamber.Totalgbc[i.time,2]                                           #,val$calc$sum$chamber.Totalgbc[i.time]
          ,val$calc$CI$chamber.12gbc[i.time,1]                                           ,val$calc$CI$chamber.12gbc[i.time,2]                                              #,val$calc$sum$chamber.12gbc[i.time]
          ,val$calc$CI$chamber.13gbc[i.time,1]                                           ,val$calc$CI$chamber.13gbc[i.time,2]                                              #,val$calc$sum$chamber.13gbc[i.time]
          ,val$calc$CI$chamber.Totalgsw[i.time,1]                                        ,val$calc$CI$chamber.Totalgsw[i.time,2]                                           #,val$calc$sum$chamber.Totalgsw[i.time]
          ,val$calc$CI$chamber.Totalgsc[i.time,1]                                        ,val$calc$CI$chamber.Totalgsc[i.time,2]                                           #,val$calc$sum$chamber.Totalgsc[i.time]
          ,val$calc$CI$chamber.12gsc[i.time,1]                                           ,val$calc$CI$chamber.12gsc[i.time,2]                                              #,val$calc$sum$chamber.12gsc[i.time]
          ,val$calc$CI$chamber.13gsc[i.time,1]                                           ,val$calc$CI$chamber.13gsc[i.time,2]                                              #,val$calc$sum$chamber.13gsc[i.time]
          ,val$calc$CI$chamber.Totalgtc[i.time,1]                                        ,val$calc$CI$chamber.Totalgtc[i.time,2]                                           #,val$calc$sum$chamber.Totalgtc[i.time]
          ,val$calc$CI$chamber.12gtc[i.time,1]                                           ,val$calc$CI$chamber.12gtc[i.time,2]                                              #,val$calc$sum$chamber.12gtc[i.time]
          ,val$calc$CI$chamber.13gtc[i.time,1]                                           ,val$calc$CI$chamber.13gtc[i.time,2]                                              #,val$calc$sum$chamber.13gtc[i.time]
          ,val$calc$CI$chamber.TotalCi[i.time,1]                                         ,val$calc$CI$chamber.TotalCi[i.time,2]                                            #,val$calc$sum$chamber.TotalCi[i.time]
          ,val$calc$CI$chamber.12Ci[i.time,1]                                            ,val$calc$CI$chamber.12Ci[i.time,2]                                               #,val$calc$sum$chamber.12Ci[i.time]
          ,val$calc$CI$chamber.13Ci[i.time,1]                                            ,val$calc$CI$chamber.13Ci[i.time,2]                                               #,val$calc$sum$chamber.13Ci[i.time]
          ,val$calc$CI$chamber.Totalpi[i.time,1]                                         ,val$calc$CI$chamber.Totalpi[i.time,2]                                            #,val$calc$sum$chamber.Totalpi[i.time]
          ,val$calc$CI$chamber.12pi[i.time,1]                                            ,val$calc$CI$chamber.12pi[i.time,2]                                               #,val$calc$sum$chamber.12pi[i.time]
          ,val$calc$CI$chamber.13pi[i.time,1]                                            ,val$calc$CI$chamber.13pi[i.time,2]                                               #,val$calc$sum$chamber.13pi[i.time]
          ,val$calc$CI$chamber.Totalpi_pa[i.time,1]                                      ,val$calc$CI$chamber.Totalpi_pa[i.time,2]                                         #,val$calc$sum$chamber.Totalpi_pa[i.time]
          ,val$calc$CI$chamber.Delta.i.simple.for.gm[i.time,1]                           ,val$calc$CI$chamber.Delta.i.simple.for.gm[i.time,2]                              #,val$calc$sum$chamber.Delta.i.simple.for.gm[i.time]
          ,val$calc$CI$chamber.Delta.i.simple.for.modeling[i.time,1]                     ,val$calc$CI$chamber.Delta.i.simple.for.modeling[i.time,2]                        #,val$calc$sum$chamber.Delta.i.simple.for.modeling[i.time]
          ,val$calc$CI$chamber.Delta.i.complex.for.gm[i.time,1]                          ,val$calc$CI$chamber.Delta.i.complex.for.gm[i.time,2]                             #,val$calc$sum$chamber.Delta.i.complex.for.gm[i.time]
          ,val$calc$CI$chamber.Delta.i.simple.for.gm_Delta.obs[i.time,1]                 ,val$calc$CI$chamber.Delta.i.simple.for.gm_Delta.obs[i.time,2]                    #,val$calc$sum$chamber.Delta.i.simple.for.gm_Delta.obs[i.time]
          ,val$calc$CI$chamber.Delta.i.complex.for.gm_Delta.obs[i.time,1]                ,val$calc$CI$chamber.Delta.i.complex.for.gm_Delta.obs[i.time,2]                   #,val$calc$sum$chamber.Delta.i.complex.for.gm_Delta.obs[i.time]
          ,val$calc$CI$chamber.Totalgm.point.simple[i.time,1]                            ,val$calc$CI$chamber.Totalgm.point.simple[i.time,2]                               #,val$calc$sum$chamber.Totalgm.point.simple[i.time]
          ,val$calc$CI$chamber.12gm.point.simple[i.time,1]                               ,val$calc$CI$chamber.12gm.point.simple[i.time,2]                                  #,val$calc$sum$chamber.12gm.point.simple[i.time]
          ,val$calc$CI$chamber.13gm.point.simple[i.time,1]                               ,val$calc$CI$chamber.13gm.point.simple[i.time,2]                                  #,val$calc$sum$chamber.13gm.point.simple[i.time]
          ,val$calc$CI$chamber.Totalgm.point.complex[i.time,1]                           ,val$calc$CI$chamber.Totalgm.point.complex[i.time,2]                              #,val$calc$sum$chamber.Totalgm.point.complex[i.time]
          ,val$calc$CI$chamber.Totalgm.to.use[i.time,1]                                  ,val$calc$CI$chamber.Totalgm.to.use[i.time,2]                                     #,val$calc$sum$chamber.Totalgm.to.use[i.time]
          ,val$calc$CI$chamber.Totalpc.using.gm[i.time,1]                                ,val$calc$CI$chamber.Totalpc.using.gm[i.time,2]                                   #,val$calc$sum$chamber.Totalpc.using.gm[i.time]
          ,val$calc$CI$chamber.12pc.using.gm[i.time,1]                                   ,val$calc$CI$chamber.12pc.using.gm[i.time,2]                                      #,val$calc$sum$chamber.12pc.using.gm[i.time]
          ,val$calc$CI$chamber.13pc.using.gm[i.time,1]                                   ,val$calc$CI$chamber.13pc.using.gm[i.time,2]                                      #,val$calc$sum$chamber.13pc.using.gm[i.time]
          ,val$calc$CI$chamber.Totalpc.using.simple.Delta.for.gm[i.time,1]               ,val$calc$CI$chamber.Totalpc.using.simple.Delta.for.gm[i.time,2]                  #,val$calc$sum$chamber.Totalpc.using.simple.Delta.for.gm[i.time]
          ,val$calc$CI$chamber.Totalpc.using.simple.Delta.for.modeling[i.time,1]         ,val$calc$CI$chamber.Totalpc.using.simple.Delta.for.modeling[i.time,2]            #,val$calc$sum$chamber.Totalpc.using.simple.Delta.for.modeling[i.time]
          ,val$calc$CI$chamber.Totalpc.using.complex.Delta.no.decarboxylation[i.time,1]  ,val$calc$CI$chamber.Totalpc.using.complex.Delta.no.decarboxylation[i.time,2]     #,val$calc$sum$chamber.Totalpc.using.complex.Delta.no.decarboxylation[i.time]
          ,val$calc$CI$chamber.Totalpc.using.complex.Delta.full.model[i.time,1]          ,val$calc$CI$chamber.Totalpc.using.complex.Delta.full.model[i.time,2]             #,val$calc$sum$chamber.Totalpc.using.complex.Delta.full.model[i.time]
          ,val$calc$CI$chamber.Totalpc.to.use[i.time,1]                                  ,val$calc$CI$chamber.Totalpc.to.use[i.time,2]                                     #,val$calc$sum$chamber.Totalpc.to.use[i.time]
          ,val$calc$CI$chamber.TotalCc[i.time,1]                                         ,val$calc$CI$chamber.TotalCc[i.time,2]                                            #,val$calc$sum$chamber.TotalCc[i.time]
          ,val$calc$CI$chamber.12Cc[i.time,1]                                            ,val$calc$CI$chamber.12Cc[i.time,2]                                               #,val$calc$sum$chamber.12Cc[i.time]
          ,val$calc$CI$chamber.13Cc[i.time,1]                                            ,val$calc$CI$chamber.13Cc[i.time,2]                                               #,val$calc$sum$chamber.13Cc[i.time]
          ,val$calc$CI$Licor.flow.uin[i.time,1]                                          ,val$calc$CI$Licor.flow.uin[i.time,2]                                             #,val$calc$sum$Licor.flow.uin[i.time]
          ,val$calc$CI$Licor.H2OR.xin[i.time,1]                                          ,val$calc$CI$Licor.H2OR.xin[i.time,2]                                             #,val$calc$sum$Licor.H2OR.xin[i.time]
          ,val$calc$CI$Licor.La[i.time,1]                                                ,val$calc$CI$Licor.La[i.time,2]                                                   #,val$calc$sum$Licor.La[i.time]
          ,val$calc$CI$Licor.Atm.press[i.time,1]                                         ,val$calc$CI$Licor.Atm.press[i.time,2]                                            #,"Licor.Atm.press[i.time]
          ,val$calc$CI$Licor.gsc.CI.L[i.time,1]                                          ,val$calc$CI$Licor.gsc.CI.U[i.time,2]                                             #,"Licor.gsc"
          ,val$calc$CI$Licor.Ci.CI.L[i.time,1]                                           ,val$calc$CI$Licor.Ci.CI.U[i.time,2]                                              #,"Licor.Ci"
          ,val$calc$CI$Licor.StmRat.CI.L[i.time,1]                                       ,val$calc$CI$Licor.StmRat.CI.U[i.time,2]                                          #,"Licor.StmRat"
          ,val$calc$CI$Licor.gbw.CI.L[i.time,1]                                          ,val$calc$CI$Licor.gbw.CI.U[i.time,2]                                             #,"Licor.gbw"
          ,val$calc$CI$Licor.temp.block.CI.L[i.time,1]                                   ,val$calc$CI$Licor.temp.block.CI.U[i.time,2]                                      #,"Licor.temp.block"
          ,val$calc$CI$Licor.Ce.CI.L[i.time,1]                                           ,val$calc$CI$Licor.Ce.CI.U[i.time,2]                                              #,"Licor.Ce"
          ,val$calc$CI$Licor.Co.CI.L[i.time,1]                                           ,val$calc$CI$Licor.Co.CI.U[i.time,2]                                              #,"Licor.Co"
          ,val$calc$CI$Licor.xout[i.time,1]                                              ,val$calc$CI$Licor.xout[i.time,2]                                                 #,"Licor.xout"
          ,val$calc$CI$Licor.rh.ref[i.time,1]                                            ,val$calc$CI$Licor.rh.ref[i.time,2]                                               #,"Licor.rh.ref"
          ,val$calc$CI$Licor.rh.sam.CI.L[i.time,1]                                       ,val$calc$CI$Licor.rh.sam.CI.U[i.time,2]                                          #,"Licor.rh.sam"
          ,val$calc$CI$Licor.CsMch.CI.L[i.time,1]                                        ,val$calc$CI$Licor.CsMch.CI.U[i.time,2]                                           #,"Licor.CsMch"
          ,val$calc$CI$Licor.HsMch.CI.L[i.time,1]                                        ,val$calc$CI$Licor.HsMch.CI.U[i.time,2]                                           #,"Licor.HsMch"
          ,val$calc$CI$Licor.StableF.CI.L[i.time,1]                                      ,val$calc$CI$Licor.StableF.CI.U[i.time,2]                                         #,"Licor.StableF"
          ,val$calc$CI$Licor.Status.CI.L[i.time,1]                                       ,val$calc$CI$Licor.Status.CI.U[i.time,2]                                          #,"Licor.Status"
          , sep=",")
      )
  }

  write(val$write$CI_Calc, file=output.CI.Calc.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

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

  val$write$CI_Calc_last <- val$write$CI_Calc[last.sites];

  write(val$write$CI_Calc, file=output.CI.Calc.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)
  write(val$write$CI_Calc_last, file=output.CI.Calc.last.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

