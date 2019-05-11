f.val.calc.all.driver <-
function# This function calls all the \code{f.val.calc.*} files
(val.TDL
###
, val.Licor
###
, val.const
###
, sw
###
)
{
  # DEBUG DEBUG DEBUG DEBUG DEBUG
  # val.TDL   <- val$sum$TDL;
  # val.Licor <- val$sum$Licor;
  # val.const <- val$const;
  # DEBUG DEBUG DEBUG DEBUG DEBUG
  val.temp <- as.list(new.env()); # hold current set of calculated values below

  #-------------------
  ##details<<
  ## SECTION Licor data assign variables
  # LICOR DATA SECTION (moved from bottom to top 9/5/2012, so Licor.flow.uin can replace flow.adjusted)

    # VPD VpdL  LI6400 header, these values listed because I will likely use them in a plot
  val.temp$VPD                  <- val.Licor$VPD;
    # Transpiration Trmmol  LI6400 header, these values listed because I will likely use them in a plot
  val.temp$E.transpiration      <- val.Licor$E;
    # Leaf Temp Tleaf LI6400 header, these values listed because I will likely use them in a plot
  val.temp$leaf.temp            <- val.Licor$temp.leaf;
    # Air Temp ?  Tair  LI6400 header, these values listed because I will likely use them in a plot
  val.temp$air.temp             <- val.Licor$temp.air;
    # Light in  PARi  LI6400 header, these values listed because I will likely use them in a plot
  val.temp$light.in             <- val.Licor$par.int;
    # Light out PARo  LI6400 header, these values listed because I will likely use them in a plot
  val.temp$light.out            <- val.Licor$par.ext;
    # 7/21/2012 added Licor var to save
  val.temp$Licor.flow.uin       <- val.Licor$uin         ;
  val.temp$Licor.H2OR.xin       <- val.Licor$xin         ;
  val.temp$Licor.La             <- val.Licor$La          ;
  val.temp$Licor.Atm.press      <- val.Licor$Atm.press   ;
  val.temp$Licor.gsc            <- val.Licor$gsc         ;
  val.temp$Licor.Ci             <- val.Licor$Ci          ;
  val.temp$Licor.StmRat         <- val.Licor$StmRat      ;
  val.temp$Licor.gbw            <- val.Licor$gbw         ;
  val.temp$Licor.temp.block     <- val.Licor$temp.block  ;
  val.temp$Licor.Ce             <- val.Licor$Ce          ;
  val.temp$Licor.Co             <- val.Licor$Co          ;
  val.temp$Licor.xout           <- val.Licor$xout        ;
  val.temp$Licor.rh.ref         <- val.Licor$rh.ref      ;
  val.temp$Licor.rh.sam         <- val.Licor$rh.sam      ;
  val.temp$Licor.CsMch          <- val.Licor$CsMch       ;
  val.temp$Licor.HsMch          <- val.Licor$HsMch       ;
  val.temp$Licor.StableF        <- val.Licor$StableF     ;
  val.temp$Licor.Status         <- val.Licor$Status      ;

  # typically not used
  #    ,"VpdA"
  #    ,"Ci.Ca"
  #    ,"pi"
  #    ,"uc_20_mV"
  #    ,"uc_21_mV"
  #    ,"U_S"
  #    ,"Trans"
  #    ,"CndCO2"
  #    ,"Ref_mV"
  #    ,"xTemp1"
  #    ,"xTemp2"


  #-------------------
  ##details<<
  ## SECTION Concentrations.

  # also include interp concentration values "0.1-16" "2012-07-10"
  ##details<<
  ## Calibration tank concentrations
  val.temp$tank.hi.12  <- val.TDL$interp.tank.hi.12 ;
  val.temp$tank.hi.13  <- val.TDL$interp.tank.hi.13 ;
  val.temp$tank.low.12 <- val.TDL$interp.tank.low.12;
  val.temp$tank.low.13 <- val.TDL$interp.tank.low.13;

  ##details<<
  ## Calibration tank gain. \code{\link{f.val.calc.gain}}
  temp.gain <-
    f.val.calc.gain( val.const$true.value.hi.12C, val.const$true.value.hi.13C, val.const$true.value.lo.12C, val.const$true.value.lo.13C
                    ,val.TDL$interp.tank.hi.12, val.TDL$interp.tank.hi.13, val.TDL$interp.tank.low.12, val.TDL$interp.tank.low.13);
  val.temp$gain.12C <- temp.gain$gain.12C;
  val.temp$gain.13C <- temp.gain$gain.13C;

  ##details<<
  ## Calibration tank offset. \code{\link{f.val.calc.offset}}
  temp.offset <-
    f.val.calc.offset( val.const$true.value.hi.12C, val.const$true.value.hi.13C
                      ,val.temp$gain.12C, val.temp$gain.13C
                      ,val.TDL$interp.tank.hi.12, val.TDL$interp.tank.hi.13);
  val.temp$offset.12C <- temp.offset$offset.12C;
  val.temp$offset.13C <- temp.offset$offset.13C;

  # # Plot active gains, offsets, and corrected magnitudes
  #  p.o <- paste("Plot active gains, offsets, and corrected magnitudes\n"); wWw <- write.progress(p.o, time.start);
  #plot.gain.offset(val.temp$gain.12C, val.temp$gain.13C, val.temp$offset.12C, val.temp$offset.13C, val.TDL$time, plot.format.list);

  ##details<<
  ## Corrected reference values. \code{\link{f.val.calc.corrected}}
    # 12Ce  12C Site 1 reference Ce is the ppm CO2 concentration entering the leaf chamber, equivalent terms: reference, Site 1, CO2R
    # 13Ce  13C Site 1 reference
  temp.corrected <-
    f.val.calc.corrected(val.TDL$interp.reference.12, val.TDL$interp.reference.13
                        ,val.temp$gain.12C, val.temp$gain.13C
                        ,val.temp$offset.12C, val.temp$offset.13C)
    val.temp$reference.12Ce <- temp.corrected$corrected.12C;
    val.temp$reference.13Ce <- temp.corrected$corrected.13C;

  ##details<<
  ## Corrected chamber values. \code{\link{f.val.calc.corrected}}
    # 12Co  12C Site 21 chamber Co is the outgoing ppm CO2 concentration from the leaf chamber: sample, Site 21, CO2S, Ca, also air above the leaf
    # 13Co  13C Site 21 chamber
  temp.corrected <-
    f.val.calc.corrected(val.TDL$chamber.12, val.TDL$chamber.13
                        ,val.temp$gain.12C, val.temp$gain.13C
                        ,val.temp$offset.12C, val.temp$offset.13C)
    val.temp$chamber.12Co <- temp.corrected$corrected.12C;
    val.temp$chamber.13Co <- temp.corrected$corrected.13C;


  ##details<<
  ## Total reference and chamber values. \code{\link{f.val.calc.total.mol.fraction.CO2}}
    # Total Ce  Total Site 1
    # Total Co  Total Site 21
  val.temp$reference.TotalCe <-
    f.val.calc.total.mol.fraction.CO2(val.temp$reference.12Ce, val.temp$reference.13Ce, val.const$fo.13C);
  val.temp$chamber.TotalCo   <-
    f.val.calc.total.mol.fraction.CO2(val.temp$chamber.12Co, val.temp$chamber.13Co, val.const$fo.13C);

    # Ce-Co Total Site 1 - Total Site 21  I like having this as an output for diagnostics
  val.temp$chamber.reference.Total.diff.CeCo <- val.temp$reference.TotalCe - val.temp$chamber.TotalCo;
  val.temp$chamber.reference.12.diff.CeCo    <- val.temp$reference.12Ce    - val.temp$chamber.12Co;
  val.temp$chamber.reference.13.diff.CeCo    <- val.temp$reference.13Ce    - val.temp$chamber.13Co;

  ##details<<
  ## xi. \code{\link{f.val.calc.xi}}
    # xi Ce/(Ce-Co)  I like having this as an output for diagnostics
  val.temp$xi <-
    f.val.calc.xi(val.temp$reference.TotalCe, val.temp$chamber.TotalCo)

  # # Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample
  #  p.o <- paste("Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample\n"); wWw <- write.progress(p.o, time.start);
  #plot.corrected.total.diff.xi( val.temp$reference.12Ce, val.temp$reference.13Ce
  #                             ,val.temp$chamber.12Co, val.temp$chamber.13Co
  #                             ,val.temp$reference.TotalCe, val.temp$chamber.TotalCo
  #                             ,val.temp$chamber.reference.Total.diff.CeCo, val.temp$xi, val.TDL$time, plot.format.list);

  #-------------------
  ##details<<
  ## SECTION A Photosynthesis

  # # Deprecated (0.1-20, 9/5/2012) ##
    # Adjusted flow, LI6400 flow includes water in the entering air but the TDL removes this water before measuring and this effectively reduces the flow (use H2OR to correct it)
  #val.temp$flow.adjusted <-                                  # 9/5/2012
  #  f.val.calc.flow.adjusted(val.Licor$uin, val.Licor$xin);  # 9/5/2012

  ##details<<
  ## A, TDL photosynthesis. \code{\link{f.val.calc.TDL.A.photosynthesis}}
  val.temp$TDL.A.photosynthesis <-
    #f.val.calc.TDL.A.photosynthesis( val.temp$flow.adjusted, val.temp$reference.TotalCe  # 9/5/2012
    f.val.calc.TDL.A.photosynthesis( val.temp$Licor.flow.uin, val.temp$reference.TotalCe  # 9/5/2012
                                    ,val.temp$chamber.TotalCo, val.Licor$La);
  ##details<<
  ## 12A, TDL photosynthesis. \code{\link{f.val.calc.TDL.A.photosynthesis}}
  val.temp$TDL.12A.photosynthesis <-
    #f.val.calc.TDL.A.photosynthesis( val.temp$flow.adjusted, val.temp$reference.12Ce  # 9/5/2012
    f.val.calc.TDL.A.photosynthesis( val.temp$Licor.flow.uin, val.temp$reference.12Ce  # 9/5/2012
                                    ,val.temp$chamber.12Co, val.Licor$La);
  ##details<<
  ## 13A, TDL photosynthesis. \code{\link{f.val.calc.TDL.A.photosynthesis}}
  val.temp$TDL.13A.photosynthesis <-
    #f.val.calc.TDL.A.photosynthesis( val.temp$flow.adjusted, val.temp$reference.13Ce  # 9/5/2012
    f.val.calc.TDL.A.photosynthesis( val.temp$Licor.flow.uin, val.temp$reference.13Ce  # 9/5/2012
                                    ,val.temp$chamber.13Co, val.Licor$La);

  ##details<<
  ## Total A LI6400  Photo LI6400 header, these values listed because I will likely use them in a plot
  val.temp$Licor.A.photosynthesis <- val.Licor$A;

  ##details<<
  ## Delta from ratios in and out?, (Re/Ro)-1, is this really the same? \code{\link{f.val.calc.Delta.from.ratios.in.out}}
  val.temp$Delta.from.ratios.in.out <-
    f.val.calc.Delta.from.ratios.in.out( val.temp$reference.12Ce, val.temp$reference.13Ce
                                        ,val.temp$chamber.12Co,   val.temp$chamber.13Co)

  ##details<<
  ## D from A ratio, (Ro/(13A/12A))-1, should be the same as Dobs above. \code{\link{f.val.calc.Delta.from.A.ratio}}
  val.temp$Delta.from.A.ratio <-
    f.val.calc.Delta.from.A.ratio( val.temp$chamber.12Co, val.temp$chamber.13Co
                                  ,val.temp$TDL.12A.photosynthesis, val.temp$TDL.13A.photosynthesis)

  ##details<<
  ## Select TDL or Licor based on switch
  # switch added for A.photosynthesis 9/5/2012
  if (sw$Licor.or.TDL.A.photosynthesis == 0) { # Licor
    # use Licor
    val.temp$selected.A.photosynthesis   <- val.temp$Licor.A.photosynthesis;
    val.temp$selected.12A.photosynthesis <- rep(NA, length(val.temp$TDL.12A.photosynthesis));
    val.temp$selected.13A.photosynthesis <- rep(NA, length(val.temp$TDL.13A.photosynthesis));
    #val.temp$TDL.A.photosynthesis   <- rep(NA, length(val.temp$TDL.A.photosynthesis  ));
    #val.temp$TDL.12A.photosynthesis <- rep(NA, length(val.temp$TDL.12A.photosynthesis));
    #val.temp$TDL.13A.photosynthesis <- rep(NA, length(val.temp$TDL.13A.photosynthesis));
  }
  if (sw$Licor.or.TDL.A.photosynthesis == 1) { # TDL
    # use TDL
    val.temp$selected.A.photosynthesis   <- val.temp$TDL.A.photosynthesis  ;
    val.temp$selected.12A.photosynthesis <- val.temp$TDL.12A.photosynthesis;
    val.temp$selected.13A.photosynthesis <- val.temp$TDL.13A.photosynthesis;
    #val.temp$Licor.A.photosynthesis <- rep(NA, length(val.temp$Licor.A.photosynthesis));
  }


  # # Plot A photosynthesis
  #  p.o <- paste("Plot A photosynthesis\n"); wWw <- write.progress(p.o, time.start);
  #plot.A.photosynthesis( val.temp$TDL.A.photosynthesis, val.temp$TDL.12A.photosynthesis, val.temp$TDL.13A.photosynthesis
  #            ,val.temp$Licor.A.photosynthesis, val.temp$Delta.from.ratios.in.out, val.temp$Delta.from.A.ratio
  #            ,val.TDL$time, plot.format.list)
  #
  # # Plot Licor Temp Light values
  #  p.o <- paste("Plot Licor Temp Light values\n"); wWw <- write.progress(p.o, time.start);
  #plot.Licor.temp.light(val.temp$VPD, val.temp$E.transpiration, val.temp$leaf.temp, val.temp$air.temp
  #                      ,val.temp$light.in, val.temp$light.out, val.temp$flow.adjusted
  #                      ,val.TDL$time, plot.format.list)



  #-------------------
  ##details<<
  ## SECTION Delta

  ##details<<
  ## delta reference and chamber values. \code{\link{f.val.calc.delta.proportion}}
    # de  d13C Site 1
  val.temp$reference.delta.e <-
    f.val.calc.delta.proportion(val.temp$reference.12Ce, val.temp$reference.13Ce, val.const$Rstd.13C);
    # do  d13C Site 21
  val.temp$chamber.delta.o   <-
    f.val.calc.delta.proportion(val.temp$chamber.12Co, val.temp$chamber.13Co, val.const$Rstd.13C);

  ##details<<
  ## delta diff
    # do-de d Site 21 - d Site 1  I like having this as an output for diagnostics
  val.temp$chamber.reference.delta.diff.CoCe <- val.temp$chamber.delta.o - val.temp$reference.delta.e;

  ##details<<
  ## Delta discrim
  ##details<<
  ## Dobs observed discrimination. \code{\link{f.val.calc.Delta.obs}}
  val.temp$Delta.obs <-
    f.val.calc.Delta.obs(val.temp$reference.delta.e, val.temp$chamber.delta.o, val.temp$xi)
  ##details<<
  ## Dobs per mil  1000*Dobs. \code{\link{f.val.calc.Delta.obs.permil}}
  val.temp$Delta.obs.permil <-
    f.val.calc.Delta.obs.permil(val.temp$reference.delta.e, val.temp$chamber.delta.o, val.temp$xi)

  ##details<<
  ## delta13C Assimilated, isotopic composition of assimilated sugars. \code{\link{f.val.calc.delta.13C.Assim}}
  val.temp$delta.13C.Assim <-
    f.val.calc.delta.13C.Assim(val.temp$chamber.delta.o, val.temp$Delta.obs)

  ##details<<
  ## p (Co - Ce) / Co. \code{\link{f.val.calc.p}}
  val.temp$p <-
    f.val.calc.p(val.temp$reference.TotalCe, val.temp$chamber.TotalCo)

  ##details<<
  ## delta13C Respired, isotopic composition of respired CO2. \code{\link{f.val.calc.delta.13C.Resp}}
  val.temp$delta.13C.Resp <-
    f.val.calc.delta.13C.Resp(val.temp$reference.delta.e, val.temp$chamber.delta.o, val.temp$p)

  # # Plot delta, Delta, and p
  #  p.o <- paste("Plot delta, Delta, and p\n"); wWw <- write.progress(p.o, time.start);
  #plot.delta.Delta.p( val.temp$reference.delta.e, val.temp$chamber.delta.o, val.temp$chamber.reference.delta.diff.CoCe
  #                   ,val.temp$Delta.obs, val.temp$p, val.temp$delta.13C.Assim, val.temp$delta.13C.Resp
  #                   , val.TDL$time, plot.format.list)

  #-------------------
  ##details<<
  ## SECTION g conductance
  # 9/6/2012 moved above Cs when fixed it's calculation

  ##details<<
  ## gbw BLcond  boundary layer conductance for water, Blcond is LI 6400 header
  val.temp$chamber.Totalgbw <- val.Licor$gbw;
  ##details<<
  ## gbc BLcond/1.37 boundary layer conductance for CO2
  val.temp$chamber.Totalgbc <- val.Licor$gbw / val.const$gbc.1.37;
  val.temp$chamber.12gbc    <- val.temp$chamber.Totalgbc;
  val.temp$chamber.13gbc    <- val.temp$chamber.Totalgbc / (1 + (val.const$a.b / 1000));

  ##details<<
  ## gsw cond  stomatal conductance for water, cond is LI6400 header
  val.temp$chamber.Totalgsw <- val.Licor$gsc;
  ##details<<
  ## gsc gsw/1.6 stomatal conductance for CO2
  val.temp$chamber.Totalgsc <- val.temp$chamber.Totalgsw / val.const$gsc.1.6;
  val.temp$chamber.12gsc    <- val.temp$chamber.Totalgsc;
  val.temp$chamber.13gsc    <- val.temp$chamber.Totalgsc / (1 + (val.const$a.b / 1000));

  ##details<<
  ## gtc, total (stomatal and boundary layer) conductance for CO2
  val.temp$chamber.Totalgtc <- f.val.calc.gtc( val.temp$chamber.Totalgbc, val.temp$chamber.Totalgsc);
  val.temp$chamber.12gtc    <- val.temp$chamber.Totalgtc;
  val.temp$chamber.13gtc    <- f.val.calc.gtc( val.temp$chamber.13gbc, val.temp$chamber.13gsc);


  # # Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2
  #  p.o <- paste("Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2\n"); wWw <- write.progress(p.o, time.start);
  #plot.gbc.gsc.gtc(val.temp$chamber.Totalgbc, val.temp$chamber.13gbc
  #                ,val.temp$chamber.Totalgsc, val.temp$chamber.13gsc
  #                ,val.temp$chamber.Totalgtc, val.temp$chamber.13gtc
  #                ,val.TDL$time, plot.format.list)


  #-------------------
  ##details<<
  ## SECTION Cx and px (concentrations and pressures)

  ##details<<
  ## Ca = Co, ppm CO2 concentration above the leaf = concentration leaving the leaf chamber = ambient CO2 concentration
  val.temp$chamber.TotalCa <- val.temp$chamber.TotalCo;
  val.temp$chamber.12Ca    <- val.temp$chamber.12Co   ;
  val.temp$chamber.13Ca    <- val.temp$chamber.13Co   ;

  ##details<<
  ## Cs, ppm CO2 concentration at the leaf surface, cs calculated from eq 40 Ball 1987 Ch 20 Stomatal Function, eds Zeiger, Farquhar, Cowan. \code{\link{f.val.calc.Cs}}
  #val.temp$chamber.TotalCs <- f.val.calc.Cs(val.Licor$gbw, val.Licor$E, val.temp$chamber.TotalCo, val.temp$TDL.A.photosynthesis);
  #val.temp$chamber.12Cs    <- f.val.calc.Cs(val.Licor$gbw, val.Licor$E, val.temp$chamber.12Co   , val.temp$TDL.A.photosynthesis);
  #val.temp$chamber.13Cs    <- f.val.calc.Cs(val.Licor$gbw, val.Licor$E, val.temp$chamber.13Co   , val.temp$TDL.A.photosynthesis);
  # 7/28/2012 Using A from Licor until test effect of using A from TDL
  # switch added for A.photosynthesis 9/5/2012
  # fixed calculation of Cs, was using val.Licor$gbc before I changed name to gbw 9/6/2012
  val.temp$chamber.TotalCs <- f.val.calc.Cs(val.temp$chamber.Totalgbc, val.Licor$E, val.temp$chamber.TotalCo, val.temp$selected.A.photosynthesis);
  val.temp$chamber.12Cs    <- f.val.calc.Cs(val.temp$chamber.12gbc   , val.Licor$E, val.temp$chamber.12Co   , val.temp$selected.12A.photosynthesis);
  val.temp$chamber.13Cs    <- f.val.calc.Cs(val.temp$chamber.13gbc   , val.Licor$E, val.temp$chamber.13Co   , val.temp$selected.13A.photosynthesis);

  # # Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface
  #  p.o <- paste("Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface\n"); wWw <- write.progress(p.o, time.start);
  #plot.Ca.Cs(val.temp$chamber.TotalCa, val.temp$chamber.12Ca, val.temp$chamber.13Ca
  #          ,val.temp$chamber.TotalCs, val.temp$chamber.12Cs, val.temp$chamber.13Cs
  #          ,val.TDL$time, plot.format.list)

  ##details<<
  ## pa, partial pressure of CO2 above the leaf, Press is the atmospheric pressure value from the LI6400. \code{\link{f.val.calc.pp}}
  val.temp$chamber.Totalpa <- f.val.calc.pp(val.temp$chamber.TotalCo, val.Licor$Atm.press);
  val.temp$chamber.12pa    <- f.val.calc.pp(val.temp$chamber.12Co   , val.Licor$Atm.press);
  val.temp$chamber.13pa    <- f.val.calc.pp(val.temp$chamber.13Co   , val.Licor$Atm.press);

  ##details<<
  ## ps, partial pressure of CO2 at the leaf surface, Press is the atmospheric pressure value from the LI6400. \code{\link{f.val.calc.pp}}
  ## NB: same formula as pa, but using Cs instead of Co
  val.temp$chamber.Totalps <- f.val.calc.pp(val.temp$chamber.TotalCs, val.Licor$Atm.press);
  val.temp$chamber.12ps    <- f.val.calc.pp(val.temp$chamber.12Cs   , val.Licor$Atm.press);
  val.temp$chamber.13ps    <- f.val.calc.pp(val.temp$chamber.13Cs   , val.Licor$Atm.press);

  # # Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface
  #  p.o <- paste("Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface\n"); wWw <- write.progress(p.o, time.start);
  #plot.pa.ps(val.temp$chamber.Totalpa, val.temp$chamber.12pa, val.temp$chamber.13pa
  #          ,val.temp$chamber.Totalps, val.temp$chamber.12ps, val.temp$chamber.13ps
  #          ,val.TDL$time, plot.format.list)


  ##details<<
  ## Ci, ppm CO2 concentration in the sub-stomatal cavities, ci calculated from eq 35 Ball 1987 Ch 20 Stomatal Function, eds Zeiger, Farquhar, Cowan. \code{\link{f.val.calc.Cs}}
  ## NB: same formula as Cs, but using gtc instead of gbc
  #val.temp$chamber.TotalCi <- f.val.calc.Cs(val.temp$chamber.Totalgtc, val.Licor$E, val.temp$chamber.TotalCo, val.temp$TDL.A.photosynthesis);
  #val.temp$chamber.12Ci    <- f.val.calc.Cs(val.temp$chamber.12gtc,    val.Licor$E, val.temp$chamber.12Co   , val.temp$TDL.A.photosynthesis);
  #val.temp$chamber.13Ci    <- f.val.calc.Cs(val.temp$chamber.13gtc,    val.Licor$E, val.temp$chamber.13Co   , val.temp$TDL.A.photosynthesis);
  # 7/28/2012 Using A from Licor until test effect of using A from TDL
  # switch added for A.photosynthesis 9/5/2012
  val.temp$chamber.TotalCi <- f.val.calc.Cs(val.temp$chamber.Totalgtc, val.Licor$E, val.temp$chamber.TotalCo, val.temp$selected.A.photosynthesis);
  val.temp$chamber.12Ci    <- f.val.calc.Cs(val.temp$chamber.12gtc,    val.Licor$E, val.temp$chamber.12Co   , val.temp$selected.12A.photosynthesis);
  val.temp$chamber.13Ci    <- f.val.calc.Cs(val.temp$chamber.13gtc,    val.Licor$E, val.temp$chamber.13Co   , val.temp$selected.13A.photosynthesis);

  ##details<<
  ## pi, partial pressure of CO2 in the substomatal cavities, Press is the atmospheric pressure value from the LI6400. \code{\link{f.val.calc.pp}}
  ## NB: same formula as pa, but using Ci instead of Co
  val.temp$chamber.Totalpi <- f.val.calc.pp(val.temp$chamber.TotalCi, val.Licor$Atm.press);
  val.temp$chamber.12pi    <- f.val.calc.pp(val.temp$chamber.12Ci   , val.Licor$Atm.press);
  val.temp$chamber.13pi    <- f.val.calc.pp(val.temp$chamber.13Ci   , val.Licor$Atm.press);

  # # Plot Ci, Pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities
  #  p.o <- paste("Plot Ci, pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities\n"); wWw <- write.progress(p.o, time.start);
  #plot.Ci.pi(val.temp$chamber.TotalCi, val.temp$chamber.12Ci, val.temp$chamber.13Ci
  #          ,val.temp$chamber.Totalpi, val.temp$chamber.12pi, val.temp$chamber.13pi
  #          ,val.TDL$time, plot.format.list)

  #-------------------

  ##details<<
  ## pi/pa total pi/pa or total Ci/Ca  ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions
  val.temp$chamber.Totalpi_pa <- val.temp$chamber.Totalpi / val.temp$chamber.Totalpa;

  # # Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions
  #  p.o <- paste("Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf\n"); wWw <- write.progress(p.o, time.start);
  #plot.pi_pa(val.temp$chamber.Totalpi, val.temp$chamber.Totalpa, val.temp$chamber.Totalpi_pa
  #          ,val.TDL$time, plot.format.list)


  #-------------------
  ##details<<
  ## SECTION Delta.i predictions

  ##details<<
  ## Delta.i simple for gm, predicted discrimination including boundary layer effects but not decarboxylation effects. \code{\link{f.val.calc.Delta.i.simple.for.gm}}
  val.temp$chamber.Delta.i.simple.for.gm <-
    f.val.calc.Delta.i.simple.for.gm( val.const$a, val.const$b.gm, val.const$a.b
                                     ,val.temp$chamber.Totalpa, val.temp$chamber.Totalps, val.temp$chamber.Totalpi)

  ##details<<
  ## Delta.i simple for modeling  a + (b-a) pi/pa predicted discrimination including boundary layer effects but using b adjustments to approximate effects of gm and decarboxylations. \code{\link{f.val.calc.Delta.i.simple.for.modeling}}
  val.temp$chamber.Delta.i.simple.for.modeling <-
    f.val.calc.Delta.i.simple.for.modeling( val.const$a, val.const$b.modeling
                                     ,val.temp$chamber.Totalpa, val.temp$chamber.Totalpi)

  ##details<<
  ## Delta.i complex for gm  CHECK DERIVATION  predicted discrimination including boundary layer effects AND decarboxylation effects. \code{\link{f.val.calc.Delta.i.complex.for.gm}}
  val.temp$chamber.Delta.i.complex.for.gm <-
    f.val.calc.Delta.i.complex.for.gm( val.const$a, val.const$b.gm, val.const$a.b
                                      ,val.temp$chamber.Totalpa, val.temp$chamber.Totalps, val.temp$chamber.Totalpi
                                      ,val.const$b.gm
                                      ,val.const$f.photo.respiration, val.const$Gamma.star
                                      ,val.const$e, val.const$Rd, val.const$k);

  # CAN PROBABLY REMOVE THIS COMMENTED CODE TO SELECT DELTA.i
  #  # select one of the calculated Delta.i values to use
  #val.temp$chamber.Delta.i.to.use <- NULL; # as default
  #switch(sw$calc.Delta.i.to.use
  #    # 1 = Delta.i.simple.for.gm
  #        , val.temp$chamber.Delta.i.to.use <- val.temp$chamber.Delta.i.simple.for.gm
  #    # 2 = Delta.i.simple.for.modeling
  #        , val.temp$chamber.Delta.i.to.use <- val.temp$chamber.Delta.i.simple.for.modeling
  #    # 3 = Delta.i.complex.for.gm
  #        , val.temp$chamber.Delta.i.to.use <- val.temp$chamber.Delta.i.complex.for.gm
  #  );

  #-------------------

  ##details<<
  ## Delta.i simple for gm-Dobs   (Di simple for gm)-(Dobs per mil)
  val.temp$chamber.Delta.i.simple.for.gm_Delta.obs <-
    val.temp$chamber.Delta.i.simple.for.gm - val.temp$Delta.obs.permil; # val.temp$Delta.obs.permil;   # 8/19/2011
  ##details<<
  ## Delta.i complex for gm-Dobs  (Di complex for gm)-(Dobs per mil)
  val.temp$chamber.Delta.i.complex.for.gm_Delta.obs <-
    val.temp$chamber.Delta.i.complex.for.gm - val.temp$Delta.obs.permil; # val.temp$Delta.obs.permil;  # 8/19/2011



  # # Plot Delta.i, predicted discrimination
  #  p.o <- paste("Plot Delta.i, predicted discrimination\n"); wWw <- write.progress(p.o, time.start);
  #plot.Delta.i(val.temp$chamber.Delta.i.simple.for.gm, val.temp$chamber.Delta.i.simple.for.modeling, val.temp$chamber.Delta.i.complex.for.gm
  #            ,val.temp$chamber.Delta.i.simple.for.gm_Delta.obs, val.temp$chamber.Delta.i.complex.for.gm_Delta.obs
  #            ,val.TDL$time, plot.format.list)


  #-------------------
  ##details<<
  ## SECTION gm mesophyll conductance

  ##details<<
  ## gm point simple, internal leaf (mesophyll) conductance calculated for every D value ignoring decarboxylation effects. \code{\link{f.val.calc.gm.point.simple}}
  val.temp$chamber.Totalgm.point.simple <-
    # switch added for A.photosynthesis 9/5/2012
    #f.val.calc.gm.point.simple( val.const$b.gm, val.const$b.s, val.const$a.l, val.Licor$A
    f.val.calc.gm.point.simple( val.const$b.gm, val.const$b.s, val.const$a.l, val.temp$selected.A.photosynthesis
                               ,val.temp$chamber.Totalpa, val.temp$chamber.Delta.i.simple.for.gm, val.temp$Delta.obs.permil);  # 8/19/2011  use permil
  # switch added for A.photosynthesis 9/5/2012
  #val.temp$chamber.12gm.point.simple <- val.temp$chamber.Totalgm.point.simple;
  val.temp$chamber.12gm.point.simple <-
    f.val.calc.gm.point.simple( val.const$b.gm, val.const$b.s, val.const$a.l, val.temp$selected.12A.photosynthesis
                               ,val.temp$chamber.12pa, val.temp$chamber.Delta.i.simple.for.gm, val.temp$Delta.obs.permil);
  val.temp$chamber.13gm.point.simple <-
    # switch added for A.photosynthesis 9/5/2012
    #f.val.calc.gm.point.simple( val.const$b.gm, val.const$b.s, val.const$a.l, val.temp$TDL.13A.photosynthesis
    f.val.calc.gm.point.simple( val.const$b.gm, val.const$b.s, val.const$a.l, val.temp$selected.13A.photosynthesis
                               ,val.temp$chamber.13pa, val.temp$chamber.Delta.i.simple.for.gm, val.temp$Delta.obs.permil);

  ##details<<
  ## gm point complex, internal leaf (mesophyll) conductance calculated for every D value estimating decarboxylation effects. \code{\link{f.val.calc.gm.point.complex}}
  val.temp$chamber.Totalgm.point.complex <-
    # switch added for A.photosynthesis 9/5/2012
    #f.val.calc.gm.point.complex( val.const$b.gm, val.const$b.s, val.const$a.l, val.Licor$A
    f.val.calc.gm.point.complex( val.const$b.gm, val.const$b.s, val.const$a.l, val.temp$selected.A.photosynthesis
                                ,val.temp$chamber.Totalpa, val.temp$chamber.Delta.i.complex.for.gm, val.temp$Delta.obs.permil  # 8/19/2011  use permil
                                ,val.const$f.photo.respiration, val.const$Gamma.star
                                ,val.const$e, val.const$Rd, val.const$k);

  ##details<<
  ## select one of the calculated gm values to use
  val.temp$chamber.Totalgm.to.use <- NULL; # as default
  switch(sw$calc.gm.to.use
      # 1 = gm.point.simple
          , val.temp$chamber.Totalgm.to.use <- val.temp$chamber.Totalgm.point.simple
      # 2 = gm.point.complex
          , val.temp$chamber.Totalgm.to.use <- val.temp$chamber.Totalgm.point.complex
    );

  # # Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects
  #  p.o <- paste("Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects\n"); wWw <- write.progress(p.o, time.start);
  #plot.gm(val.temp$chamber.Totalgm.point.simple, val.temp$chamber.12gm.point.simple, val.temp$chamber.13gm.point.simple
  #       ,val.temp$chamber.Totalgm.point.complex
  #       ,val.TDL$time, plot.format.list)



  #-------------------
  ##details<<
  ## SECTION pc

  ##details<<
  ## pc using gm,  total partial pressure of CO2 at the site of carboxylation, Press is the atmospheric pressure value from the LI6400. \code{\link{f.val.calc.pc.using.gm}}
    # switch added for A.photosynthesis 9/5/2012
  #val.temp$chamber.Totalpc.using.gm <- f.val.calc.pc.using.gm(val.temp$chamber.Totalpi, val.Licor$A, val.temp$chamber.Totalgm.to.use);
  #val.temp$chamber.12pc.using.gm    <- f.val.calc.pc.using.gm(val.temp$chamber.12pi   , val.Licor$A, val.temp$chamber.Totalgm.to.use);
  #val.temp$chamber.13pc.using.gm    <- f.val.calc.pc.using.gm(val.temp$chamber.13pi   , val.Licor$A, val.temp$chamber.Totalgm.to.use);
  val.temp$chamber.Totalpc.using.gm <- f.val.calc.pc.using.gm(val.temp$chamber.Totalpi, val.temp$selected.A.photosynthesis, val.temp$chamber.Totalgm.to.use);
  val.temp$chamber.12pc.using.gm    <- f.val.calc.pc.using.gm(val.temp$chamber.12pi   , val.temp$selected.12A.photosynthesis, val.temp$chamber.Totalgm.to.use);
  val.temp$chamber.13pc.using.gm    <- f.val.calc.pc.using.gm(val.temp$chamber.13pi   , val.temp$selected.13A.photosynthesis, val.temp$chamber.Totalgm.to.use);

  ##details<<
  ## pc using simple D for gm, includes boundary layer. \code{\link{f.val.calc.pc.using.simple.Delta.for.gm}}
  val.temp$chamber.Totalpc.using.simple.Delta.for.gm <-
    f.val.calc.pc.using.simple.Delta.for.gm( val.temp$chamber.Delta.i.simple.for.gm, val.temp$chamber.Totalpa
                                            ,val.temp$chamber.Totalps, val.const$a, val.const$b.gm, val.const$a.b);

  ##details<<
  ## pc using simple D for modeling. \code{\link{f.val.calc.pc.using.simple.Delta.for.modeling}}
  val.temp$chamber.Totalpc.using.simple.Delta.for.modeling <-
    f.val.calc.pc.using.simple.Delta.for.modeling( val.temp$chamber.Delta.i.simple.for.modeling, val.temp$chamber.Totalpa
                                                  ,val.const$a, val.const$b.modeling);

  # # # What value of b to use here?
  ##details<<
  ## pc using complex D, no decarboxylation  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-Dpa]/(bs+al-b)  is this different from two up? They give different values but both may not be derived properly. \code{\link{f.val.calc.pc.using.complex.Delta.no.decarboxylation}}
  val.temp$chamber.Totalpc.using.complex.Delta.no.decarboxylation <-
    f.val.calc.pc.using.complex.Delta.no.decarboxylation( val.temp$chamber.Delta.i.complex.for.gm, val.temp$chamber.Totalpa
                                                         ,val.temp$chamber.Totalps, val.temp$chamber.Totalpi
                                                         ,val.const$a, val.const$a.b, val.const$a.l, val.const$b.modeling, val.const$b.s)

  # # # What value of b to use here?
  ##details<<
  ## pc using complex D, full model  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-(eRd/k+fG*)-Dpa]/(bs+al-b)  includes boundary layer and decarboxylation effects. \code{\link{f.val.calc.pc.using.complex.Delta.full.model}}
  val.temp$chamber.Totalpc.using.complex.Delta.full.model <-
    f.val.calc.pc.using.complex.Delta.full.model( val.temp$chamber.Delta.i.complex.for.gm, val.temp$chamber.Totalpa
                                                 ,val.temp$chamber.Totalps, val.temp$chamber.Totalpi
                                                 ,val.const$a, val.const$a.b, val.const$a.l, val.const$b.modeling, val.const$b.s
                                                 ,val.const$e, val.const$Rd, val.const$k, val.const$Gamma.star, val.const$f.photo.respiration)


  # # Plot pc, total partial pressure of CO2 at the site of carboxylation
  #  p.o <- paste("Plot pc, total partial pressure of CO2 at the site of carboxylation\n"); wWw <- write.progress(p.o, time.start);
  #plot.pc.total( val.temp$chamber.Totalpc.using.gm, val.temp$chamber.12pc.using.gm, val.temp$chamber.13pc.using.gm
  #              ,val.TDL$time, plot.format.list)
  #
  # # Plot pc, simple and complex
  #  p.o <- paste("Plot pc, simple and complex\n"); wWw <- write.progress(p.o, time.start);
  #plot.pc.simple.complex(val.temp$chamber.Totalpc.using.simple.Delta.for.gm, val.temp$chamber.Totalpc.using.simple.Delta.for.modeling
  #                      ,val.temp$chamber.Totalpc.using.complex.Delta.no.decarboxylation, val.temp$chamber.Totalpc.using.complex.Delta.full.model
  #                      ,val.TDL$time, plot.format.list)


  ##details<<
  ## select one of the calculated pc values to use
  val.temp$chamber.Totalpc.to.use <- NULL; # as default
  switch(sw$calc.pc.to.use
      # 1 = pc.using.gm
          , val.temp$chamber.Totalpc.to.use <- val.temp$chamber.Totalpc.using.gm
      # 2 = pc.using.simple.Delta.for.gm
          , val.temp$chamber.Totalpc.to.use <- val.temp$chamber.Totalpc.using.simple.Delta.for.gm
      # 3 = pc.using.simple.Delta.for.modeling
          , val.temp$chamber.Totalpc.to.use <- val.temp$chamber.Totalpc.using.simple.Delta.for.modeling
      # 4 = pc.using.complex.Delta.no.decarboxylation
          , val.temp$chamber.Totalpc.to.use <- val.temp$chamber.Totalpc.using.complex.Delta.no.decarboxylation
      # 5 = pc.using.complex.Delta.full.model
          , val.temp$chamber.Totalpc.to.use <- val.temp$chamber.Totalpc.using.complex.Delta.full.model
    );

  # (after pc -- one of the several pc's)
  ##details<<
  ## Cc  (pc*10^6)/(Press*1000)  ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol. \code{\link{f.val.calc.pp}}
  ## NB: same formula as pa, but using pc instead of Co
  val.temp$chamber.TotalCc <- f.val.calc.pp(val.temp$chamber.Totalpc.using.gm, val.Licor$Atm.press);
  val.temp$chamber.12Cc    <- f.val.calc.pp(val.temp$chamber.12pc.using.gm   , val.Licor$Atm.press);
  val.temp$chamber.13Cc    <- f.val.calc.pp(val.temp$chamber.13pc.using.gm   , val.Licor$Atm.press);


  # # Plot Cc, ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol
  #  p.o <- paste("Plot Cc, ppm CO2 concentration at the site of carboxylation\n"); wWw <- write.progress(p.o, time.start);
  #plot.Cc.total(val.temp$chamber.TotalCc, val.temp$chamber.12Cc, val.temp$chamber.13Cc
  #             ,val.TDL$time, plot.format.list)


  return( val.temp );
  ### val.temp
}

