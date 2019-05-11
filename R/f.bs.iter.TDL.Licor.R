f.bs.iter.TDL.Licor <-
function# one BS resample of TDL and Licor means
###
(val.obs.TDL
###
, val.sum.TDL
###
, val.obs.Licor
###
, val.sum.Licor
###
, var.interp
###
)
{

  ##details<<
  ## create val.bs.sum for calculated observed values to use in NP bootstrap
  val.bs.sum        <- as.list(new.env()); # for calculated observed values to use in NP bootstrap
  val.bs.sum$TDL    <- as.list(new.env()); # for calculated observed values to use in NP bootstrap
  val.bs.sum$Licor  <- as.list(new.env()); # for calculated observed values to use in NP bootstrap

  ##details<<
  ## TDL using \code{\link{f.np.bs.mean}}
  val.bs.sum$TDL$n                     <- val.sum.TDL$n;
  val.bs.sum$TDL$first.ind             <- val.sum.TDL$first.ind;
  val.bs.sum$TDL$ind                   <- val.sum.TDL$ind;
  val.bs.sum$TDL$site                  <- val.sum.TDL$site;
  val.bs.sum$TDL$n.sam                 <- val.sum.TDL$n.sam;
  val.bs.sum$TDL$time                  <- val.sum.TDL$time;
  val.bs.sum$TDL$conc12CO2             <- f.np.bs.mean (val.obs.TDL$conc12CO2          , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$conc13CO2             <- f.np.bs.mean (val.obs.TDL$conc13CO2          , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$TGApressure           <- f.np.bs.mean (val.obs.TDL$TGApressure        , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$MassFlow1             <- f.np.bs.mean (val.obs.TDL$MassFlow1          , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$Pressure1             <- f.np.bs.mean (val.obs.TDL$Pressure1          , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$MassFlow2             <- f.np.bs.mean (val.obs.TDL$MassFlow2          , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$Pressure2             <- f.np.bs.mean (val.obs.TDL$Pressure2          , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$PressureProMan        <- f.np.bs.mean (val.obs.TDL$PressureProMan     , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$interp.tank.hi.12     <- f.par.bs.mean(val.sum.TDL$interp.tank.hi.12  , var.interp$tank.hi.12   , val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$interp.tank.hi.13     <- f.par.bs.mean(val.sum.TDL$interp.tank.hi.13  , var.interp$tank.hi.13   , val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$interp.tank.low.12    <- f.par.bs.mean(val.sum.TDL$interp.tank.low.12 , var.interp$tank.low.12  , val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$interp.tank.low.13    <- f.par.bs.mean(val.sum.TDL$interp.tank.low.13 , var.interp$tank.low.13  , val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$interp.reference.12   <- f.par.bs.mean(val.sum.TDL$interp.reference.12, var.interp$reference.12 , val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$interp.reference.13   <- f.par.bs.mean(val.sum.TDL$interp.reference.13, var.interp$reference.13 , val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$chamber.12            <- f.np.bs.mean (val.obs.TDL$chamber.12         , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);
  val.bs.sum$TDL$chamber.13            <- f.np.bs.mean (val.obs.TDL$chamber.13         , val.bs.sum$TDL$first.ind, val.bs.sum$TDL$ind, val.bs.sum$TDL$n.sam, val.bs.sum$TDL$n);

  ##details<<
  ## Licor
  ## XXX Still need to determine what to do about BS for Licor data
  val.bs.sum$Licor$n                   <- val.sum.Licor$n           ;
  val.bs.sum$Licor$first.ind           <- val.sum.Licor$first.ind   ;
  val.bs.sum$Licor$ind                 <- val.sum.Licor$ind         ;
  val.bs.sum$Licor$site                <- val.sum.Licor$site        ;
  val.bs.sum$Licor$time                <- val.sum.Licor$time        ;
  val.bs.sum$Licor$FTime               <- val.sum.Licor$FTime       ;
  val.bs.sum$Licor$A                   <- val.sum.Licor$A           ;
  val.bs.sum$Licor$gsc                 <- val.sum.Licor$gsc         ;
  val.bs.sum$Licor$Ci                  <- val.sum.Licor$Ci          ;
  val.bs.sum$Licor$E                   <- val.sum.Licor$E           ;
  val.bs.sum$Licor$VPD                 <- val.sum.Licor$VPD         ;
  val.bs.sum$Licor$La                  <- val.sum.Licor$La          ;
  val.bs.sum$Licor$StmRat              <- val.sum.Licor$StmRat      ;
  val.bs.sum$Licor$gbw                 <- val.sum.Licor$gbw         ;
  val.bs.sum$Licor$temp.air            <- val.sum.Licor$temp.air    ;
  val.bs.sum$Licor$temp.leaf           <- val.sum.Licor$temp.leaf   ;
  val.bs.sum$Licor$temp.block          <- val.sum.Licor$temp.block  ;
  val.bs.sum$Licor$Ce                  <- val.sum.Licor$Ce          ;
  val.bs.sum$Licor$Co                  <- val.sum.Licor$Co          ;
  val.bs.sum$Licor$xin                 <- val.sum.Licor$xin         ;
  val.bs.sum$Licor$xout                <- val.sum.Licor$xout        ;
  val.bs.sum$Licor$rh.ref              <- val.sum.Licor$rh.ref      ;
  val.bs.sum$Licor$rh.sam              <- val.sum.Licor$rh.sam      ;
  val.bs.sum$Licor$uin                 <- val.sum.Licor$uin         ;
  val.bs.sum$Licor$par.int             <- val.sum.Licor$par.int     ;
  val.bs.sum$Licor$par.ext             <- val.sum.Licor$par.ext     ;
  val.bs.sum$Licor$Atm.press           <- val.sum.Licor$Atm.press   ;
  val.bs.sum$Licor$CsMch               <- val.sum.Licor$CsMch       ;
  val.bs.sum$Licor$HsMch               <- val.sum.Licor$HsMch       ;
  val.bs.sum$Licor$StableF             <- val.sum.Licor$StableF     ;
  val.bs.sum$Licor$Status              <- val.sum.Licor$Status      ;
  val.bs.sum$Licor$VpdA                <- val.sum.Licor$VpdA        ;
  val.bs.sum$Licor$Ci.Ca               <- val.sum.Licor$Ci.Ca       ;
  val.bs.sum$Licor$pi                  <- val.sum.Licor$pi          ;
  val.bs.sum$Licor$uc_20_mV            <- val.sum.Licor$uc_20_mV    ;
  val.bs.sum$Licor$uc_21_mV            <- val.sum.Licor$uc_21_mV    ;
  val.bs.sum$Licor$U_S                 <- val.sum.Licor$U_S         ;
  val.bs.sum$Licor$Trans               <- val.sum.Licor$Trans       ;
  val.bs.sum$Licor$CndCO2              <- val.sum.Licor$CndCO2      ;
  val.bs.sum$Licor$Ref_mV              <- val.sum.Licor$Ref_mV      ;
  val.bs.sum$Licor$xTemp1              <- val.sum.Licor$xTemp1      ;
  val.bs.sum$Licor$xTemp2              <- val.sum.Licor$xTemp2      ;

  return( val.bs.sum );
  ### val.bs.sum
}

