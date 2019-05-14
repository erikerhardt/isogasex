f_val_bs_CI <-
function# create CI for each calculated value
###
(val.calc.bs
###
, val.bs.TDL
###
, val.bs.Licor
###
, R.bootstrap
###
, sig.CI
###
, sw
###
)
{

  ##details<<
  ## Create val.CI CI endpoints for $TDL, $Licor, and $calc.

  val.CI              <- as.list(new.env());
  val.CI$TDL          <- as.list(new.env());
  val.CI$Licor        <- as.list(new.env());
  val.CI$calc         <- as.list(new.env());

  ##details<<
  ## Calculate quantile indices of sorted data for sig.CI.
  ind.CI <- c( floor  (R.bootstrap * (sig.CI / 2 ))
              ,ceiling(R.bootstrap * (1 - sig.CI / 2)));

  ##details<<
  ## For reasonable variables, calculates CIs using \code{\link{f_val_bs_CI_endpoints}}.

  # TDL
  if (sw$use.TDL) {
    val.CI$TDL$n                     <- val.bs.TDL$n
    val.CI$TDL$first.ind             <- val.bs.TDL$first.ind
    val.CI$TDL$ind                   <- val.bs.TDL$ind
    val.CI$TDL$site                  <- val.bs.TDL$site
    val.CI$TDL$n.sam                 <- val.bs.TDL$n.sam
    val.CI$TDL$time                  <- val.bs.TDL$time
    val.CI$TDL$conc12CO2             <- f_val_bs_CI_endpoints(val.bs.TDL$conc12CO2            , val.bs.TDL$n, ind.CI);
    val.CI$TDL$conc13CO2             <- f_val_bs_CI_endpoints(val.bs.TDL$conc13CO2            , val.bs.TDL$n, ind.CI);
    val.CI$TDL$TGApressure           <- f_val_bs_CI_endpoints(val.bs.TDL$TGApressure          , val.bs.TDL$n, ind.CI);
    val.CI$TDL$MassFlow1             <- f_val_bs_CI_endpoints(val.bs.TDL$MassFlow1            , val.bs.TDL$n, ind.CI);
    val.CI$TDL$Pressure1             <- f_val_bs_CI_endpoints(val.bs.TDL$Pressure1            , val.bs.TDL$n, ind.CI);
    val.CI$TDL$MassFlow2             <- f_val_bs_CI_endpoints(val.bs.TDL$MassFlow2            , val.bs.TDL$n, ind.CI);
    val.CI$TDL$Pressure2             <- f_val_bs_CI_endpoints(val.bs.TDL$Pressure2            , val.bs.TDL$n, ind.CI);
    val.CI$TDL$PressureProMan        <- f_val_bs_CI_endpoints(val.bs.TDL$PressureProMan       , val.bs.TDL$n, ind.CI);
    val.CI$TDL$interp.tank.hi.12     <- f_val_bs_CI_endpoints(val.bs.TDL$interp.tank.hi.12    , val.bs.TDL$n, ind.CI);
    val.CI$TDL$interp.tank.hi.13     <- f_val_bs_CI_endpoints(val.bs.TDL$interp.tank.hi.13    , val.bs.TDL$n, ind.CI);
    val.CI$TDL$interp.tank.low.12    <- f_val_bs_CI_endpoints(val.bs.TDL$interp.tank.low.12   , val.bs.TDL$n, ind.CI);
    val.CI$TDL$interp.tank.low.13    <- f_val_bs_CI_endpoints(val.bs.TDL$interp.tank.low.13   , val.bs.TDL$n, ind.CI);
    val.CI$TDL$interp.reference.12   <- f_val_bs_CI_endpoints(val.bs.TDL$interp.reference.12  , val.bs.TDL$n, ind.CI);
    val.CI$TDL$interp.reference.13   <- f_val_bs_CI_endpoints(val.bs.TDL$interp.reference.13  , val.bs.TDL$n, ind.CI);
    val.CI$TDL$chamber.12            <- f_val_bs_CI_endpoints(val.bs.TDL$chamber.12           , val.bs.TDL$n, ind.CI);
    val.CI$TDL$chamber.13            <- f_val_bs_CI_endpoints(val.bs.TDL$chamber.13           , val.bs.TDL$n, ind.CI);
  }

  # Licor
  if (sw$use.Licor) {
    val.CI$Licor$n                   <- val.bs.Licor$n
    val.CI$Licor$first.ind           <- val.bs.Licor$first.ind
    val.CI$Licor$ind                 <- val.bs.Licor$ind
    val.CI$Licor$site                <- val.bs.Licor$site
    val.CI$Licor$time                <- val.bs.Licor$time
    val.CI$Licor$FTime               <- f_val_bs_CI_endpoints(val.bs.Licor$FTime        , val.bs.Licor$n, ind.CI);
    val.CI$Licor$A                   <- f_val_bs_CI_endpoints(val.bs.Licor$A            , val.bs.Licor$n, ind.CI);
    val.CI$Licor$gsc                 <- f_val_bs_CI_endpoints(val.bs.Licor$gsc          , val.bs.Licor$n, ind.CI);
    val.CI$Licor$Ci                  <- f_val_bs_CI_endpoints(val.bs.Licor$Ci           , val.bs.Licor$n, ind.CI);
    val.CI$Licor$E                   <- f_val_bs_CI_endpoints(val.bs.Licor$E            , val.bs.Licor$n, ind.CI);
    val.CI$Licor$VPD                 <- f_val_bs_CI_endpoints(val.bs.Licor$VPD          , val.bs.Licor$n, ind.CI);
    val.CI$Licor$La                  <- f_val_bs_CI_endpoints(val.bs.Licor$La           , val.bs.Licor$n, ind.CI);
    val.CI$Licor$StmRat              <- f_val_bs_CI_endpoints(val.bs.Licor$StmRat       , val.bs.Licor$n, ind.CI);
    val.CI$Licor$gbw                 <- f_val_bs_CI_endpoints(val.bs.Licor$gbw          , val.bs.Licor$n, ind.CI);
    val.CI$Licor$temp.air            <- f_val_bs_CI_endpoints(val.bs.Licor$temp.air     , val.bs.Licor$n, ind.CI);
    val.CI$Licor$temp.leaf           <- f_val_bs_CI_endpoints(val.bs.Licor$temp.leaf    , val.bs.Licor$n, ind.CI);
    val.CI$Licor$temp.block          <- f_val_bs_CI_endpoints(val.bs.Licor$temp.block   , val.bs.Licor$n, ind.CI);
    val.CI$Licor$Ce                  <- f_val_bs_CI_endpoints(val.bs.Licor$Ce           , val.bs.Licor$n, ind.CI);
    val.CI$Licor$Co                  <- f_val_bs_CI_endpoints(val.bs.Licor$Co           , val.bs.Licor$n, ind.CI);
    val.CI$Licor$xin                 <- f_val_bs_CI_endpoints(val.bs.Licor$xin          , val.bs.Licor$n, ind.CI);
    val.CI$Licor$xout                <- f_val_bs_CI_endpoints(val.bs.Licor$xout         , val.bs.Licor$n, ind.CI);
    val.CI$Licor$rh.ref              <- f_val_bs_CI_endpoints(val.bs.Licor$rh.ref       , val.bs.Licor$n, ind.CI);
    val.CI$Licor$rh.sam              <- f_val_bs_CI_endpoints(val.bs.Licor$rh.sam       , val.bs.Licor$n, ind.CI);
    val.CI$Licor$uin                 <- f_val_bs_CI_endpoints(val.bs.Licor$uin          , val.bs.Licor$n, ind.CI);
    val.CI$Licor$par.int             <- f_val_bs_CI_endpoints(val.bs.Licor$par.int      , val.bs.Licor$n, ind.CI);
    val.CI$Licor$par.ext             <- f_val_bs_CI_endpoints(val.bs.Licor$par.ext      , val.bs.Licor$n, ind.CI);
    val.CI$Licor$Atm.press           <- f_val_bs_CI_endpoints(val.bs.Licor$Atm.press    , val.bs.Licor$n, ind.CI);
    val.CI$Licor$CsMch               <- f_val_bs_CI_endpoints(val.bs.Licor$CsMch        , val.bs.Licor$n, ind.CI);
    val.CI$Licor$HsMch               <- f_val_bs_CI_endpoints(val.bs.Licor$HsMch        , val.bs.Licor$n, ind.CI);
    val.CI$Licor$StableF             <- f_val_bs_CI_endpoints(val.bs.Licor$StableF      , val.bs.Licor$n, ind.CI);
    val.CI$Licor$Status              <- f_val_bs_CI_endpoints(val.bs.Licor$Status       , val.bs.Licor$n, ind.CI);
    val.CI$Licor$VpdA                <- f_val_bs_CI_endpoints(val.bs.Licor$VpdA         , val.bs.Licor$n, ind.CI);
    val.CI$Licor$Ci.Ca               <- f_val_bs_CI_endpoints(val.bs.Licor$Ci.Ca        , val.bs.Licor$n, ind.CI);
    val.CI$Licor$pi                  <- f_val_bs_CI_endpoints(val.bs.Licor$pi           , val.bs.Licor$n, ind.CI);
    val.CI$Licor$uc_20_mV            <- f_val_bs_CI_endpoints(val.bs.Licor$uc_20_mV     , val.bs.Licor$n, ind.CI);
    val.CI$Licor$uc_21_mV            <- f_val_bs_CI_endpoints(val.bs.Licor$uc_21_mV     , val.bs.Licor$n, ind.CI);
    val.CI$Licor$U_S                 <- f_val_bs_CI_endpoints(val.bs.Licor$U_S          , val.bs.Licor$n, ind.CI);
    val.CI$Licor$Trans               <- f_val_bs_CI_endpoints(val.bs.Licor$Trans        , val.bs.Licor$n, ind.CI);
    val.CI$Licor$CndCO2              <- f_val_bs_CI_endpoints(val.bs.Licor$CndCO2       , val.bs.Licor$n, ind.CI);
    val.CI$Licor$Ref_mV              <- f_val_bs_CI_endpoints(val.bs.Licor$Ref_mV       , val.bs.Licor$n, ind.CI);
    val.CI$Licor$xTemp1              <- f_val_bs_CI_endpoints(val.bs.Licor$xTemp1       , val.bs.Licor$n, ind.CI);
    val.CI$Licor$xTemp2              <- f_val_bs_CI_endpoints(val.bs.Licor$xTemp2       , val.bs.Licor$n, ind.CI);
  }

  # Calc values
  val.CI$calc$gain.12C                                                <- f_val_bs_CI_endpoints(val.calc.bs$gain.12C                                              , val.bs.TDL$n, ind.CI);
  val.CI$calc$gain.13C                                                <- f_val_bs_CI_endpoints(val.calc.bs$gain.13C                                              , val.bs.TDL$n, ind.CI);
  val.CI$calc$offset.12C                                              <- f_val_bs_CI_endpoints(val.calc.bs$offset.12C                                            , val.bs.TDL$n, ind.CI);
  val.CI$calc$offset.13C                                              <- f_val_bs_CI_endpoints(val.calc.bs$offset.13C                                            , val.bs.TDL$n, ind.CI);
  val.CI$calc$reference.12Ce                                          <- f_val_bs_CI_endpoints(val.calc.bs$reference.12Ce                                        , val.bs.TDL$n, ind.CI);
  val.CI$calc$reference.13Ce                                          <- f_val_bs_CI_endpoints(val.calc.bs$reference.13Ce                                        , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12Co                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12Co                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13Co                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13Co                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$reference.TotalCe                                       <- f_val_bs_CI_endpoints(val.calc.bs$reference.TotalCe                                     , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.TotalCo                                         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.TotalCo                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.reference.Total.diff.CeCo                       <- f_val_bs_CI_endpoints(val.calc.bs$chamber.reference.Total.diff.CeCo                     , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.reference.12.diff.CeCo                          <- f_val_bs_CI_endpoints(val.calc.bs$chamber.reference.12.diff.CeCo                        , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.reference.13.diff.CeCo                          <- f_val_bs_CI_endpoints(val.calc.bs$chamber.reference.13.diff.CeCo                        , val.bs.TDL$n, ind.CI);
  val.CI$calc$xi                                                      <- f_val_bs_CI_endpoints(val.calc.bs$xi                                                    , val.bs.TDL$n, ind.CI);
  #val.CI$calc$flow.adjusted                                           <- f_val_bs_CI_endpoints(val.calc.bs$flow.adjusted                                         , val.bs.TDL$n, ind.CI); # 9/5/2012
  val.CI$calc$TDL.A.photosynthesis                                    <- f_val_bs_CI_endpoints(val.calc.bs$TDL.A.photosynthesis                                  , val.bs.TDL$n, ind.CI);
  val.CI$calc$TDL.12A.photosynthesis                                  <- f_val_bs_CI_endpoints(val.calc.bs$TDL.12A.photosynthesis                                , val.bs.TDL$n, ind.CI);
  val.CI$calc$TDL.13A.photosynthesis                                  <- f_val_bs_CI_endpoints(val.calc.bs$TDL.13A.photosynthesis                                , val.bs.TDL$n, ind.CI);
  val.CI$calc$Licor.A.photosynthesis                                  <- f_val_bs_CI_endpoints(val.calc.bs$Licor.A.photosynthesis                                , val.bs.TDL$n, ind.CI);
  val.CI$calc$Delta.from.ratios.in.out                                <- f_val_bs_CI_endpoints(val.calc.bs$Delta.from.ratios.in.out                              , val.bs.TDL$n, ind.CI);
  val.CI$calc$Delta.from.A.ratio                                      <- f_val_bs_CI_endpoints(val.calc.bs$Delta.from.A.ratio                                    , val.bs.TDL$n, ind.CI);
  val.CI$calc$VPD                                                     <- f_val_bs_CI_endpoints(val.calc.bs$VPD                                                   , val.bs.TDL$n, ind.CI);
  val.CI$calc$E.transpiration                                         <- f_val_bs_CI_endpoints(val.calc.bs$E.transpiration                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$leaf.temp                                               <- f_val_bs_CI_endpoints(val.calc.bs$leaf.temp                                             , val.bs.TDL$n, ind.CI);
  val.CI$calc$air.temp                                                <- f_val_bs_CI_endpoints(val.calc.bs$air.temp                                              , val.bs.TDL$n, ind.CI);
  val.CI$calc$light.in                                                <- f_val_bs_CI_endpoints(val.calc.bs$light.in                                              , val.bs.TDL$n, ind.CI);
  val.CI$calc$light.out                                               <- f_val_bs_CI_endpoints(val.calc.bs$light.out                                             , val.bs.TDL$n, ind.CI);
  val.CI$calc$reference.delta.e                                       <- f_val_bs_CI_endpoints(val.calc.bs$reference.delta.e                                     , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.delta.o                                         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.delta.o                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.reference.delta.diff.CoCe                       <- f_val_bs_CI_endpoints(val.calc.bs$chamber.reference.delta.diff.CoCe                     , val.bs.TDL$n, ind.CI);
  val.CI$calc$Delta.obs                                               <- f_val_bs_CI_endpoints(val.calc.bs$Delta.obs                                             , val.bs.TDL$n, ind.CI);
  val.CI$calc$Delta.obs.permil                                        <- f_val_bs_CI_endpoints(val.calc.bs$Delta.obs.permil                                      , val.bs.TDL$n, ind.CI);
  val.CI$calc$delta.13C.Assim                                         <- f_val_bs_CI_endpoints(val.calc.bs$delta.13C.Assim                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$p                                                       <- f_val_bs_CI_endpoints(val.calc.bs$p                                                     , val.bs.TDL$n, ind.CI);
  val.CI$calc$delta.13C.Resp                                          <- f_val_bs_CI_endpoints(val.calc.bs$delta.13C.Resp                                        , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.TotalCa                                         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.TotalCa                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12Ca                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12Ca                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13Ca                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13Ca                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.TotalCs                                         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.TotalCs                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12Cs                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12Cs                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13Cs                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13Cs                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalpa                                         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalpa                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12pa                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12pa                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13pa                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13pa                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalps                                         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalps                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12ps                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12ps                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13ps                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13ps                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalgbw                                        <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalgbw                                      , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalgbc                                        <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalgbc                                      , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12gbc                                           <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12gbc                                         , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13gbc                                           <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13gbc                                         , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalgsw                                        <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalgsw                                      , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalgsc                                        <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalgsc                                      , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12gsc                                           <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12gsc                                         , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13gsc                                           <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13gsc                                         , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalgtc                                        <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalgtc                                      , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12gtc                                           <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12gtc                                         , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13gtc                                           <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13gtc                                         , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.TotalCi                                         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.TotalCi                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12Ci                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12Ci                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13Ci                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13Ci                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalpi                                         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalpi                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12pi                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12pi                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13pi                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13pi                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalpi_pa                                      <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalpi_pa                                    , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Delta.i.simple.for.gm                           <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Delta.i.simple.for.gm                         , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Delta.i.simple.for.modeling                     <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Delta.i.simple.for.modeling                   , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Delta.i.complex.for.gm                          <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Delta.i.complex.for.gm                        , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Delta.i.simple.for.gm_Delta.obs                 <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Delta.i.simple.for.gm_Delta.obs               , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Delta.i.complex.for.gm_Delta.obs                <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Delta.i.complex.for.gm_Delta.obs              , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalgm.point.simple                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalgm.point.simple                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12gm.point.simple                               <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12gm.point.simple                             , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13gm.point.simple                               <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13gm.point.simple                             , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalgm.point.complex                           <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalgm.point.complex                         , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalgm.to.use                                  <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalgm.to.use                                , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalpc.using.gm                                <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalpc.using.gm                              , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12pc.using.gm                                   <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12pc.using.gm                                 , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13pc.using.gm                                   <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13pc.using.gm                                 , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalpc.using.simple.Delta.for.gm               <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalpc.using.simple.Delta.for.gm             , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalpc.using.simple.Delta.for.modeling         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalpc.using.simple.Delta.for.modeling       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalpc.using.complex.Delta.no.decarboxylation  <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalpc.using.complex.Delta.no.decarboxylation, val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalpc.using.complex.Delta.full.model          <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalpc.using.complex.Delta.full.model        , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.Totalpc.to.use                                  <- f_val_bs_CI_endpoints(val.calc.bs$chamber.Totalpc.to.use                                , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.TotalCc                                         <- f_val_bs_CI_endpoints(val.calc.bs$chamber.TotalCc                                       , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.12Cc                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.12Cc                                          , val.bs.TDL$n, ind.CI);
  val.CI$calc$chamber.13Cc                                            <- f_val_bs_CI_endpoints(val.calc.bs$chamber.13Cc                                          , val.bs.TDL$n, ind.CI);

  return( val.CI );
  ### val.CI
}

