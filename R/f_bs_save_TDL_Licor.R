#' Title
#'
#' @param val.bs.sum
#' @param val.bs
#' @param i.bs
#' @param R.bootstrap
#' @param sw
#'
#' @return
#' @export
#'
#' @examples
f_bs_save_TDL_Licor <-
function# save BS resample of TDL and Licor means
###
(val.bs.sum
###
, val.bs
###
, i.bs
###
, R.bootstrap
###
, sw
###
)
{
  ## Debug
  # val.bs.sum  <- val.bs.sum
  # val.bs      <- val$bs
  # i.bs        <- i.bs
  # R.bootstrap <- R.bootstrap

  ##details<<
  ## create val.bs$TDL and $Licor for calculated observed values to use in NP bootstrap
  if (i.bs == 1) {
    val.bs$TDL    <- as.list(new.env()); # for calculated observed values to use in NP bootstrap
    val.bs$Licor  <- as.list(new.env()); # for calculated observed values to use in NP bootstrap

    ##details<<
    ## Initialize values to zeros.
    zeros <- matrix(0, nrow=val.bs.sum$TDL$n, ncol=R.bootstrap);
    # TDL
    if (sw$use.TDL) {
      val.bs$TDL$n                     <- val.bs.sum$TDL$n        ;
      val.bs$TDL$first.ind             <- val.bs.sum$TDL$first.ind;
      val.bs$TDL$ind                   <- val.bs.sum$TDL$ind      ;
      val.bs$TDL$site                  <- val.bs.sum$TDL$site     ;
      val.bs$TDL$n.sam                 <- val.bs.sum$TDL$n.sam    ;
      val.bs$TDL$time                  <- val.bs.sum$TDL$time     ;
      val.bs$TDL$conc12CO2             <- zeros;
      val.bs$TDL$conc13CO2             <- zeros;
      val.bs$TDL$TGApressure           <- zeros;
      val.bs$TDL$MassFlow1             <- zeros;
      val.bs$TDL$Pressure1             <- zeros;
      val.bs$TDL$MassFlow2             <- zeros;
      val.bs$TDL$Pressure2             <- zeros;
      val.bs$TDL$PressureProMan        <- zeros;
      val.bs$TDL$interp.tank.hi.12     <- zeros;
      val.bs$TDL$interp.tank.hi.13     <- zeros;
      val.bs$TDL$interp.tank.low.12    <- zeros;
      val.bs$TDL$interp.tank.low.13    <- zeros;
      val.bs$TDL$interp.reference.12   <- zeros;
      val.bs$TDL$interp.reference.13   <- zeros;
      val.bs$TDL$chamber.12            <- zeros;
      val.bs$TDL$chamber.13            <- zeros;
    }

    # Licor
    if (sw$use.Licor) {
      val.bs$Licor$n                   <- val.bs.sum$Licor$n        ;
      val.bs$Licor$first.ind           <- val.bs.sum$Licor$first.ind;
      val.bs$Licor$ind                 <- val.bs.sum$Licor$ind      ;
      val.bs$Licor$site                <- val.bs.sum$Licor$site     ;
      val.bs$Licor$time                <- val.bs.sum$Licor$time     ;
      val.bs$Licor$FTime               <- zeros;
      val.bs$Licor$A                   <- zeros;
      val.bs$Licor$gsc                 <- zeros;
      val.bs$Licor$Ci                  <- zeros;
      val.bs$Licor$E                   <- zeros;
      val.bs$Licor$VPD                 <- zeros;
      val.bs$Licor$La                  <- zeros;
      val.bs$Licor$StmRat              <- zeros;
      val.bs$Licor$gbw                 <- zeros;
      val.bs$Licor$temp.air            <- zeros;
      val.bs$Licor$temp.leaf           <- zeros;
      val.bs$Licor$temp.block          <- zeros;
      val.bs$Licor$Ce                  <- zeros;
      val.bs$Licor$Co                  <- zeros;
      val.bs$Licor$xin                 <- zeros;
      val.bs$Licor$xout                <- zeros;
      val.bs$Licor$rh.ref              <- zeros;
      val.bs$Licor$rh.sam              <- zeros;
      val.bs$Licor$uin                 <- zeros;
      val.bs$Licor$par.int             <- zeros;
      val.bs$Licor$par.ext             <- zeros;
      val.bs$Licor$Atm.press           <- zeros;
      val.bs$Licor$CsMch               <- zeros;
      val.bs$Licor$HsMch               <- zeros;
      val.bs$Licor$StableF             <- zeros;
      val.bs$Licor$Status              <- zeros;
      val.bs$Licor$VpdA                <- zeros;
      val.bs$Licor$Ci.Ca               <- zeros;
      val.bs$Licor$pi                  <- zeros;
      val.bs$Licor$uc_20_mV            <- zeros;
      val.bs$Licor$uc_21_mV            <- zeros;
      val.bs$Licor$U_S                 <- zeros;
      val.bs$Licor$Trans               <- zeros;
      val.bs$Licor$CndCO2              <- zeros;
      val.bs$Licor$Ref_mV              <- zeros;
      val.bs$Licor$xTemp1              <- zeros;
      val.bs$Licor$xTemp2              <- zeros;
    }
  }

  ##details<<
  ## If using TDL, copy val.bs.sum$TDL$* to val.bs$TDL$*
  # TDL
  if (sw$use.TDL) {
    val.bs$TDL$conc12CO2           [,i.bs]  <- val.bs.sum$TDL$conc12CO2           ;
    val.bs$TDL$conc13CO2           [,i.bs]  <- val.bs.sum$TDL$conc13CO2           ;
    val.bs$TDL$TGApressure         [,i.bs]  <- val.bs.sum$TDL$TGApressure         ;
    val.bs$TDL$MassFlow1           [,i.bs]  <- val.bs.sum$TDL$MassFlow1           ;
    val.bs$TDL$Pressure1           [,i.bs]  <- val.bs.sum$TDL$Pressure1           ;
    val.bs$TDL$MassFlow2           [,i.bs]  <- val.bs.sum$TDL$MassFlow2           ;
    val.bs$TDL$Pressure2           [,i.bs]  <- val.bs.sum$TDL$Pressure2           ;
    val.bs$TDL$PressureProMan      [,i.bs]  <- val.bs.sum$TDL$PressureProMan      ;
    val.bs$TDL$interp.tank.hi.12   [,i.bs]  <- val.bs.sum$TDL$interp.tank.hi.12   ;
    val.bs$TDL$interp.tank.hi.13   [,i.bs]  <- val.bs.sum$TDL$interp.tank.hi.13   ;
    val.bs$TDL$interp.tank.low.12  [,i.bs]  <- val.bs.sum$TDL$interp.tank.low.12  ;
    val.bs$TDL$interp.tank.low.13  [,i.bs]  <- val.bs.sum$TDL$interp.tank.low.13  ;
    val.bs$TDL$interp.reference.12 [,i.bs]  <- val.bs.sum$TDL$interp.reference.12 ;
    val.bs$TDL$interp.reference.13 [,i.bs]  <- val.bs.sum$TDL$interp.reference.13 ;
    val.bs$TDL$chamber.12          [,i.bs]  <- val.bs.sum$TDL$chamber.12          ;
    val.bs$TDL$chamber.13          [,i.bs]  <- val.bs.sum$TDL$chamber.13          ;
  }

  ##details<<
  ## If using Licor, copy val.bs.sum$Licor$* to val.bs$Licor$*
  # Licor
  if (sw$use.Licor) {
    # Still need to determine what to do about BS for Licor data
    val.bs$Licor$FTime             [,i.bs]  <- val.bs.sum$Licor$FTime             ;
    val.bs$Licor$A                 [,i.bs]  <- val.bs.sum$Licor$A                 ;
    val.bs$Licor$gsc               [,i.bs]  <- val.bs.sum$Licor$gsc               ;
    val.bs$Licor$Ci                [,i.bs]  <- val.bs.sum$Licor$Ci                ;
    val.bs$Licor$E                 [,i.bs]  <- val.bs.sum$Licor$E                 ;
    val.bs$Licor$VPD               [,i.bs]  <- val.bs.sum$Licor$VPD               ;
    val.bs$Licor$La                [,i.bs]  <- val.bs.sum$Licor$La                ;
    val.bs$Licor$StmRat            [,i.bs]  <- val.bs.sum$Licor$StmRat            ;
    val.bs$Licor$gbw               [,i.bs]  <- val.bs.sum$Licor$gbw               ;
    val.bs$Licor$temp.air          [,i.bs]  <- val.bs.sum$Licor$temp.air          ;
    val.bs$Licor$temp.leaf         [,i.bs]  <- val.bs.sum$Licor$temp.leaf         ;
    val.bs$Licor$temp.block        [,i.bs]  <- val.bs.sum$Licor$temp.block        ;
    val.bs$Licor$Ce                [,i.bs]  <- val.bs.sum$Licor$Ce                ;
    val.bs$Licor$Co                [,i.bs]  <- val.bs.sum$Licor$Co                ;
    val.bs$Licor$xin               [,i.bs]  <- val.bs.sum$Licor$xin               ;
    val.bs$Licor$xout              [,i.bs]  <- val.bs.sum$Licor$xout              ;
    val.bs$Licor$rh.ref            [,i.bs]  <- val.bs.sum$Licor$rh.ref            ;
    val.bs$Licor$rh.sam            [,i.bs]  <- val.bs.sum$Licor$rh.sam            ;
    val.bs$Licor$uin               [,i.bs]  <- val.bs.sum$Licor$uin               ;
    val.bs$Licor$par.int           [,i.bs]  <- val.bs.sum$Licor$par.int           ;
    val.bs$Licor$par.ext           [,i.bs]  <- val.bs.sum$Licor$par.ext           ;
    val.bs$Licor$Atm.press         [,i.bs]  <- val.bs.sum$Licor$Atm.press         ;
    val.bs$Licor$CsMch             [,i.bs]  <- val.bs.sum$Licor$CsMch             ;
    val.bs$Licor$HsMch             [,i.bs]  <- val.bs.sum$Licor$HsMch             ;
    val.bs$Licor$StableF           [,i.bs]  <- val.bs.sum$Licor$StableF           ;
    val.bs$Licor$Status            [,i.bs]  <- val.bs.sum$Licor$Status            ;
    val.bs$Licor$VpdA              [,i.bs]  <- val.bs.sum$Licor$VpdA              ;
    val.bs$Licor$Ci.Ca             [,i.bs]  <- val.bs.sum$Licor$Ci.Ca             ;
    val.bs$Licor$pi                [,i.bs]  <- val.bs.sum$Licor$pi                ;
    val.bs$Licor$uc_20_mV          [,i.bs]  <- val.bs.sum$Licor$uc_20_mV          ;
    val.bs$Licor$uc_21_mV          [,i.bs]  <- val.bs.sum$Licor$uc_21_mV          ;
    val.bs$Licor$U_S               [,i.bs]  <- val.bs.sum$Licor$U_S               ;
    val.bs$Licor$Trans             [,i.bs]  <- val.bs.sum$Licor$Trans             ;
    val.bs$Licor$CndCO2            [,i.bs]  <- val.bs.sum$Licor$CndCO2            ;
    val.bs$Licor$Ref_mV            [,i.bs]  <- val.bs.sum$Licor$Ref_mV            ;
    val.bs$Licor$xTemp1            [,i.bs]  <- val.bs.sum$Licor$xTemp1            ;
    val.bs$Licor$xTemp2            [,i.bs]  <- val.bs.sum$Licor$xTemp2            ;
  }

  return( val.bs );
  ### val.bs
}

