#' Title
#'
#' @param val_bs_sum
#' @param val_bs
#' @param i_bs
#' @param R_bootstrap
#' @param sw
#'
#' @return
#' @export
#'
#' @examples
f_bs_save_TDL_Licor <-
function# save BS resample of TDL and Licor means
###
(val_bs_sum
###
, val_bs
###
, i_bs
###
, R_bootstrap
###
, sw
###
)
{
  ## Debug
  # val_bs_sum  <- val_bs_sum
  # val_bs      <- val$bs
  # i_bs        <- i_bs
  # R_bootstrap <- R_bootstrap

  ##details<<
  ## create val_bs$TDL and $Licor for calculated observed values to use in NP bootstrap
  if (i_bs == 1) {
    val_bs$TDL    <- as.list(new.env()); # for calculated observed values to use in NP bootstrap
    val_bs$Licor  <- as.list(new.env()); # for calculated observed values to use in NP bootstrap

    ##details<<
    ## Initialize values to zeros.
    zeros <- matrix(0, nrow=val_bs_sum$TDL$n, ncol=R_bootstrap);
    # TDL
    if (sw$use_TDL) {
      val_bs$TDL$n                     <- val_bs_sum$TDL$n        ;
      val_bs$TDL$first_ind             <- val_bs_sum$TDL$first_ind;
      val_bs$TDL$ind                   <- val_bs_sum$TDL$ind      ;
      val_bs$TDL$site                  <- val_bs_sum$TDL$site     ;
      val_bs$TDL$n_sam                 <- val_bs_sum$TDL$n_sam    ;
      val_bs$TDL$time                  <- val_bs_sum$TDL$time     ;
      val_bs$TDL$conc12CO2             <- zeros;
      val_bs$TDL$conc13CO2             <- zeros;
      val_bs$TDL$TGApressure           <- zeros;
      val_bs$TDL$MassFlow1             <- zeros;
      val_bs$TDL$Pressure1             <- zeros;
      val_bs$TDL$MassFlow2             <- zeros;
      val_bs$TDL$Pressure2             <- zeros;
      val_bs$TDL$PressureProMan        <- zeros;
      val_bs$TDL$interp_tank_hi_12     <- zeros;
      val_bs$TDL$interp_tank_hi_13     <- zeros;
      val_bs$TDL$interp_tank_low_12    <- zeros;
      val_bs$TDL$interp_tank_low_13    <- zeros;
      val_bs$TDL$interp_reference_12   <- zeros;
      val_bs$TDL$interp_reference_13   <- zeros;
      val_bs$TDL$chamber_12            <- zeros;
      val_bs$TDL$chamber_13            <- zeros;
    }

    # Licor
    if (sw$use_Licor) {
      val_bs$Licor$n                   <- val_bs_sum$Licor$n        ;
      val_bs$Licor$first_ind           <- val_bs_sum$Licor$first_ind;
      val_bs$Licor$ind                 <- val_bs_sum$Licor$ind      ;
      val_bs$Licor$site                <- val_bs_sum$Licor$site     ;
      val_bs$Licor$time                <- val_bs_sum$Licor$time     ;
      val_bs$Licor$FTime               <- zeros;
      val_bs$Licor$A                   <- zeros;
      val_bs$Licor$gsc                 <- zeros;
      val_bs$Licor$Ci                  <- zeros;
      val_bs$Licor$E                   <- zeros;
      val_bs$Licor$VPD                 <- zeros;
      val_bs$Licor$La                  <- zeros;
      val_bs$Licor$StmRat              <- zeros;
      val_bs$Licor$gbw                 <- zeros;
      val_bs$Licor$temp_air            <- zeros;
      val_bs$Licor$temp_leaf           <- zeros;
      val_bs$Licor$temp_block          <- zeros;
      val_bs$Licor$Ce                  <- zeros;
      val_bs$Licor$Co                  <- zeros;
      val_bs$Licor$xin                 <- zeros;
      val_bs$Licor$xout                <- zeros;
      val_bs$Licor$rh_ref              <- zeros;
      val_bs$Licor$rh_sam              <- zeros;
      val_bs$Licor$uin                 <- zeros;
      val_bs$Licor$par_int             <- zeros;
      val_bs$Licor$par_ext             <- zeros;
      val_bs$Licor$Atm_press           <- zeros;
      val_bs$Licor$CsMch               <- zeros;
      val_bs$Licor$HsMch               <- zeros;
      val_bs$Licor$StableF             <- zeros;
      val_bs$Licor$Status              <- zeros;
      val_bs$Licor$VpdA                <- zeros;
      val_bs$Licor$Ci_Ca               <- zeros;
      val_bs$Licor$pi                  <- zeros;
      val_bs$Licor$uc_20_mV            <- zeros;
      val_bs$Licor$uc_21_mV            <- zeros;
      val_bs$Licor$U_S                 <- zeros;
      val_bs$Licor$Trans               <- zeros;
      val_bs$Licor$CndCO2              <- zeros;
      val_bs$Licor$Ref_mV              <- zeros;
      val_bs$Licor$xTemp1              <- zeros;
      val_bs$Licor$xTemp2              <- zeros;
    }
  }

  ##details<<
  ## If using TDL, copy val_bs_sum$TDL$* to val_bs$TDL$*
  # TDL
  if (sw$use_TDL) {
    val_bs$TDL$conc12CO2           [,i_bs]  <- val_bs_sum$TDL$conc12CO2           ;
    val_bs$TDL$conc13CO2           [,i_bs]  <- val_bs_sum$TDL$conc13CO2           ;
    val_bs$TDL$TGApressure         [,i_bs]  <- val_bs_sum$TDL$TGApressure         ;
    val_bs$TDL$MassFlow1           [,i_bs]  <- val_bs_sum$TDL$MassFlow1           ;
    val_bs$TDL$Pressure1           [,i_bs]  <- val_bs_sum$TDL$Pressure1           ;
    val_bs$TDL$MassFlow2           [,i_bs]  <- val_bs_sum$TDL$MassFlow2           ;
    val_bs$TDL$Pressure2           [,i_bs]  <- val_bs_sum$TDL$Pressure2           ;
    val_bs$TDL$PressureProMan      [,i_bs]  <- val_bs_sum$TDL$PressureProMan      ;
    val_bs$TDL$interp_tank_hi_12   [,i_bs]  <- val_bs_sum$TDL$interp_tank_hi_12   ;
    val_bs$TDL$interp_tank_hi_13   [,i_bs]  <- val_bs_sum$TDL$interp_tank_hi_13   ;
    val_bs$TDL$interp_tank_low_12  [,i_bs]  <- val_bs_sum$TDL$interp_tank_low_12  ;
    val_bs$TDL$interp_tank_low_13  [,i_bs]  <- val_bs_sum$TDL$interp_tank_low_13  ;
    val_bs$TDL$interp_reference_12 [,i_bs]  <- val_bs_sum$TDL$interp_reference_12 ;
    val_bs$TDL$interp_reference_13 [,i_bs]  <- val_bs_sum$TDL$interp_reference_13 ;
    val_bs$TDL$chamber_12          [,i_bs]  <- val_bs_sum$TDL$chamber_12          ;
    val_bs$TDL$chamber_13          [,i_bs]  <- val_bs_sum$TDL$chamber_13          ;
  }

  ##details<<
  ## If using Licor, copy val_bs_sum$Licor$* to val_bs$Licor$*
  # Licor
  if (sw$use_Licor) {
    # Still need to determine what to do about BS for Licor data
    val_bs$Licor$FTime             [,i_bs]  <- val_bs_sum$Licor$FTime             ;
    val_bs$Licor$A                 [,i_bs]  <- val_bs_sum$Licor$A                 ;
    val_bs$Licor$gsc               [,i_bs]  <- val_bs_sum$Licor$gsc               ;
    val_bs$Licor$Ci                [,i_bs]  <- val_bs_sum$Licor$Ci                ;
    val_bs$Licor$E                 [,i_bs]  <- val_bs_sum$Licor$E                 ;
    val_bs$Licor$VPD               [,i_bs]  <- val_bs_sum$Licor$VPD               ;
    val_bs$Licor$La                [,i_bs]  <- val_bs_sum$Licor$La                ;
    val_bs$Licor$StmRat            [,i_bs]  <- val_bs_sum$Licor$StmRat            ;
    val_bs$Licor$gbw               [,i_bs]  <- val_bs_sum$Licor$gbw               ;
    val_bs$Licor$temp_air          [,i_bs]  <- val_bs_sum$Licor$temp_air          ;
    val_bs$Licor$temp_leaf         [,i_bs]  <- val_bs_sum$Licor$temp_leaf         ;
    val_bs$Licor$temp_block        [,i_bs]  <- val_bs_sum$Licor$temp_block        ;
    val_bs$Licor$Ce                [,i_bs]  <- val_bs_sum$Licor$Ce                ;
    val_bs$Licor$Co                [,i_bs]  <- val_bs_sum$Licor$Co                ;
    val_bs$Licor$xin               [,i_bs]  <- val_bs_sum$Licor$xin               ;
    val_bs$Licor$xout              [,i_bs]  <- val_bs_sum$Licor$xout              ;
    val_bs$Licor$rh_ref            [,i_bs]  <- val_bs_sum$Licor$rh_ref            ;
    val_bs$Licor$rh_sam            [,i_bs]  <- val_bs_sum$Licor$rh_sam            ;
    val_bs$Licor$uin               [,i_bs]  <- val_bs_sum$Licor$uin               ;
    val_bs$Licor$par_int           [,i_bs]  <- val_bs_sum$Licor$par_int           ;
    val_bs$Licor$par_ext           [,i_bs]  <- val_bs_sum$Licor$par_ext           ;
    val_bs$Licor$Atm_press         [,i_bs]  <- val_bs_sum$Licor$Atm_press         ;
    val_bs$Licor$CsMch             [,i_bs]  <- val_bs_sum$Licor$CsMch             ;
    val_bs$Licor$HsMch             [,i_bs]  <- val_bs_sum$Licor$HsMch             ;
    val_bs$Licor$StableF           [,i_bs]  <- val_bs_sum$Licor$StableF           ;
    val_bs$Licor$Status            [,i_bs]  <- val_bs_sum$Licor$Status            ;
    val_bs$Licor$VpdA              [,i_bs]  <- val_bs_sum$Licor$VpdA              ;
    val_bs$Licor$Ci_Ca             [,i_bs]  <- val_bs_sum$Licor$Ci_Ca             ;
    val_bs$Licor$pi                [,i_bs]  <- val_bs_sum$Licor$pi                ;
    val_bs$Licor$uc_20_mV          [,i_bs]  <- val_bs_sum$Licor$uc_20_mV          ;
    val_bs$Licor$uc_21_mV          [,i_bs]  <- val_bs_sum$Licor$uc_21_mV          ;
    val_bs$Licor$U_S               [,i_bs]  <- val_bs_sum$Licor$U_S               ;
    val_bs$Licor$Trans             [,i_bs]  <- val_bs_sum$Licor$Trans             ;
    val_bs$Licor$CndCO2            [,i_bs]  <- val_bs_sum$Licor$CndCO2            ;
    val_bs$Licor$Ref_mV            [,i_bs]  <- val_bs_sum$Licor$Ref_mV            ;
    val_bs$Licor$xTemp1            [,i_bs]  <- val_bs_sum$Licor$xTemp1            ;
    val_bs$Licor$xTemp2            [,i_bs]  <- val_bs_sum$Licor$xTemp2            ;
  }

  return( val_bs );
  ### val_bs
}

