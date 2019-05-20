#' one BS resample of TDL and Licor means
#'
#' create val_bs_sum for calculated observed values to use in NP bootstrap
#'
#' TDL using \code{\link{f_np_bs_mean}}
#'
#' Licor
#' XXX Still need to determine what to do about BS for Licor data
#'
#' @param val_obs_TDL
#' @param val_sum_TDL
#' @param val_obs_Licor
#' @param val_sum_Licor
#' @param var_interp
#'
#' @return val_bs_sum
#' @export
#'
#' @examples
f_bs_iter_TDL_Licor <-
function# one BS resample of TDL and Licor means
###
(val_obs_TDL
###
, val_sum_TDL
###
, val_obs_Licor
###
, val_sum_Licor
###
, var_interp
###
)
{

  ##details<<
  ## create val_bs_sum for calculated observed values to use in NP bootstrap
  val_bs_sum        <- as.list(new.env()); # for calculated observed values to use in NP bootstrap
  val_bs_sum$TDL    <- as.list(new.env()); # for calculated observed values to use in NP bootstrap
  val_bs_sum$Licor  <- as.list(new.env()); # for calculated observed values to use in NP bootstrap

  ##details<<
  ## TDL using \code{\link{f_np_bs_mean}}
  val_bs_sum$TDL$n                     <- val_sum_TDL$n;
  val_bs_sum$TDL$first_ind             <- val_sum_TDL$first_ind;
  val_bs_sum$TDL$ind                   <- val_sum_TDL$ind;
  val_bs_sum$TDL$site                  <- val_sum_TDL$site;
  val_bs_sum$TDL$n_sam                 <- val_sum_TDL$n_sam;
  val_bs_sum$TDL$time                  <- val_sum_TDL$time;
  val_bs_sum$TDL$conc12CO2             <- f_np_bs_mean (val_obs_TDL$conc12CO2          , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$conc13CO2             <- f_np_bs_mean (val_obs_TDL$conc13CO2          , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$TGApressure           <- f_np_bs_mean (val_obs_TDL$TGApressure        , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$MassFlow1             <- f_np_bs_mean (val_obs_TDL$MassFlow1          , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$Pressure1             <- f_np_bs_mean (val_obs_TDL$Pressure1          , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$MassFlow2             <- f_np_bs_mean (val_obs_TDL$MassFlow2          , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$Pressure2             <- f_np_bs_mean (val_obs_TDL$Pressure2          , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$PressureProMan        <- f_np_bs_mean (val_obs_TDL$PressureProMan     , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$interp_tank_hi_12     <- f_par_bs_mean(val_sum_TDL$interp_tank_hi_12  , var_interp$tank_hi_12   , val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$interp_tank_hi_13     <- f_par_bs_mean(val_sum_TDL$interp_tank_hi_13  , var_interp$tank_hi_13   , val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$interp_tank_low_12    <- f_par_bs_mean(val_sum_TDL$interp_tank_low_12 , var_interp$tank_low_12  , val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$interp_tank_low_13    <- f_par_bs_mean(val_sum_TDL$interp_tank_low_13 , var_interp$tank_low_13  , val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$interp_reference_12   <- f_par_bs_mean(val_sum_TDL$interp_reference_12, var_interp$reference_12 , val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$interp_reference_13   <- f_par_bs_mean(val_sum_TDL$interp_reference_13, var_interp$reference_13 , val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$chamber_12            <- f_np_bs_mean (val_obs_TDL$chamber_12         , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);
  val_bs_sum$TDL$chamber_13            <- f_np_bs_mean (val_obs_TDL$chamber_13         , val_bs_sum$TDL$first_ind, val_bs_sum$TDL$ind, val_bs_sum$TDL$n_sam, val_bs_sum$TDL$n);

  ##details<<
  ## Licor
  ## XXX Still need to determine what to do about BS for Licor data
  val_bs_sum$Licor$n                   <- val_sum_Licor$n           ;
  val_bs_sum$Licor$first_ind           <- val_sum_Licor$first_ind   ;
  val_bs_sum$Licor$ind                 <- val_sum_Licor$ind         ;
  val_bs_sum$Licor$site                <- val_sum_Licor$site        ;
  val_bs_sum$Licor$time                <- val_sum_Licor$time        ;
  val_bs_sum$Licor$FTime               <- val_sum_Licor$FTime       ;
  val_bs_sum$Licor$A                   <- val_sum_Licor$A           ;
  val_bs_sum$Licor$gsc                 <- val_sum_Licor$gsc         ;
  val_bs_sum$Licor$Ci                  <- val_sum_Licor$Ci          ;
  val_bs_sum$Licor$E                   <- val_sum_Licor$E           ;
  val_bs_sum$Licor$VPD                 <- val_sum_Licor$VPD         ;
  val_bs_sum$Licor$La                  <- val_sum_Licor$La          ;
  val_bs_sum$Licor$StmRat              <- val_sum_Licor$StmRat      ;
  val_bs_sum$Licor$gbw                 <- val_sum_Licor$gbw         ;
  val_bs_sum$Licor$temp_air            <- val_sum_Licor$temp_air    ;
  val_bs_sum$Licor$temp_leaf           <- val_sum_Licor$temp_leaf   ;
  val_bs_sum$Licor$temp_block          <- val_sum_Licor$temp_block  ;
  val_bs_sum$Licor$Ce                  <- val_sum_Licor$Ce          ;
  val_bs_sum$Licor$Co                  <- val_sum_Licor$Co          ;
  val_bs_sum$Licor$xin                 <- val_sum_Licor$xin         ;
  val_bs_sum$Licor$xout                <- val_sum_Licor$xout        ;
  val_bs_sum$Licor$rh_ref              <- val_sum_Licor$rh_ref      ;
  val_bs_sum$Licor$rh_sam              <- val_sum_Licor$rh_sam      ;
  val_bs_sum$Licor$uin                 <- val_sum_Licor$uin         ;
  val_bs_sum$Licor$par_int             <- val_sum_Licor$par_int     ;
  val_bs_sum$Licor$par_ext             <- val_sum_Licor$par_ext     ;
  val_bs_sum$Licor$Atm_press           <- val_sum_Licor$Atm_press   ;
  val_bs_sum$Licor$CsMch               <- val_sum_Licor$CsMch       ;
  val_bs_sum$Licor$HsMch               <- val_sum_Licor$HsMch       ;
  val_bs_sum$Licor$StableF             <- val_sum_Licor$StableF     ;
  val_bs_sum$Licor$Status              <- val_sum_Licor$Status      ;
  val_bs_sum$Licor$VpdA                <- val_sum_Licor$VpdA        ;
  val_bs_sum$Licor$Ci_Ca               <- val_sum_Licor$Ci_Ca       ;
  val_bs_sum$Licor$pi                  <- val_sum_Licor$pi          ;
  val_bs_sum$Licor$uc_20_mV            <- val_sum_Licor$uc_20_mV    ;
  val_bs_sum$Licor$uc_21_mV            <- val_sum_Licor$uc_21_mV    ;
  val_bs_sum$Licor$U_S                 <- val_sum_Licor$U_S         ;
  val_bs_sum$Licor$Trans               <- val_sum_Licor$Trans       ;
  val_bs_sum$Licor$CndCO2              <- val_sum_Licor$CndCO2      ;
  val_bs_sum$Licor$Ref_mV              <- val_sum_Licor$Ref_mV      ;
  val_bs_sum$Licor$xTemp1              <- val_sum_Licor$xTemp1      ;
  val_bs_sum$Licor$xTemp2              <- val_sum_Licor$xTemp2      ;

  return( val_bs_sum );
  ### val_bs_sum
}

