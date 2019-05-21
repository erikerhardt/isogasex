#' create CI for each calculated value
#'
#' Create val_CI CI endpoints for $TDL, $Licor, and $calc.
#'
#' Calculate quantile indices of sorted data for sig_CI.
#'
#' For reasonable variables, calculates CIs using \code{\link{f_val_bs_CI_endpoints}}.
#'
#' @param val_calc_bs
#' @param val_bs_TDL
#' @param val_bs_Licor
#' @param R_bootstrap
#' @param sig_CI
#' @param sw
#'
#' @return val_CI
#'
#' @examples
f_val_bs_CI <-
function# create CI for each calculated value
###
(val_calc_bs
###
, val_bs_TDL
###
, val_bs_Licor
###
, R_bootstrap
###
, sig_CI
###
, sw
###
)
{

  ##details<<
  ## Create val_CI CI endpoints for $TDL, $Licor, and $calc.

  val_CI              <- as.list(new.env());
  val_CI$TDL          <- as.list(new.env());
  val_CI$Licor        <- as.list(new.env());
  val_CI$calc         <- as.list(new.env());

  ##details<<
  ## Calculate quantile indices of sorted data for sig_CI.
  ind_CI <- c( floor  (R_bootstrap * (sig_CI / 2 ))
              ,ceiling(R_bootstrap * (1 - sig_CI / 2)));

  ##details<<
  ## For reasonable variables, calculates CIs using \code{\link{f_val_bs_CI_endpoints}}.

  # TDL
  if (sw$use_TDL) {
    val_CI$TDL$n                     <- val_bs_TDL$n
    val_CI$TDL$first_ind             <- val_bs_TDL$first_ind
    val_CI$TDL$ind                   <- val_bs_TDL$ind
    val_CI$TDL$site                  <- val_bs_TDL$site
    val_CI$TDL$n_sam                 <- val_bs_TDL$n_sam
    val_CI$TDL$time                  <- val_bs_TDL$time
    val_CI$TDL$conc12CO2             <- f_val_bs_CI_endpoints(val_bs_TDL$conc12CO2            , val_bs_TDL$n, ind_CI);
    val_CI$TDL$conc13CO2             <- f_val_bs_CI_endpoints(val_bs_TDL$conc13CO2            , val_bs_TDL$n, ind_CI);
    val_CI$TDL$TGApressure           <- f_val_bs_CI_endpoints(val_bs_TDL$TGApressure          , val_bs_TDL$n, ind_CI);
    val_CI$TDL$MassFlow1             <- f_val_bs_CI_endpoints(val_bs_TDL$MassFlow1            , val_bs_TDL$n, ind_CI);
    val_CI$TDL$Pressure1             <- f_val_bs_CI_endpoints(val_bs_TDL$Pressure1            , val_bs_TDL$n, ind_CI);
    val_CI$TDL$MassFlow2             <- f_val_bs_CI_endpoints(val_bs_TDL$MassFlow2            , val_bs_TDL$n, ind_CI);
    val_CI$TDL$Pressure2             <- f_val_bs_CI_endpoints(val_bs_TDL$Pressure2            , val_bs_TDL$n, ind_CI);
    val_CI$TDL$PressureProMan        <- f_val_bs_CI_endpoints(val_bs_TDL$PressureProMan       , val_bs_TDL$n, ind_CI);
    val_CI$TDL$interp_tank_hi_12     <- f_val_bs_CI_endpoints(val_bs_TDL$interp_tank_hi_12    , val_bs_TDL$n, ind_CI);
    val_CI$TDL$interp_tank_hi_13     <- f_val_bs_CI_endpoints(val_bs_TDL$interp_tank_hi_13    , val_bs_TDL$n, ind_CI);
    val_CI$TDL$interp_tank_low_12    <- f_val_bs_CI_endpoints(val_bs_TDL$interp_tank_low_12   , val_bs_TDL$n, ind_CI);
    val_CI$TDL$interp_tank_low_13    <- f_val_bs_CI_endpoints(val_bs_TDL$interp_tank_low_13   , val_bs_TDL$n, ind_CI);
    val_CI$TDL$interp_reference_12   <- f_val_bs_CI_endpoints(val_bs_TDL$interp_reference_12  , val_bs_TDL$n, ind_CI);
    val_CI$TDL$interp_reference_13   <- f_val_bs_CI_endpoints(val_bs_TDL$interp_reference_13  , val_bs_TDL$n, ind_CI);
    val_CI$TDL$chamber_12            <- f_val_bs_CI_endpoints(val_bs_TDL$chamber_12           , val_bs_TDL$n, ind_CI);
    val_CI$TDL$chamber_13            <- f_val_bs_CI_endpoints(val_bs_TDL$chamber_13           , val_bs_TDL$n, ind_CI);
  }

  # Licor
  if (sw$use_Licor) {
    val_CI$Licor$n                   <- val_bs_Licor$n
    val_CI$Licor$first_ind           <- val_bs_Licor$first_ind
    val_CI$Licor$ind                 <- val_bs_Licor$ind
    val_CI$Licor$site                <- val_bs_Licor$site
    val_CI$Licor$time                <- val_bs_Licor$time
    val_CI$Licor$FTime               <- f_val_bs_CI_endpoints(val_bs_Licor$FTime        , val_bs_Licor$n, ind_CI);
    val_CI$Licor$A                   <- f_val_bs_CI_endpoints(val_bs_Licor$A            , val_bs_Licor$n, ind_CI);
    val_CI$Licor$gsc                 <- f_val_bs_CI_endpoints(val_bs_Licor$gsc          , val_bs_Licor$n, ind_CI);
    val_CI$Licor$Ci                  <- f_val_bs_CI_endpoints(val_bs_Licor$Ci           , val_bs_Licor$n, ind_CI);
    val_CI$Licor$E                   <- f_val_bs_CI_endpoints(val_bs_Licor$E            , val_bs_Licor$n, ind_CI);
    val_CI$Licor$VPD                 <- f_val_bs_CI_endpoints(val_bs_Licor$VPD          , val_bs_Licor$n, ind_CI);
    val_CI$Licor$La                  <- f_val_bs_CI_endpoints(val_bs_Licor$La           , val_bs_Licor$n, ind_CI);
    val_CI$Licor$StmRat              <- f_val_bs_CI_endpoints(val_bs_Licor$StmRat       , val_bs_Licor$n, ind_CI);
    val_CI$Licor$gbw                 <- f_val_bs_CI_endpoints(val_bs_Licor$gbw          , val_bs_Licor$n, ind_CI);
    val_CI$Licor$temp_air            <- f_val_bs_CI_endpoints(val_bs_Licor$temp_air     , val_bs_Licor$n, ind_CI);
    val_CI$Licor$temp_leaf           <- f_val_bs_CI_endpoints(val_bs_Licor$temp_leaf    , val_bs_Licor$n, ind_CI);
    val_CI$Licor$temp_block          <- f_val_bs_CI_endpoints(val_bs_Licor$temp_block   , val_bs_Licor$n, ind_CI);
    val_CI$Licor$Ce                  <- f_val_bs_CI_endpoints(val_bs_Licor$Ce           , val_bs_Licor$n, ind_CI);
    val_CI$Licor$Co                  <- f_val_bs_CI_endpoints(val_bs_Licor$Co           , val_bs_Licor$n, ind_CI);
    val_CI$Licor$xin                 <- f_val_bs_CI_endpoints(val_bs_Licor$xin          , val_bs_Licor$n, ind_CI);
    val_CI$Licor$xout                <- f_val_bs_CI_endpoints(val_bs_Licor$xout         , val_bs_Licor$n, ind_CI);
    val_CI$Licor$rh_ref              <- f_val_bs_CI_endpoints(val_bs_Licor$rh_ref       , val_bs_Licor$n, ind_CI);
    val_CI$Licor$rh_sam              <- f_val_bs_CI_endpoints(val_bs_Licor$rh_sam       , val_bs_Licor$n, ind_CI);
    val_CI$Licor$uin                 <- f_val_bs_CI_endpoints(val_bs_Licor$uin          , val_bs_Licor$n, ind_CI);
    val_CI$Licor$par_int             <- f_val_bs_CI_endpoints(val_bs_Licor$par_int      , val_bs_Licor$n, ind_CI);
    val_CI$Licor$par_ext             <- f_val_bs_CI_endpoints(val_bs_Licor$par_ext      , val_bs_Licor$n, ind_CI);
    val_CI$Licor$Atm_press           <- f_val_bs_CI_endpoints(val_bs_Licor$Atm_press    , val_bs_Licor$n, ind_CI);
    val_CI$Licor$CsMch               <- f_val_bs_CI_endpoints(val_bs_Licor$CsMch        , val_bs_Licor$n, ind_CI);
    val_CI$Licor$HsMch               <- f_val_bs_CI_endpoints(val_bs_Licor$HsMch        , val_bs_Licor$n, ind_CI);
    val_CI$Licor$StableF             <- f_val_bs_CI_endpoints(val_bs_Licor$StableF      , val_bs_Licor$n, ind_CI);
    val_CI$Licor$Status              <- f_val_bs_CI_endpoints(val_bs_Licor$Status       , val_bs_Licor$n, ind_CI);
    val_CI$Licor$VpdA                <- f_val_bs_CI_endpoints(val_bs_Licor$VpdA         , val_bs_Licor$n, ind_CI);
    val_CI$Licor$Ci_Ca               <- f_val_bs_CI_endpoints(val_bs_Licor$Ci_Ca        , val_bs_Licor$n, ind_CI);
    val_CI$Licor$pi                  <- f_val_bs_CI_endpoints(val_bs_Licor$pi           , val_bs_Licor$n, ind_CI);
    val_CI$Licor$uc_20_mV            <- f_val_bs_CI_endpoints(val_bs_Licor$uc_20_mV     , val_bs_Licor$n, ind_CI);
    val_CI$Licor$uc_21_mV            <- f_val_bs_CI_endpoints(val_bs_Licor$uc_21_mV     , val_bs_Licor$n, ind_CI);
    val_CI$Licor$U_S                 <- f_val_bs_CI_endpoints(val_bs_Licor$U_S          , val_bs_Licor$n, ind_CI);
    val_CI$Licor$Trans               <- f_val_bs_CI_endpoints(val_bs_Licor$Trans        , val_bs_Licor$n, ind_CI);
    val_CI$Licor$CndCO2              <- f_val_bs_CI_endpoints(val_bs_Licor$CndCO2       , val_bs_Licor$n, ind_CI);
    val_CI$Licor$Ref_mV              <- f_val_bs_CI_endpoints(val_bs_Licor$Ref_mV       , val_bs_Licor$n, ind_CI);
    val_CI$Licor$xTemp1              <- f_val_bs_CI_endpoints(val_bs_Licor$xTemp1       , val_bs_Licor$n, ind_CI);
    val_CI$Licor$xTemp2              <- f_val_bs_CI_endpoints(val_bs_Licor$xTemp2       , val_bs_Licor$n, ind_CI);
  }

  # Calc values
  val_CI$calc$gain_12C                                                <- f_val_bs_CI_endpoints(val_calc_bs$gain_12C                                              , val_bs_TDL$n, ind_CI);
  val_CI$calc$gain_13C                                                <- f_val_bs_CI_endpoints(val_calc_bs$gain_13C                                              , val_bs_TDL$n, ind_CI);
  val_CI$calc$offset_12C                                              <- f_val_bs_CI_endpoints(val_calc_bs$offset_12C                                            , val_bs_TDL$n, ind_CI);
  val_CI$calc$offset_13C                                              <- f_val_bs_CI_endpoints(val_calc_bs$offset_13C                                            , val_bs_TDL$n, ind_CI);
  val_CI$calc$reference_12Ce                                          <- f_val_bs_CI_endpoints(val_calc_bs$reference_12Ce                                        , val_bs_TDL$n, ind_CI);
  val_CI$calc$reference_13Ce                                          <- f_val_bs_CI_endpoints(val_calc_bs$reference_13Ce                                        , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12Co                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12Co                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13Co                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13Co                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$reference_TotalCe                                       <- f_val_bs_CI_endpoints(val_calc_bs$reference_TotalCe                                     , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_TotalCo                                         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_TotalCo                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_reference_Total_diff_CeCo                       <- f_val_bs_CI_endpoints(val_calc_bs$chamber_reference_Total_diff_CeCo                     , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_reference_12_diff_CeCo                          <- f_val_bs_CI_endpoints(val_calc_bs$chamber_reference_12_diff_CeCo                        , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_reference_13_diff_CeCo                          <- f_val_bs_CI_endpoints(val_calc_bs$chamber_reference_13_diff_CeCo                        , val_bs_TDL$n, ind_CI);
  val_CI$calc$xi                                                      <- f_val_bs_CI_endpoints(val_calc_bs$xi                                                    , val_bs_TDL$n, ind_CI);
  #val_CI$calc$flow_adjusted                                           <- f_val_bs_CI_endpoints(val_calc_bs$flow_adjusted                                         , val_bs_TDL$n, ind_CI); # 9/5/2012
  val_CI$calc$TDL_A_photosynthesis                                    <- f_val_bs_CI_endpoints(val_calc_bs$TDL_A_photosynthesis                                  , val_bs_TDL$n, ind_CI);
  val_CI$calc$TDL_12A_photosynthesis                                  <- f_val_bs_CI_endpoints(val_calc_bs$TDL_12A_photosynthesis                                , val_bs_TDL$n, ind_CI);
  val_CI$calc$TDL_13A_photosynthesis                                  <- f_val_bs_CI_endpoints(val_calc_bs$TDL_13A_photosynthesis                                , val_bs_TDL$n, ind_CI);
  val_CI$calc$Licor_A_photosynthesis                                  <- f_val_bs_CI_endpoints(val_calc_bs$Licor_A_photosynthesis                                , val_bs_TDL$n, ind_CI);
  val_CI$calc$Delta_from_ratios_in_out                                <- f_val_bs_CI_endpoints(val_calc_bs$Delta_from_ratios_in_out                              , val_bs_TDL$n, ind_CI);
  val_CI$calc$Delta_from_A_ratio                                      <- f_val_bs_CI_endpoints(val_calc_bs$Delta_from_A_ratio                                    , val_bs_TDL$n, ind_CI);
  val_CI$calc$VPD                                                     <- f_val_bs_CI_endpoints(val_calc_bs$VPD                                                   , val_bs_TDL$n, ind_CI);
  val_CI$calc$E_transpiration                                         <- f_val_bs_CI_endpoints(val_calc_bs$E_transpiration                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$leaf_temp                                               <- f_val_bs_CI_endpoints(val_calc_bs$leaf_temp                                             , val_bs_TDL$n, ind_CI);
  val_CI$calc$air_temp                                                <- f_val_bs_CI_endpoints(val_calc_bs$air_temp                                              , val_bs_TDL$n, ind_CI);
  val_CI$calc$light_in                                                <- f_val_bs_CI_endpoints(val_calc_bs$light_in                                              , val_bs_TDL$n, ind_CI);
  val_CI$calc$light_out                                               <- f_val_bs_CI_endpoints(val_calc_bs$light_out                                             , val_bs_TDL$n, ind_CI);
  val_CI$calc$reference_delta_e                                       <- f_val_bs_CI_endpoints(val_calc_bs$reference_delta_e                                     , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_delta_o                                         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_delta_o                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_reference_delta_diff_CoCe                       <- f_val_bs_CI_endpoints(val_calc_bs$chamber_reference_delta_diff_CoCe                     , val_bs_TDL$n, ind_CI);
  val_CI$calc$Delta_obs                                               <- f_val_bs_CI_endpoints(val_calc_bs$Delta_obs                                             , val_bs_TDL$n, ind_CI);
  val_CI$calc$Delta_obs_permil                                        <- f_val_bs_CI_endpoints(val_calc_bs$Delta_obs_permil                                      , val_bs_TDL$n, ind_CI);
  val_CI$calc$delta_13C_Assim                                         <- f_val_bs_CI_endpoints(val_calc_bs$delta_13C_Assim                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$p                                                       <- f_val_bs_CI_endpoints(val_calc_bs$p                                                     , val_bs_TDL$n, ind_CI);
  val_CI$calc$delta_13C_Resp                                          <- f_val_bs_CI_endpoints(val_calc_bs$delta_13C_Resp                                        , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_TotalCa                                         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_TotalCa                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12Ca                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12Ca                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13Ca                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13Ca                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_TotalCs                                         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_TotalCs                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12Cs                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12Cs                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13Cs                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13Cs                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalpa                                         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalpa                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12pa                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12pa                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13pa                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13pa                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalps                                         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalps                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12ps                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12ps                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13ps                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13ps                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalgbw                                        <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalgbw                                      , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalgbc                                        <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalgbc                                      , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12gbc                                           <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12gbc                                         , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13gbc                                           <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13gbc                                         , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalgsw                                        <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalgsw                                      , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalgsc                                        <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalgsc                                      , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12gsc                                           <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12gsc                                         , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13gsc                                           <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13gsc                                         , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalgtc                                        <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalgtc                                      , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12gtc                                           <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12gtc                                         , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13gtc                                           <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13gtc                                         , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_TotalCi                                         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_TotalCi                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12Ci                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12Ci                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13Ci                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13Ci                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalpi                                         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalpi                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12pi                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12pi                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13pi                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13pi                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalpi_pa                                      <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalpi_pa                                    , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Delta_i_simple_for_gm                           <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Delta_i_simple_for_gm                         , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Delta_i_simple_for_modeling                     <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Delta_i_simple_for_modeling                   , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Delta_i_complex_for_gm                          <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Delta_i_complex_for_gm                        , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Delta_i_simple_for_gm_Delta_obs                 <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Delta_i_simple_for_gm_Delta_obs               , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Delta_i_complex_for_gm_Delta_obs                <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Delta_i_complex_for_gm_Delta_obs              , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalgm_point_simple                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalgm_point_simple                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12gm_point_simple                               <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12gm_point_simple                             , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13gm_point_simple                               <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13gm_point_simple                             , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalgm_point_complex                           <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalgm_point_complex                         , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalgm_to_use                                  <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalgm_to_use                                , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalpc_using_gm                                <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalpc_using_gm                              , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12pc_using_gm                                   <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12pc_using_gm                                 , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13pc_using_gm                                   <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13pc_using_gm                                 , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalpc_using_simple_Delta_for_gm               <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalpc_using_simple_Delta_for_gm             , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalpc_using_simple_Delta_for_modeling         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalpc_using_simple_Delta_for_modeling       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalpc_using_complex_Delta_no_decarboxylation  <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalpc_using_complex_Delta_no_decarboxylation, val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalpc_using_complex_Delta_full_model          <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalpc_using_complex_Delta_full_model        , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_Totalpc_to_use                                  <- f_val_bs_CI_endpoints(val_calc_bs$chamber_Totalpc_to_use                                , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_TotalCc                                         <- f_val_bs_CI_endpoints(val_calc_bs$chamber_TotalCc                                       , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_12Cc                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_12Cc                                          , val_bs_TDL$n, ind_CI);
  val_CI$calc$chamber_13Cc                                            <- f_val_bs_CI_endpoints(val_calc_bs$chamber_13Cc                                          , val_bs_TDL$n, ind_CI);

  return( val_CI );
  ### val_CI
}

