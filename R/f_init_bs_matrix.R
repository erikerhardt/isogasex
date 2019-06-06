#' init bs values to zero
#'
#' Create bs for calculated bootstrap values to estimate error, initializing to zeros.
#'
#' @param n xxxPARAMxxx
#' @param R_bootstrap xxxPARAMxxx
#'
#' @return bs xxxRETURNxxx
#'
f_init_bs_matrix <-
function# init bs values to zero
###
(n
###
, R_bootstrap
###
)
{
  ##details<<
  ## Create bs for calculated bootstrap values to estimate error, initializing to zeros.
  bs  <- as.list(new.env()); # for calculated bootstrap values to estimate error
  zeros <- matrix(0, nrow=n, ncol=R_bootstrap);
  bs$gain_12C                                                <- zeros;
  bs$gain_13C                                                <- zeros;
  bs$offset_12C                                              <- zeros;
  bs$offset_13C                                              <- zeros;
  bs$reference_12Ce                                          <- zeros;
  bs$reference_13Ce                                          <- zeros;
  bs$chamber_12Co                                            <- zeros;
  bs$chamber_13Co                                            <- zeros;
  bs$reference_TotalCe                                       <- zeros;
  bs$chamber_TotalCo                                         <- zeros;
  bs$chamber_reference_Total_diff_CeCo                       <- zeros;
  bs$chamber_reference_12_diff_CeCo                          <- zeros;
  bs$chamber_reference_13_diff_CeCo                          <- zeros;
  bs$xi                                                      <- zeros;
  #bs$flow_adjusted                                           <- zeros;  # 9/5/2012
  bs$TDL_A_photosynthesis                                    <- zeros;
  bs$TDL_12A_photosynthesis                                  <- zeros;
  bs$TDL_13A_photosynthesis                                  <- zeros;
  bs$Licor_A_photosynthesis                                  <- zeros;
  bs$Delta_from_ratios_in_out                                <- zeros;
  bs$Delta_from_A_ratio                                      <- zeros;
  bs$VPD                                                     <- zeros;
  bs$E_transpiration                                         <- zeros;
  bs$leaf_temp                                               <- zeros;
  bs$air_temp                                                <- zeros;
  bs$light_in                                                <- zeros;
  bs$light_out                                               <- zeros;
  bs$reference_delta_e                                       <- zeros;
  bs$chamber_delta_o                                         <- zeros;
  bs$chamber_reference_delta_diff_CoCe                       <- zeros;
  bs$Delta_obs                                               <- zeros;
  bs$Delta_obs_permil                                        <- zeros;
  bs$delta_13C_Assim                                         <- zeros;
  bs$p                                                       <- zeros;
  bs$delta_13C_Resp                                          <- zeros;
  bs$chamber_TotalCa                                         <- zeros;
  bs$chamber_12Ca                                            <- zeros;
  bs$chamber_13Ca                                            <- zeros;
  bs$chamber_TotalCs                                         <- zeros;
  bs$chamber_12Cs                                            <- zeros;
  bs$chamber_13Cs                                            <- zeros;
  bs$chamber_Totalpa                                         <- zeros;
  bs$chamber_12pa                                            <- zeros;
  bs$chamber_13pa                                            <- zeros;
  bs$chamber_Totalps                                         <- zeros;
  bs$chamber_12ps                                            <- zeros;
  bs$chamber_13ps                                            <- zeros;
  bs$chamber_Totalgbw                                        <- zeros;
  bs$chamber_Totalgbc                                        <- zeros;
  bs$chamber_12gbc                                           <- zeros;
  bs$chamber_13gbc                                           <- zeros;
  bs$chamber_Totalgsw                                        <- zeros;
  bs$chamber_Totalgsc                                        <- zeros;
  bs$chamber_12gsc                                           <- zeros;
  bs$chamber_13gsc                                           <- zeros;
  bs$chamber_Totalgtc                                        <- zeros;
  bs$chamber_12gtc                                           <- zeros;
  bs$chamber_13gtc                                           <- zeros;
  bs$chamber_TotalCi                                         <- zeros;
  bs$chamber_12Ci                                            <- zeros;
  bs$chamber_13Ci                                            <- zeros;
  bs$chamber_Totalpi                                         <- zeros;
  bs$chamber_12pi                                            <- zeros;
  bs$chamber_13pi                                            <- zeros;
  bs$chamber_Totalpi_pa                                      <- zeros;
  bs$chamber_Delta_i_simple_for_gm                           <- zeros;
  bs$chamber_Delta_i_simple_for_modeling                     <- zeros;
  bs$chamber_Delta_i_complex_for_gm                          <- zeros;
  bs$chamber_Delta_i_simple_for_gm_Delta_obs                 <- zeros;
  bs$chamber_Delta_i_complex_for_gm_Delta_obs                <- zeros;
  bs$chamber_Totalgm_point_simple                            <- zeros;
  bs$chamber_12gm_point_simple                               <- zeros;
  bs$chamber_13gm_point_simple                               <- zeros;
  bs$chamber_Totalgm_point_complex                           <- zeros;
  bs$chamber_Totalgm_to_use                                  <- zeros;
  bs$chamber_Totalpc_using_gm                                <- zeros;
  bs$chamber_12pc_using_gm                                   <- zeros;
  bs$chamber_13pc_using_gm                                   <- zeros;
  bs$chamber_Totalpc_using_simple_Delta_for_gm               <- zeros;
  bs$chamber_Totalpc_using_simple_Delta_for_modeling         <- zeros;
  bs$chamber_Totalpc_using_complex_Delta_no_decarboxylation  <- zeros;
  bs$chamber_Totalpc_using_complex_Delta_full_model          <- zeros;
  bs$chamber_Totalpc_to_use                                  <- zeros;
  bs$chamber_TotalCc                                         <- zeros;
  bs$chamber_12Cc                                            <- zeros;
  bs$chamber_13Cc                                            <- zeros;

  return( bs );
  ### bs
}

