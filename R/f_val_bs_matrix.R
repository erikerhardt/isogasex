#' update values for each bs iterate
#'
#' Move current value of \code{val_calc_bs_temp} into \code{bs$*} after checked with \code{\link{f_val_bs_matrix_check}}.
#'
#' @param bs
#' @param val_calc_bs_temp
#' @param i_bs
#'
#' @return bs
#' @export
#'
#' @examples
f_val_bs_matrix <-
function# update values for each bs iterate
###
(bs
###
, val_calc_bs_temp
###
, i_bs
###
)
{

  ##details<<
  ## Move current value of \code{val_calc_bs_temp} into \code{bs$*} after checked with \code{\link{f_val_bs_matrix_check}}.

  bs$gain_12C                                              [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$gain_12C                                              );
  bs$gain_13C                                              [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$gain_13C                                              );
  bs$offset_12C                                            [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$offset_12C                                            );
  bs$offset_13C                                            [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$offset_13C                                            );
  bs$reference_12Ce                                        [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$reference_12Ce                                        );
  bs$reference_13Ce                                        [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$reference_13Ce                                        );
  bs$chamber_12Co                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12Co                                          );
  bs$chamber_13Co                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13Co                                          );
  bs$reference_TotalCe                                     [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$reference_TotalCe                                     );
  bs$chamber_TotalCo                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_TotalCo                                       );
  bs$chamber_reference_Total_diff_CeCo                     [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_reference_Total_diff_CeCo                     );
  bs$chamber_reference_12_diff_CeCo                        [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_reference_12_diff_CeCo                        );
  bs$chamber_reference_13_diff_CeCo                        [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_reference_13_diff_CeCo                        );
  bs$xi                                                    [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$xi                                                    );
  #bs$flow_adjusted                                         [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$flow_adjusted                                         ); # 9/5/2012
  bs$TDL_A_photosynthesis                                  [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$TDL_A_photosynthesis                                  );
  bs$TDL_12A_photosynthesis                                [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$TDL_12A_photosynthesis                                );
  bs$TDL_13A_photosynthesis                                [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$TDL_13A_photosynthesis                                );
  bs$Licor_A_photosynthesis                                [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$Licor_A_photosynthesis                                );
  bs$Delta_from_ratios_in_out                              [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$Delta_from_ratios_in_out                              );
  bs$Delta_from_A_ratio                                    [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$Delta_from_A_ratio                                    );
  bs$VPD                                                   [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$VPD                                                   );
  bs$E_transpiration                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$E_transpiration                                       );
  bs$leaf_temp                                             [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$leaf_temp                                             );
  bs$air_temp                                              [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$air_temp                                              );
  bs$light_in                                              [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$light_in                                              );
  bs$light_out                                             [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$light_out                                             );
  bs$reference_delta_e                                     [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$reference_delta_e                                     );
  bs$chamber_delta_o                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_delta_o                                       );
  bs$chamber_reference_delta_diff_CoCe                     [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_reference_delta_diff_CoCe                     );
  bs$Delta_obs                                             [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$Delta_obs                                             );
  bs$Delta_obs_permil                                      [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$Delta_obs_permil                                      );
  bs$delta_13C_Assim                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$delta_13C_Assim                                       );
  bs$p                                                     [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$p                                                     );
  bs$delta_13C_Resp                                        [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$delta_13C_Resp                                        );
  bs$chamber_TotalCa                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_TotalCa                                       );
  bs$chamber_12Ca                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12Ca                                          );
  bs$chamber_13Ca                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13Ca                                          );
  bs$chamber_TotalCs                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_TotalCs                                       );
  bs$chamber_12Cs                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12Cs                                          );
  bs$chamber_13Cs                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13Cs                                          );
  bs$chamber_Totalpa                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalpa                                       );
  bs$chamber_12pa                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12pa                                          );
  bs$chamber_13pa                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13pa                                          );
  bs$chamber_Totalps                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalps                                       );
  bs$chamber_12ps                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12ps                                          );
  bs$chamber_13ps                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13ps                                          );
  bs$chamber_Totalgbw                                      [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalgbw                                      );
  bs$chamber_Totalgbc                                      [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalgbc                                      );
  bs$chamber_12gbc                                         [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12gbc                                         );
  bs$chamber_13gbc                                         [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13gbc                                         );
  bs$chamber_Totalgsw                                      [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalgsw                                      );
  bs$chamber_Totalgsc                                      [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalgsc                                      );
  bs$chamber_12gsc                                         [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12gsc                                         );
  bs$chamber_13gsc                                         [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13gsc                                         );
  bs$chamber_Totalgtc                                      [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalgtc                                      );
  bs$chamber_12gtc                                         [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12gtc                                         );
  bs$chamber_13gtc                                         [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13gtc                                         );
  bs$chamber_TotalCi                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_TotalCi                                       );
  bs$chamber_12Ci                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12Ci                                          );
  bs$chamber_13Ci                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13Ci                                          );
  bs$chamber_Totalpi                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalpi                                       );
  bs$chamber_12pi                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12pi                                          );
  bs$chamber_13pi                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13pi                                          );
  bs$chamber_Totalpi_pa                                    [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalpi_pa                                    );
  bs$chamber_Delta_i_simple_for_gm                         [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Delta_i_simple_for_gm                         );
  bs$chamber_Delta_i_simple_for_modeling                   [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Delta_i_simple_for_modeling                   );
  bs$chamber_Delta_i_complex_for_gm                        [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Delta_i_complex_for_gm                        );
  bs$chamber_Delta_i_simple_for_gm_Delta_obs               [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Delta_i_simple_for_gm_Delta_obs               );
  bs$chamber_Delta_i_complex_for_gm_Delta_obs              [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Delta_i_complex_for_gm_Delta_obs              );
  bs$chamber_Totalgm_point_simple                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalgm_point_simple                          );
  bs$chamber_12gm_point_simple                             [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12gm_point_simple                             );
  bs$chamber_13gm_point_simple                             [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13gm_point_simple                             );
  bs$chamber_Totalgm_point_complex                         [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalgm_point_complex                         );
  bs$chamber_Totalgm_to_use                                [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalgm_to_use                                );
  bs$chamber_Totalpc_using_gm                              [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalpc_using_gm                              );
  bs$chamber_12pc_using_gm                                 [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12pc_using_gm                                 );
  bs$chamber_13pc_using_gm                                 [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13pc_using_gm                                 );
  bs$chamber_Totalpc_using_simple_Delta_for_gm             [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalpc_using_simple_Delta_for_gm             );
  bs$chamber_Totalpc_using_simple_Delta_for_modeling       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalpc_using_simple_Delta_for_modeling       );
  bs$chamber_Totalpc_using_complex_Delta_no_decarboxylation[,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalpc_using_complex_Delta_no_decarboxylation);
  bs$chamber_Totalpc_using_complex_Delta_full_model        [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalpc_using_complex_Delta_full_model        );
  bs$chamber_Totalpc_to_use                                [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_Totalpc_to_use                                );
  bs$chamber_TotalCc                                       [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_TotalCc                                       );
  bs$chamber_12Cc                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_12Cc                                          );
  bs$chamber_13Cc                                          [,i_bs]  <- f_val_bs_matrix_check(val_calc_bs_temp$chamber_13Cc                                          );

  return( bs );
  ### bs
}

