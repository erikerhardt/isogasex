#' Calculate the mean for the Licor data based on last measurements
#'
#' create Licor$summary for numerical summaries
#'
#' Calculate mean for each site for retained observations
#'
#' @param TDL xxxPARAMxxx
#' @param Licor xxxPARAMxxx
#' @param TDL_cycle xxxPARAMxxx
#' @param sw xxxPARAMxxx
#'
#' @return Licor xxxRETURNxxx
#'
calc_mean_Licor <-
function# Calculate the mean for the Licor data based on last measurements
###
(TDL
###
, Licor
###
, TDL_cycle
###
, sw
###
)
{
  Licor_var_names <- colnames(Licor$data); # 7/15/2010 don't hardset column names
    #c("FTime", "Photo", "Cond", "Ci", "Trmmol", "VpdL", "Area"
    #, "StmRat", "BLCond", "Tair", "Tleaf", "TBlk", "CO2R", "CO2S", "H2OR"
    #, "H2OS", "RH_R", "RH_S", "Flow", "PARi", "PARo", "Press", "CsMch"
    #, "HsMch", "StableF", "Status"
    ## additional columns
    #, "VpdA", "Ci_Ca", "Ci_Pa", "uc_20_mV", "uc_21_mV", "X.U.S.", "Trans", "CndCO2", "Ref_mV", "xTemp1", "xTemp2");  # , "TChamAir"

  ##details<<
  ## create Licor$summary for numerical summaries
  Licor$summary           <- as.list(new.env());
  Licor$summary$n         <- TDL$summary$n;
  Licor$summary$ind       <- TDL$summary$ind;
  Licor$summary$first_ind <- TDL$summary$first_ind;
  Licor$summary$site      <- TDL$summary$site;
  Licor$summary$time      <- TDL$summary$time;
  Licor$summary$mean      <- matrix(0,nrow=Licor$summary$n,ncol=length(Licor_var_names));

  colnames(Licor$summary$mean ) <- Licor_var_names;

  ##details<<
  ## Calculate mean for each site for retained observations
  ind_temp <- NULL;
  for (i_list in 1:Licor$summary$n) {
    # summarized values
    i_n_obs <- TDL_cycle$table[(TDL$summary$site[i_list] == TDL_cycle$table[,"site"]),"last_n_obs"]; # number of obs
    i_ind_first <- TDL$summary$first_ind[i_list];                                                    # first index
    i_ind_last  <- TDL$summary$ind[i_list];                                                          # last index
    if (i_ind_last > Licor$n) {i_ind_last <- Licor$n;};
    rows_temp <- i_ind_first:i_ind_last;
    if (length(rows_temp) > 1) {
      Licor$summary$mean[i_list,] <- apply( Licor$data[rows_temp, Licor_var_names], MARGIN=2, mean, na.rm = TRUE); # mean, ignoring NA's
    } else { # if only 1 observation, don't use apply "0.1-16" "2012-07-10"
      Licor$summary$mean[i_list,] <- as.matrix(Licor$data[rows_temp, Licor_var_names]); # mean
    }

    #ind_temp <- c(ind_temp, i_ind_first:i_ind_last);  # indices to keep in data
  }

  #ind_temp_NA <- x_wo_y(seq(1,TDL$n), ind_temp); # excluded indices
  ind_temp_NA <- is.na(TDL$time); # excluded indices

  # data updated to exclude indices that we don't summarize
  Licor$data[ind_temp_NA,] <- NA;

  return( Licor );
  ### Licor
}

