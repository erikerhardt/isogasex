#' Calculate the mean and variance for the TDL/Licor data based on last measurements
#'
#' create TDL$summary for numerical summaries
#'
#' Calculate mean, var, and sd for each site for retained observations
#'
#' @param TDL xxxPARAMxxx
#' @param TDL_cycle xxxPARAMxxx
#'
#' @return TDL xxxRETURNxxx
#'
calc_mean_TDL <-
function# Calculate the mean and variance for the TDL/Licor data based on last measurements
###
(TDL
###
, TDL_cycle
###
)
{

  TDL_var_names <- c(
      "ConcA"
     ,"ConcB"
     ,"TGApressure"
     ,"MassFlow1"
     ,"Pressure1"
     ,"MassFlow2"
     ,"Pressure2"
     ,"PressureProMan");

  #Licor_var_names <- c("FTime", "Photo", "Cond", "Ci", "Trmmol", "VpdL", "Area"
  #  , "StmRat", "BLCond", "Tair", "Tleaf", "TBlk", "CO2R", "CO2S", "H2OR"
  #  , "H2OS", "RH_R", "RH_S", "Flow", "PARi", "PARo", "Press", "CsMch"
  #  , "HsMch", "StableF", "Status"
  #  # additional columns
  #  , "VpdA", "Ci_Ca", "Ci_Pa", "uc_20_mV", "uc_21_mV", "X.U.S.", "Trans", "CndCO2", "Ref_mV", "xTemp1", "xTemp2");

  ##details<<
  ## create TDL$summary for numerical summaries
  TDL$summary           <- as.list(new.env());
  TDL$summary$n         <- dim(TDL$last_list)[1];
  TDL$summary$first_ind <- TDL$last_list[,"first_ind"];
  TDL$summary$ind       <- TDL$last_list[,"ind"];
  TDL$summary$site      <- TDL$last_list[,"site"];
  TDL$summary$n_sam     <- rep(0, TDL$summary$n);
  TDL$summary$time      <- TDL$time[TDL$summary$ind];
  TDL$summary$mean      <- matrix(0,nrow=TDL$summary$n,ncol=length(TDL_var_names));
  TDL$summary$var       <- matrix(0,nrow=TDL$summary$n,ncol=length(TDL_var_names));
  TDL$summary$sd        <- matrix(0,nrow=TDL$summary$n,ncol=length(TDL_var_names));

  ## create Licor$summary for numerical summaries
  #Licor$summary           <- as.list(new.env());
  #Licor$summary$n         <- TDL$summary$n;
  #Licor$summary$ind       <- TDL$summary$ind;
  #Licor$summary$first_ind <- TDL$summary$first_ind;
  #Licor$summary$site      <- TDL$summary$site;
  #Licor$summary$time      <- TDL$summary$time;
  #Licor$summary$mean      <- matrix(0,nrow=Licor$summary$n,ncol=length(Licor_var_names));

  colnames(TDL$summary$mean ) <- TDL_var_names;
  colnames(TDL$summary$var  ) <- TDL_var_names;
  colnames(TDL$summary$sd   ) <- TDL_var_names;
  #colnames(Licor$summary$mean ) <- Licor_var_names;

  ##details<<
  ## Calculate mean, var, and sd for each site for retained observations
  ind_temp <- NULL;
  for (i_list in 1:TDL$summary$n) {
    # summarized values
    i_n_obs <- TDL_cycle$table[(TDL$summary$site[i_list] == TDL_cycle$table[,"site"]),"last_n_obs"]; # number of obs
    i_ind_last <- TDL$summary$ind[i_list];                                                           # last index
    i_ind_first <- TDL$summary$first_ind[i_list];                                                    # first index
    TDL$summary$n_sam [i_list]  <- i_n_obs;                                                          # number of obs
    rows_temp <- i_ind_first:i_ind_last;
    if (length(rows_temp) > 1) {
      TDL$summary$mean  [i_list,] <- apply( TDL$data  [rows_temp, TDL_var_names]  , MARGIN=2, mean, na.rm = TRUE); # mean, ignoring NA's
      TDL$summary$var   [i_list,] <- apply( TDL$data  [rows_temp, TDL_var_names]  , MARGIN=2, var , na.rm = TRUE); # var , ignoring NA's
      TDL$summary$sd    [i_list,] <- apply( TDL$data  [rows_temp, TDL_var_names]  , MARGIN=2, sd  , na.rm = TRUE); # sd  , ignoring NA's
    } else { # if only 1 observation, don't use apply "0.1-16" "2012-07-10"
      TDL$summary$mean  [i_list,] <- as.matrix(TDL$data  [rows_temp, TDL_var_names]); # mean
      # var and sd are initialized at 0
    }

    ind_temp <- c(ind_temp, rows_temp);  # indices to keep in data
  }

  ind_temp_NA <- x_wo_y(seq(1,TDL$n), ind_temp); # excluded indices

  # data updated to exclude indices that we don't summarize
  TDL$ind   [ind_temp_NA]  <- NA;
  TDL$time  [ind_temp_NA]  <- NA;
  TDL$data  [ind_temp_NA,] <- NA;
  #Licor$data[ind_temp_NA,] <- NA;

  #TDL_summary <- as.list(new.env());  # create a list to return with data
  #TDL_summary$TDL   <- TDL;
  #TDL_Licor_summary$Licor <- Licor;
  return( TDL );
  ### TDL
}

