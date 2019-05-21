#' Summary values for TDL interpolated tanks and reference
#'
#' Calculate mean, var, and sd for each site for interpolated ranges.
#'
#' Data updated to exclude indices that we don't summarize.
#'
#' @param TDL
#' @param TDL_cycle
#'
#' @return TDL
#'
#' @examples
calc_mean_TDL_interp_tanks_ref <-
function# Summary values for TDL interpolated tanks and reference
###
(TDL
###
, TDL_cycle
###
)
{
  TDL_var_names <- colnames(TDL$interp);

  TDL_interp_summary_mean <- matrix(0, nrow=TDL$summary$n, ncol=length(TDL_var_names));
  TDL_interp_summary_var  <- matrix(0, nrow=TDL$summary$n, ncol=length(TDL_var_names));
  TDL_interp_summary_sd   <- matrix(0, nrow=TDL$summary$n, ncol=length(TDL_var_names));

  ##details<<
  ## Calculate mean, var, and sd for each site for interpolated ranges.
  ind_temp <- NULL;
  for (i_list in 1:TDL$summary$n) {
    rows_temp <- TDL$summary$first_ind[i_list]:TDL$summary$ind[i_list];
    if (length(rows_temp) > 1) {
      TDL_interp_summary_mean[i_list,] <- apply( TDL$interp[rows_temp,]  , MARGIN=2, mean, na.rm = TRUE); # mean, ignoring NA's
      TDL_interp_summary_var [i_list,] <- apply( TDL$interp[rows_temp,]  , MARGIN=2, var , na.rm = TRUE); # var , ignoring NA's
      TDL_interp_summary_sd  [i_list,] <- apply( TDL$interp[rows_temp,]  , MARGIN=2, sd  , na.rm = TRUE); # sd  , ignoring NA's
    } else { # if only 1 observation, don't use apply "0.1-16" "2012-07-10"
      TDL_interp_summary_mean[i_list,] <- as.matrix(TDL$interp[rows_temp,]); # mean
      # var and sd are initialized at 0
    }
    ind_temp <- c(ind_temp, TDL$summary$first_ind[i_list]:TDL$summary$ind[i_list]);  # indices to keep in data
  }

  ind_temp_NA <- x_wo_y(seq(1,TDL$n), ind_temp); # excluded indices

  ##details<<
  ## Data updated to exclude indices that we don't summarize.
  TDL$interp[ind_temp_NA,] <- NA;

  colnames(TDL_interp_summary_mean ) <- paste("interp_",TDL_var_names,sep="");
  colnames(TDL_interp_summary_var  ) <- paste("interp_",TDL_var_names,sep="");
  colnames(TDL_interp_summary_sd   ) <- paste("interp_",TDL_var_names,sep="");

  TDL$summary$mean <- cbind(TDL$summary$mean , TDL_interp_summary_mean);
  TDL$summary$var  <- cbind(TDL$summary$var  , TDL_interp_summary_var );
  TDL$summary$sd   <- cbind(TDL$summary$sd   , TDL_interp_summary_sd  );

  return( TDL );
  ### TDL
}

