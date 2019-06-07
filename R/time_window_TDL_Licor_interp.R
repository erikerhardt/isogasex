#' keep only the overlapping time window of TDL and Licor files
#'
#' interpolate the Licor measurements via cubic splines to TDL faster sampling rate
#' assign variables names within TDL and Licor_interp
#'
#' BEGIN If use TDL
#'
#' - Indices to keep based on TDL/Licor time window overlap.
#'
#' - TDL summary ind to keep.
#'
#' - Reduce TDL summary.
#'
#' - Reduce TDL interp.
#'
#' END
#'
#' BEGIN If use Licor
#'
#' - Indices to keep based on TDL/Licor time window overlap.
#'
#' - Use \code{zoo} to merge the times for TDL and Licor.
#'
#' Create \code{Licor_interp} to hold interpolated Licor values at resolution of TDL observations.
#'
#' populate with observed values in right locations
#'
#' These indices have actual Licor values, the others are interpolated.
#'
#' Some Licor values maybe shouldn't be interpolated --
#'
#' If all values are NA, then consider column as a missing column and don't interp.
#'
#' Cubic interpolation of Licor values to TDL sampling time points using \code{nknots}=\code{df}=
#' Cubic spline interpolation using \code{smooth_spline} with \code{nknots}=\code{df}=number of Licor times matching TDL times.
#'
#' END
#'
#' Put TDL and Licor_interp together to return.
#'
#' @param TDL xxxPARAMxxx
#' @param Licor xxxPARAMxxx
#' @param TDL_Licor_times xxxPARAMxxx
#' @param sw xxxPARAMxxx
#'
#' @return TDL_and_Licor_interp xxxRETURNxxx
#' @importFrom zoo zoo
#'
time_window_TDL_Licor_interp <-
function# keep only the overlapping time window of TDL and Licor files
### interpolate the Licor measurements via cubic splines to TDL faster sampling rate
### assign_variables names within TDL and Licor_interp
(TDL
###
, Licor
###
, TDL_Licor_times
###
, sw
###
)
{
  #try_zoo <- library(zoo, logical.return = TRUE); # date functions to merge TDL and Licor by time
  #
  ## install the package if not already in the library
  #if ( try_zoo == FALSE ) {
  #  install.packages("zoo");
  #  library(zoo);
  #}

  ##details<<
  ## BEGIN If use TDL
  if (sw$use_TDL) {
    ##details<<
    ## - Indices to keep based on TDL/Licor time window overlap.
    TDL_ind_keep        <- seq(TDL_Licor_times$TDL_time_ind_first,   TDL_Licor_times$TDL_time_ind_last);

    TDL$n               <- length(TDL_ind_keep);
    TDL$time            <- TDL$time[TDL_ind_keep];
    TDL$time_org        <- TDL$time_org[TDL_ind_keep];
    TDL$data            <- TDL$data[TDL_ind_keep,];

    ##details<<
    ## - TDL summary ind to keep.
    TDL_summary_ind        <- (TDL$last_list[,1] %in% TDL_ind_keep);
    TDL$last_list          <- TDL$last_list[TDL_summary_ind,];
    TDL$last_list[,c(1,2)] <- TDL$last_list[,c(1,2)]-TDL_Licor_times$TDL_time_ind_first+1; # reset indices

    ##details<<
    ## - Reduce TDL summary.
    TDL$summary$n         <- sum(TDL_summary_ind);
    TDL$summary$first_ind <- TDL$summary$first_ind[TDL_summary_ind] - TDL_Licor_times$TDL_time_ind_first + 1;
    TDL$summary$ind       <- TDL$summary$ind      [TDL_summary_ind] - TDL_Licor_times$TDL_time_ind_first + 1;
    TDL$summary$site      <- TDL$summary$site     [TDL_summary_ind];
    TDL$summary$n_sam     <- TDL$summary$n_sam    [TDL_summary_ind];
    TDL$summary$time      <- TDL$summary$time     [TDL_summary_ind];
    TDL$summary$mean      <- TDL$summary$mean     [TDL_summary_ind,];
    TDL$summary$var       <- TDL$summary$var      [TDL_summary_ind,];
    TDL$summary$sd        <- TDL$summary$sd       [TDL_summary_ind,];

    ##details<<
    ## - Reduce TDL interp.
    TDL$interp <- TDL$interp[TDL_ind_keep,];

    ## assign TDL variables  moved to read_TDL.R 8/18/2011 10:50AM
    #TDL$ind             <- 1:TDL$n;
  }
  ##details<<
  ## END

  ##details<<
  ## BEGIN If use Licor
  if (sw$use_Licor) {
    ##details<<
    ## - Indices to keep based on TDL/Licor time window overlap.
    Licor_ind_keep      <- seq(TDL_Licor_times$Licor_time_ind_first, TDL_Licor_times$Licor_time_ind_last);

    # note, if change this, also change same lines below in if() statement
    ## Licor
    Licor$n             <- length(Licor_ind_keep);
    Licor$time          <- Licor$time[Licor_ind_keep];
    Licor$data          <- Licor$data[Licor_ind_keep,];

    ### only consider non-NA values in TDL (NAs were substituted for pre-end tank values, transition periods)
    ##TDL_nonNA_ind <- !is.na(TDL$time);

    ##details<<
    ## - Use \code{zoo} to merge the times for TDL and Licor.

    if ((sw$use_TDL==0)) { # not using TDL, replace with Licor values used below
      TDL$n <- Licor$n;
      TDL$time_org <- Licor$time;
    }

    TDL_time_merge      <- zoo::zoo(1:TDL$n,   TDL$time_org);
    Licor_time_merge    <- zoo::zoo(1:Licor$n, Licor$time);
    TL_merge            <- merge(TDL_time_merge, Licor_time_merge);      # matching TDL with Licor times
    TDL_ind_match_Licor <- TL_merge[is.finite(TL_merge[,2]),1]; # indices of TDL matching Licor times
    #Licor_matched_with_non_NA_TDL <- (1:length(TDL_ind_match_Licor))[!is.na(TDL_ind_match_Licor)]; # 2/9/2012 0:22AM add - if NA in TDL file, skip those
    #ind_match <- TDL_ind_match_Licor[Licor_matched_with_non_NA_TDL];

    ## DEBUG 2/12/2012 9:45PM
    # TL_merge[is.na(TL_merge[,1]),]


    # 2/9/2012 2:01AM
    if (sum(is.na(TL_merge[,1]))) { # NA records in TDL file, see read_TDL.R for # Where TDL$StartSeqFlag!=0 or TDL$SeqActiveFlag==0
      p_o <- paste("WARNING: Times in Licor that do not appear in TDL", "\n"); write_out(p_o);
      p_o <- paste("           (maybe an interval when TDL wasn't collecting data?)", "\n"); write_out(p_o);
      p_o <- paste("         removing these time points from Licor data", "\n"); write_out(p_o);
      p_o <- paste("         list of times:", "\n"); write_out(p_o);
      Licor_ind_not_in_TDL <- as.numeric(TL_merge[is.na(TL_merge[,1]),2]);
      p_o <- paste("           ", Licor$time[Licor_ind_not_in_TDL], "\n"); write_out(p_o);


        # not ideal, but redo Licor data lines without these observations
        Licor_ind_keep      <- setdiff(1:Licor$n, Licor_ind_not_in_TDL)
        Licor$n             <- length(Licor_ind_keep);
        Licor$time          <- Licor$time[Licor_ind_keep];
        Licor$data          <- Licor$data[Licor_ind_keep,];
        Licor_time_merge    <- zoo::zoo(1:Licor$n, Licor$time);
        TL_merge            <- merge(TDL_time_merge, Licor_time_merge);      # matching TDL with Licor times
        TDL_ind_match_Licor <- TL_merge[is.finite(TL_merge[,2]),1]; # indices of TDL matching Licor times
        if (sum(is.na(TL_merge[,1]))) { # NA records in TDL file, see read_TDL.R for # Where TDL$StartSeqFlag!=0 or TDL$SeqActiveFlag==0
          p_o <- paste("THIS WARNING SHOULD NEVER APPEAR, time_window_TDL_Licor_interp.R times1", "\n"); write_out(p_o);
        }

      #(1:length(TL_merge[,1]))[is.na(TL_merge[,1])]
    }

    ##details<<
    ## Create \code{Licor_interp} to hold interpolated Licor values at resolution of TDL observations.

    Licor_interp <- as.list(new.env());  # create a list to return with data
    Licor_interp$data <- matrix(NA,nrow=TDL$n,ncol=dim(Licor$data)[2]);  # init matrix
    colnames(Licor_interp$data) <- colnames(Licor$data);                # assign col names
    for (i_col in 1:dim(Licor$data)[2]) {
      ##details<<
      ## populate with observed values in right locations
      Licor_interp$data[TDL_ind_match_Licor,i_col] <- Licor$data[,i_col];
      #Licor_interp$data[ind_match,i_col] <- Licor$data[Licor_matched_with_non_NA_TDL,i_col]; # populate with observed values in right locations # 2/9/2012 0:22AM change - if NA in TDL file, skip those
    }

    Licor_interp$ind    <- 1:TDL$n;
    Licor_interp$time   <- TDL$time_org;  # time is same as in TDL


    Licor_interp$n                <- TDL$n;
    Licor_interp$noninterp_ind    <- rep(0,TDL$n);
    ##details<<
    ## These indices have actual Licor values, the others are interpolated.
    Licor_interp$noninterp_ind[TDL_ind_match_Licor] <- 1;

    ##details<<
    ## Some Licor values maybe shouldn't be interpolated --
    for (i_col in 1:dim(Licor$data)[2]) {
      ##details<<
      ## If all values are NA, then consider column as a missing column and don't interp.
      if (sum(is.na(as.numeric(Licor$data[,i_col]))) != Licor$n) {
        ##details<<
        ## Cubic interpolation of Licor values to TDL sampling time points using \code{nknots}=\code{df}=
        ## Cubic spline interpolation using \code{smooth.spline} with \code{nknots}=\code{df}=number of Licor times matching TDL times.
        ss <- smooth.spline(Licor_interp$ind[TDL_ind_match_Licor], Licor_interp$data[TDL_ind_match_Licor,i_col], nknots=length(TDL_ind_match_Licor), df=length(TDL_ind_match_Licor));
        #ss <- smooth.spline(Licor_interp$ind[ind_match], Licor_interp$data[ind_match,i_col], nknots=length(ind_match), df=length(ind_match));  # 2/9/2012 1:16AM
        Licor_interp$data[,i_col] <- predict(ss,Licor_interp$ind)$y;
      }
      #plot(Licor_interp$ind[TDL_ind_match_Licor], Licor_interp$data[TDL_ind_match_Licor,i_col],pch=20)
      #points(ss,col="red")
      #points(Licor_interp$ind,Licor_interp$data[,i_col],col="green",type="l")
      #points(Licor_interp$ind,Licor_interp$data[,i_col],col="blue",pch=".")
    }
  } else {
    # not using Licor data
    Licor_interp <- Licor;
  }
  ##details<<
  ## END

  ##details<<
  ## Put TDL and Licor_interp together to return.
  TDL_and_Licor_interp <- as.list(new.env());
  TDL_and_Licor_interp$TDL <- TDL;
  TDL_and_Licor_interp$Licor_interp <- Licor_interp;

  return( TDL_and_Licor_interp );
  ### TDL_and_Licor_interp
}

