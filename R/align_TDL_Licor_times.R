#' Find the overlapping time window of the TDL and Licor files and return the start/end indices of each file
#'
#' Unused sites (comment):
#' When sites are specified in TDL file that are not in the template file (because more than 4 sites are used),
#' then there are NAs for times for used sites.  In this case, the matching times above may be different
#' from the min/max times below.  In this case, we should use the matching times (that is, always use the matching times)
#' otherwise errors result from NA times in \code{\link{time_window_TDL_Licor_interp}}.
#' The logic is: use the widest matching times within the timewindow.
#'
#' Indicate first and last times for TDL and Licor.
#'
#' Report first and last times.
#'
#' Indicate the time window specified.
#'
#' Set TDL and Licor times to NA outside the time window.
#'
#' Indicate the first and last matching TDL and Licor times in time window.
#'
#' Check that TDL and Licor first and last times are identical.
#'
#' Indices for first and last times in TDL and Licor files.
#'
#' Use narrower timewindow start and end times, if specified.
#'
#' @param TDL_time xxxPARAMxxx
#' @param TDL_n xxxPARAMxxx
#' @param Licor_time xxxPARAMxxx
#' @param Licor_n xxxPARAMxxx
#' @param val_timewindow xxxPARAMxxx
#' @param sw xxxPARAMxxx
#'
#' @return TDL_Licor_times xxxRETURNxxx
#'
align_TDL_Licor_times <-
function# find the overlapping time window of the TDL and Licor files and return the start/end indices of each file
###
(TDL_time
###
, TDL_n
###
, Licor_time
###
, Licor_n
###
, val_timewindow
###
, sw
###
)
{

    # DEBUGGING
    # TDL_time   <- TDL$time   ;
    # TDL_n      <- TDL$n      ;
    # Licor_time <- Licor$time ;
    # Licor_n    <- Licor$n    ;
    # val_timewindow <- val$timewindow ;

  # 10/2/2011 1:59PM
  ##details<<
  ## Unused sites (comment):
  ##  When sites are specified in TDL file that are not in the template file (because more than 4 sites are used),
  ##  then there are NAs for times for used sites.  In this case, the matching times above may be different
  ##  from the min/max times below.  In this case, we should use the matching times (that is, always use the matching times)
  ##  otherwise errors result from NA times in \code{\link{time_window_TDL_Licor_interp}}.
  ## The logic is: use the widest matching times within the timewindow.

  ##details<<
  ## Indicate first and last times for TDL and Licor.
  TDL_run_time_first   <- if(sw$use_TDL  ) {as.POSIXct(min(TDL_time  , na.rm=TRUE))} else {NA}
  TDL_run_time_last    <- if(sw$use_TDL  ) {as.POSIXct(max(TDL_time  , na.rm=TRUE))} else {NA}
  Licor_run_time_first <- if(sw$use_Licor) {as.POSIXct(min(Licor_time, na.rm=TRUE))} else {NA}
  Licor_run_time_last  <- if(sw$use_Licor) {as.POSIXct(max(Licor_time, na.rm=TRUE))} else {NA}
  ##details<<
  ## Report first and last times.
  if(sw$use_TDL  ) {
    p_o <- paste("            TDL   first and last run times are ",
            TDL_run_time_first,
            " to ",
            TDL_run_time_last,
            "\n"); wWw <- write_out(p_o);
  }
  if(sw$use_Licor) {
    p_o <- paste("            Licor first and last run times are ",
            Licor_run_time_first,
            " to ",
            Licor_run_time_last,
            "\n"); wWw <- write_out(p_o);
  }

  ##details<<
  ## Indicate the time window specified.
  time_window_first <-
      if(!is.na(val_timewindow$start_time)) {if((val_timewindow$start_time > 0)) {as.POSIXct(val_timewindow$start_time)} else {NA}} else {NA};
  time_window_last <-
      if(!is.na(val_timewindow$end_time))   {if((val_timewindow$end_time > 0))   {as.POSIXct(val_timewindow$end_time)}   else {NA}} else {NA};

    p_o <- paste("            Specified time window    times are ",
            if(!is.na(time_window_first)) {time_window_first} else {"_BEGIN_"} ,
            " to ",
            if(!is.na(time_window_last)) {time_window_last} else {"_END_"} ,
            "\n"); wWw <- write_out(p_o);
  if (!is.na(time_window_first) & !is.na(time_window_last)) {
    if ( time_window_first >= time_window_last ) {
      p_o <- paste("ERROR -- ERROR -- Time Window start time is after end time \n"); wWw <- write_out(p_o);
      return (NULL);
    }
  }

  ##details<<
  ## Set TDL and Licor times to NA outside the time window.
  TDL_time_window   <- TDL_time;
  TDL_time_window  [(TDL_time_window   < time_window_first)] <- NA;
  TDL_time_window  [(TDL_time_window   > time_window_last )] <- NA;
  Licor_time_window <- Licor_time;
  Licor_time_window[(Licor_time_window < time_window_first)] <- NA;
  Licor_time_window[(Licor_time_window > time_window_last )] <- NA;

  ##details<<
  ## Indicate the first and last matching TDL and Licor times in time window.
  if (sw$use_TDL & sw$use_Licor) {
    # clumbsy: running these min/max lines repeatedly matches them up
    for (ii in 1:20) {
      # Set TDL and Licor times to NA outside the other (Licor and TDL) times
      Licor_time_window[(Licor_time_window < min(TDL_time_window,na.rm=TRUE)  )] <- NA;
      Licor_time_window[(Licor_time_window > max(TDL_time_window,na.rm=TRUE)  )] <- NA;
      TDL_time_window  [(TDL_time_window   < min(Licor_time_window,na.rm=TRUE))] <- NA;
      TDL_time_window  [(TDL_time_window   > max(Licor_time_window,na.rm=TRUE))] <- NA;
    }

    ## The above code works (looping over min/max), but something like below would be better.
    ## Want to match the lowest common time and the highest common time
    # TinL <- TDL_time_window %in% Licor_time_window; # indices in TDL also in Licor
    # LinT <- Licor_time_window %in% TDL_time_window;
    #
    # # indices of first and last in overlap window
    # temp_TDL_time_ind_first   <- min(seq(1,TDL_n)[TinL])  ;
    # temp_TDL_time_ind_last    <- max(seq(1,TDL_n)[TinL])  ;
    # temp_Licor_time_ind_first <- min(seq(1,Licor_n)[LinT]);
    # temp_Licor_time_ind_last  <- max(seq(1,Licor_n)[LinT]);
    #
    # # report first and last matching times
    # p_o <- paste("            First matching time TDL=",
    #         TDL_time[temp_TDL_time_ind_first],
    #         ",  Licor=",
    #         Licor_time[temp_Licor_time_ind_first], "\n"); wWw <- write_out(p_o);
    # p_o <- paste("            Last  matching time TDL=",
    #         TDL_time[temp_TDL_time_ind_last],
    #         ",  Licor=",
    #         Licor_time[temp_Licor_time_ind_last], "\n"); wWw <- write_out(p_o);

  }

  if (sw$use_TDL) {
    if (sum(!is.na(TDL_time_window))==0) {
      p_o <- paste("ERROR -- ERROR -- no TDL   times overlapping with Licor and time window \n"); wWw <- write_out(p_o);
      return (NULL);
    } else {
      TDL_time_window_first <- min(TDL_time_window,na.rm=TRUE);
      TDL_time_window_last  <- max(TDL_time_window,na.rm=TRUE);
    }
  }
  if (sw$use_Licor) {
    if (sum(!is.na(Licor_time_window))==0) {
      p_o <- paste("ERROR -- ERROR -- no Licor times overlapping with TDL   and time window \n"); wWw <- write_out(p_o);
      return (NULL);
    } else {
      Licor_time_window_first <- min(Licor_time_window,na.rm=TRUE);
      Licor_time_window_last  <- max(Licor_time_window,na.rm=TRUE);
    }
  }

  if (sw$use_TDL & sw$use_Licor) {
    ##details<<
    ## Check that TDL and Licor first and last times are identical.
    if (!(TDL_time_window_first == Licor_time_window_first)) {
      p_o <- paste("WARNING: first time window times don't match, are - ",
              "TDL:", TDL_time_window_first,
              ", Licor:", Licor_time_window_first,
              "\n"); wWw <- write_out(p_o);
    }
    if (!(TDL_time_window_last == Licor_time_window_last)) {
      p_o <- paste("WARNING: last  time window times don't match, are - ",
              "TDL:", TDL_time_window_last,
              ", Licor:", Licor_time_window_last,
              "\n"); wWw <- write_out(p_o);
    }
  }

  ##details<<
  ## Indices for first and last times in TDL and Licor files.
  if (sw$use_TDL) {
    TDL_time_ind_first   <- seq(1,TDL_n  )[(!is.na(TDL_time  ) & (TDL_time   == TDL_time_window_first  ))];
    TDL_time_ind_last    <- seq(1,TDL_n  )[(!is.na(TDL_time  ) & (TDL_time   == TDL_time_window_last   ))];
    p_o <- paste("            TDL   first and last times used are ",
            "index ", TDL_time_ind_first, " time ", TDL_time_window_first,
            " to ",
            "index ", TDL_time_ind_last, " time ", TDL_time_window_last,
            "\n"); wWw <- write_out(p_o);
  } else {
    TDL_time_ind_first   <- NA;
    TDL_time_ind_last    <- NA;
  }
  if (sw$use_Licor) {
    Licor_time_ind_first <- seq(1,Licor_n)[(!is.na(Licor_time) & (Licor_time == Licor_time_window_first))];
    Licor_time_ind_last  <- seq(1,Licor_n)[(!is.na(Licor_time) & (Licor_time == Licor_time_window_last ))];
    p_o <- paste("            Licor first and last times used are ",
            "index ", Licor_time_ind_first, " time ", Licor_time_window_first,
            " to ",
            "index ", Licor_time_ind_last, " time ", Licor_time_window_last,
            "\n"); wWw <- write_out(p_o);
  } else {
    Licor_time_ind_first   <- NA;
    Licor_time_ind_last    <- NA;
  }

  if (sw$use_TDL & sw$use_Licor) {
    TDL_time_points   <- TDL_time_ind_last   - TDL_time_ind_first  ;
    Licor_time_points <- Licor_time_ind_last - Licor_time_ind_first;
    p_o <- paste("            On average there are roughly", format(TDL_time_points/Licor_time_points, digits=4), "TDL timepoints per Licor timepoint.", "\n"); wWw <- write_out(p_o);
  } # if TDL & Licor

  # Time overlap 9/8/2011
  ##details<<
  ## Use narrower timewindow start and end times, if specified.
  window_start <-
    max(c(
        if(sw$use_TDL  )                    {as.POSIXct(TDL_time_window_first  )} else {NA}
      , if(sw$use_Licor)                    {as.POSIXct(Licor_time_window_first)} else {NA}
    ), na.rm=TRUE);
  window_end <-
    min(c(
        if(sw$use_TDL  )                    {as.POSIXct(TDL_time_window_last  )} else {NA}
      , if(sw$use_Licor)                    {as.POSIXct(Licor_time_window_last)} else {NA}
    ), na.rm=TRUE);

  p_o <- paste("            Time window used for analysis is: ", window_start, " to ", window_end, ".", "\n"); wWw <- write_out(p_o);

  ## all new code above, 10/4/2011
  ## commented code (at bottom) was removed from here on 10/4/2011

  TDL_Licor_times <- as.list(new.env());  # create a list to return with data

  TDL_Licor_times$TDL_time_ind_first   <- TDL_time_ind_first   ;
  TDL_Licor_times$TDL_time_ind_last    <- TDL_time_ind_last    ;
  TDL_Licor_times$Licor_time_ind_first <- Licor_time_ind_first ;
  TDL_Licor_times$Licor_time_ind_last  <- Licor_time_ind_last  ;

  return( TDL_Licor_times );
  ### TDL_Licor_times
}


  ## commented 10/4/2011, new code is above
  # if (sw$use_TDL & sw$use_Licor) {
  #
  #   ## pre-timewindow way of finding start/end times  9/8/2011
  #   # indices of overlap window for each file
  #   TinL <- TDL_time %in% Licor_time; # indices in TDL also in Licor
  #   LinT <- Licor_time %in% TDL_time;
  #
  #   if ( (sum(TinL) == 0) || (sum(LinT) == 0) ) {
  #     p_o <- paste("ERROR -- ERROR -- TDL and Licor times do not overlap \n"); wWw <- write_out(p_o);
  #     return (NULL);
  #   }
  #
  #   # indices of first and last in overlap window
  #   temp_TDL_time_ind_first   <- min(seq(1,TDL_n)[TinL])  ;
  #   temp_TDL_time_ind_last    <- max(seq(1,TDL_n)[TinL])  ;
  #   temp_Licor_time_ind_first <- min(seq(1,Licor_n)[LinT]);
  #   temp_Licor_time_ind_last  <- max(seq(1,Licor_n)[LinT]);
  #
  #   # report first and last matching times
  #   p_o <- paste("            First matching time TDL=",
  #           TDL_time[temp_TDL_time_ind_first],
  #           ",  Licor=",
  #           Licor_time[temp_Licor_time_ind_first], "\n"); wWw <- write_out(p_o);
  #   p_o <- paste("            Last  matching time TDL=",
  #           TDL_time[temp_TDL_time_ind_last],
  #           ",  Licor=",
  #           Licor_time[temp_Licor_time_ind_last], "\n"); wWw <- write_out(p_o);
  #
  #   TDL_time_points   <- temp_TDL_time_ind_last   - temp_TDL_time_ind_first  ;
  #   Licor_time_points <- temp_Licor_time_ind_last - temp_Licor_time_ind_first;
  #   p_o <- paste("            On average there are roughly", format(TDL_time_points/Licor_time_points, digits=4), "TDL timepoints per Licor timepoint.", "\n"); wWw <- write_out(p_o);
  #
  #   # # Time overlap 9/8/2011
  #   # # use timewindow start time
  #   # if ((val_timewindow$start_time > 0) & !is.na(val_timewindow$start_time)) {
  #   #   window_start <-
  #   #     max(c(
  #   #         as.POSIXct(min(TDL_time, na.rm=TRUE))
  #   #       , as.POSIXct(min(Licor_time, na.rm=TRUE))
  #   #       , as.POSIXct(val_timewindow$start_time)
  #   #     ));
  #   # } else {
  #   #   window_start <-
  #   #     max(c(
  #   #         as.POSIXct(min(TDL_time, na.rm=TRUE))
  #   #       , as.POSIXct(min(Licor_time, na.rm=TRUE))
  #   #     ));
  #   # };
  #   #
  #   # if ((val_timewindow$end_time > 0) & !is.na(val_timewindow$end_time)) {
  #   #   window_end <-
  #   #     min(c(
  #   #         as.POSIXct(max(TDL_time, na.rm=TRUE))
  #   #       , as.POSIXct(max(Licor_time, na.rm=TRUE))
  #   #       , as.POSIXct(val_timewindow$end_time)
  #   #     ));
  #   # } else {
  #   #   window_end <-
  #   #     min(c(
  #   #         as.POSIXct(max(TDL_time, na.rm=TRUE))
  #   #       , as.POSIXct(max(Licor_time, na.rm=TRUE))
  #   #     ));
  #   # };
  #   #
  #   # if ( window_start >= window_end ) {
  #   #   p_o <- paste("ERROR -- ERROR -- Time Window start time is after end time \n"); wWw <- write_out(p_o);
  #   #   return (NULL);
  #   # }
  #   #
  #   # # indices of first and last in overlap window
  #   # TDL_time_ind_first   <- min(seq(1,TDL_n)[!is.na(TDL_time)][TDL_time[!is.na(TDL_time)] >= window_start]);
  #   # TDL_time_ind_last    <- max(seq(1,TDL_n)[!is.na(TDL_time)][TDL_time[!is.na(TDL_time)] <= window_end  ]);
  #   # Licor_time_ind_first <- min(seq(1,Licor_n)[!is.na(Licor_time)][Licor_time[!is.na(Licor_time)] >= window_start]);
  #   # Licor_time_ind_last  <- max(seq(1,Licor_n)[!is.na(Licor_time)][Licor_time[!is.na(Licor_time)] <= window_end  ]);
  #   #
  #   # p_o <- paste("            Time window used for analysis is: ", window_start, " to ", window_end, ".", "\n"); wWw <- write_out(p_o);
  #
  # } # if TDL & Licor
  #
  # # if (sw$use_TDL & (sw$use_Licor==0)) { # TDL data only
  # #   TDL_time_ind_first   <- 1;
  # #   TDL_time_ind_last    <- TDL_n;
  # #   Licor_time_ind_first <- NA;
  # #   Licor_time_ind_last  <- NA;
  # # };
  # # if ((sw$use_TDL==0) & sw$use_Licor) { # Licor data only
  # #   TDL_time_ind_first   <- NA;
  # #   TDL_time_ind_last    <- NA;
  # #   Licor_time_ind_first <- 1;
  # #   Licor_time_ind_last  <- Licor_n;
  # # };
  #
  # # 10/2/2011 1:59PM
  # #  When sites are specified in TDL file that are not in the template file (because more than 4 sites are used),
  # #  then there are NAs for times for used sites.  In this case, the matching times above may be different
  # #  from the min/max times below.  In this case, we should use the matching times (that is, always use the matching times)
  # #  otherwise errors result from NA times in time_window_TDL_Licor_interp().
  # # The logic is: use the widest matching times within the timewindow.
  #
  # # Time overlap 9/8/2011
  # # use timewindow start time
  # window_start <-
  #   max(c(
  #       if(sw$use_TDL  )                    {as.POSIXct(min(TDL_time  , na.rm=TRUE))} else {NA}
  #     , if(sw$use_Licor)                    {as.POSIXct(min(Licor_time, na.rm=TRUE))} else {NA}
  #     , if(!is.na(val_timewindow$start_time)) {if((val_timewindow$start_time > 0)) {as.POSIXct(val_timewindow$start_time)}   else {NA}} else {NA}
  #   ), na.rm=TRUE);
  # window_end <-
  #   min(c(
  #       if(sw$use_TDL  )                    {as.POSIXct(max(TDL_time  , na.rm=TRUE))} else {NA}
  #     , if(sw$use_Licor)                    {as.POSIXct(max(Licor_time, na.rm=TRUE))} else {NA}
  #     , if(!is.na(val_timewindow$end_time)) {if((val_timewindow$end_time > 0)) {as.POSIXct(val_timewindow$end_time)}   else {NA}} else {NA}
  #   ), na.rm=TRUE);
  #
  # if ( window_start >= window_end ) {
  #   p_o <- paste("ERROR -- ERROR -- Time Window start time is after end time \n"); wWw <- write_out(p_o);
  #   return (NULL);
  # }
  #
  # p_o <- paste("            Time window used for analysis is: ", window_start, " to ", window_end, ".", "\n"); wWw <- write_out(p_o);
  #
  # # indices of first and last in overlap window
  # if (sw$use_TDL) { # TDL
  #   TDL_time_ind_first   <- min(seq(1,TDL_n)[!is.na(TDL_time)][TDL_time[!is.na(TDL_time)] >= window_start]);
  #   TDL_time_ind_last    <- max(seq(1,TDL_n)[!is.na(TDL_time)][TDL_time[!is.na(TDL_time)] <= window_end  ]);
  # } else {
  #   TDL_time_ind_first   <- NA;
  #   TDL_time_ind_last    <- NA;
  # };
  # if (sw$use_Licor) { # Licor
  #   Licor_time_ind_first <- min(seq(1,Licor_n)[!is.na(Licor_time)][Licor_time[!is.na(Licor_time)] >= window_start]);
  #   Licor_time_ind_last  <- max(seq(1,Licor_n)[!is.na(Licor_time)][Licor_time[!is.na(Licor_time)] <= window_end  ]);
  # } else {
  #   Licor_time_ind_first <- NA;
  #   Licor_time_ind_last  <- NA;
  # };
