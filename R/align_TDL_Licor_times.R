align_TDL_Licor_times <-
function# find the overlapping time window of the TDL and Licor files and return the start/end indices of each file
###
(TDL.time
###
, TDL.n
###
, Licor.time
###
, Licor.n
###
, val.timewindow
###
, sw
###
)
{

    # DEBUGGING
    # TDL.time   <- TDL$time   ;
    # TDL.n      <- TDL$n      ;
    # Licor.time <- Licor$time ;
    # Licor.n    <- Licor$n    ;
    # val.timewindow <- val$timewindow ;

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
  TDL.run.time.first   <- if(sw$use.TDL  ) {as.POSIXct(min(TDL.time  , na.rm=TRUE))} else {NA}
  TDL.run.time.last    <- if(sw$use.TDL  ) {as.POSIXct(max(TDL.time  , na.rm=TRUE))} else {NA}
  Licor.run.time.first <- if(sw$use.Licor) {as.POSIXct(min(Licor.time, na.rm=TRUE))} else {NA}
  Licor.run.time.last  <- if(sw$use.Licor) {as.POSIXct(max(Licor.time, na.rm=TRUE))} else {NA}
  ##details<<
  ## Report first and last times.
  if(sw$use.TDL  ) {
    p.o <- paste("            TDL   first and last run times are ",
            TDL.run.time.first,
            " to ",
            TDL.run.time.last,
            "\n"); wWw <- write_out(p.o);
  }
  if(sw$use.Licor) {
    p.o <- paste("            Licor first and last run times are ",
            Licor.run.time.first,
            " to ",
            Licor.run.time.last,
            "\n"); wWw <- write_out(p.o);
  }

  ##details<<
  ## Indicate the time window specified.
  time.window.first <-
      if(!is.na(val.timewindow$start.time)) {if((val.timewindow$start.time > 0)) {as.POSIXct(val.timewindow$start.time)} else {NA}} else {NA};
  time.window.last <-
      if(!is.na(val.timewindow$end.time))   {if((val.timewindow$end.time > 0))   {as.POSIXct(val.timewindow$end.time)}   else {NA}} else {NA};

    p.o <- paste("            Specified time window    times are ",
            if(!is.na(time.window.first)) {time.window.first} else {"_BEGIN_"} ,
            " to ",
            if(!is.na(time.window.last)) {time.window.last} else {"_END_"} ,
            "\n"); wWw <- write_out(p.o);
  if (!is.na(time.window.first) & !is.na(time.window.last)) {
    if ( time.window.first >= time.window.last ) {
      p.o <- paste("ERROR -- ERROR -- Time Window start time is after end time \n"); wWw <- write_out(p.o);
      return (NULL);
    }
  }

  ##details<<
  ## Set TDL and Licor times to NA outside the time window.
  TDL.time.window   <- TDL.time;
  TDL.time.window  [(TDL.time.window   < time.window.first)] <- NA;
  TDL.time.window  [(TDL.time.window   > time.window.last )] <- NA;
  Licor.time.window <- Licor.time;
  Licor.time.window[(Licor.time.window < time.window.first)] <- NA;
  Licor.time.window[(Licor.time.window > time.window.last )] <- NA;

  ##details<<
  ## Indicate the first and last matching TDL and Licor times in time window.
  if (sw$use.TDL & sw$use.Licor) {
    # clumbsy: running these min/max lines repeatedly matches them up
    for (ii in 1:20) {
      # Set TDL and Licor times to NA outside the other (Licor and TDL) times
      Licor.time.window[(Licor.time.window < min(TDL.time.window,na.rm=TRUE)  )] <- NA;
      Licor.time.window[(Licor.time.window > max(TDL.time.window,na.rm=TRUE)  )] <- NA;
      TDL.time.window  [(TDL.time.window   < min(Licor.time.window,na.rm=TRUE))] <- NA;
      TDL.time.window  [(TDL.time.window   > max(Licor.time.window,na.rm=TRUE))] <- NA;
    }

    ## The above code works (looping over min/max), but something like below would be better.
    ## Want to match the lowest common time and the highest common time
    # TinL <- TDL.time.window %in% Licor.time.window; # indices in TDL also in Licor
    # LinT <- Licor.time.window %in% TDL.time.window;
    #
    # # indices of first and last in overlap window
    # temp.TDL.time.ind.first   <- min(seq(1,TDL.n)[TinL])  ;
    # temp.TDL.time.ind.last    <- max(seq(1,TDL.n)[TinL])  ;
    # temp.Licor.time.ind.first <- min(seq(1,Licor.n)[LinT]);
    # temp.Licor.time.ind.last  <- max(seq(1,Licor.n)[LinT]);
    #
    # # report first and last matching times
    # p.o <- paste("            First matching time TDL=",
    #         TDL.time[temp.TDL.time.ind.first],
    #         ",  Licor=",
    #         Licor.time[temp.Licor.time.ind.first], "\n"); wWw <- write_out(p.o);
    # p.o <- paste("            Last  matching time TDL=",
    #         TDL.time[temp.TDL.time.ind.last],
    #         ",  Licor=",
    #         Licor.time[temp.Licor.time.ind.last], "\n"); wWw <- write_out(p.o);

  }

  if (sw$use.TDL) {
    if (sum(!is.na(TDL.time.window))==0) {
      p.o <- paste("ERROR -- ERROR -- no TDL   times overlapping with Licor and time window \n"); wWw <- write_out(p.o);
      return (NULL);
    } else {
      TDL.time.window.first <- min(TDL.time.window,na.rm=TRUE);
      TDL.time.window.last  <- max(TDL.time.window,na.rm=TRUE);
    }
  }
  if (sw$use.Licor) {
    if (sum(!is.na(Licor.time.window))==0) {
      p.o <- paste("ERROR -- ERROR -- no Licor times overlapping with TDL   and time window \n"); wWw <- write_out(p.o);
      return (NULL);
    } else {
      Licor.time.window.first <- min(Licor.time.window,na.rm=TRUE);
      Licor.time.window.last  <- max(Licor.time.window,na.rm=TRUE);
    }
  }

  if (sw$use.TDL & sw$use.Licor) {
    ##details<<
    ## Check that TDL and Licor first and last times are identical.
    if (!(TDL.time.window.first == Licor.time.window.first)) {
      p.o <- paste("WARNING: first time window times don't match, are - ",
              "TDL:", TDL.time.window.first,
              ", Licor:", Licor.time.window.first,
              "\n"); wWw <- write_out(p.o);
    }
    if (!(TDL.time.window.last == Licor.time.window.last)) {
      p.o <- paste("WARNING: last  time window times don't match, are - ",
              "TDL:", TDL.time.window.last,
              ", Licor:", Licor.time.window.last,
              "\n"); wWw <- write_out(p.o);
    }
  }

  ##details<<
  ## Indices for first and last times in TDL and Licor files.
  if (sw$use.TDL) {
    TDL.time.ind.first   <- seq(1,TDL.n  )[(!is.na(TDL.time  ) & (TDL.time   == TDL.time.window.first  ))];
    TDL.time.ind.last    <- seq(1,TDL.n  )[(!is.na(TDL.time  ) & (TDL.time   == TDL.time.window.last   ))];
    p.o <- paste("            TDL   first and last times used are ",
            "index ", TDL.time.ind.first, " time ", TDL.time.window.first,
            " to ",
            "index ", TDL.time.ind.last, " time ", TDL.time.window.last,
            "\n"); wWw <- write_out(p.o);
  } else {
    TDL.time.ind.first   <- NA;
    TDL.time.ind.last    <- NA;
  }
  if (sw$use.Licor) {
    Licor.time.ind.first <- seq(1,Licor.n)[(!is.na(Licor.time) & (Licor.time == Licor.time.window.first))];
    Licor.time.ind.last  <- seq(1,Licor.n)[(!is.na(Licor.time) & (Licor.time == Licor.time.window.last ))];
    p.o <- paste("            Licor first and last times used are ",
            "index ", Licor.time.ind.first, " time ", Licor.time.window.first,
            " to ",
            "index ", Licor.time.ind.last, " time ", Licor.time.window.last,
            "\n"); wWw <- write_out(p.o);
  } else {
    Licor.time.ind.first   <- NA;
    Licor.time.ind.last    <- NA;
  }

  if (sw$use.TDL & sw$use.Licor) {
    TDL.time.points   <- TDL.time.ind.last   - TDL.time.ind.first  ;
    Licor.time.points <- Licor.time.ind.last - Licor.time.ind.first;
    p.o <- paste("            On average there are roughly", format(TDL.time.points/Licor.time.points, digits=4), "TDL timepoints per Licor timepoint.", "\n"); wWw <- write_out(p.o);
  } # if TDL & Licor

  # Time overlap 9/8/2011
  ##details<<
  ## Use narrower timewindow start and end times, if specified.
  window.start <-
    max(c(
        if(sw$use.TDL  )                    {as.POSIXct(TDL.time.window.first  )} else {NA}
      , if(sw$use.Licor)                    {as.POSIXct(Licor.time.window.first)} else {NA}
    ), na.rm=TRUE);
  window.end <-
    min(c(
        if(sw$use.TDL  )                    {as.POSIXct(TDL.time.window.last  )} else {NA}
      , if(sw$use.Licor)                    {as.POSIXct(Licor.time.window.last)} else {NA}
    ), na.rm=TRUE);

  p.o <- paste("            Time window used for analysis is: ", window.start, " to ", window.end, ".", "\n"); wWw <- write_out(p.o);

  ## all new code above, 10/4/2011
  ## commented code (at bottom) was removed from here on 10/4/2011

  TDL.Licor.times <- as.list(new.env());  # create a list to return with data

  TDL.Licor.times$TDL.time.ind.first   <- TDL.time.ind.first   ;
  TDL.Licor.times$TDL.time.ind.last    <- TDL.time.ind.last    ;
  TDL.Licor.times$Licor.time.ind.first <- Licor.time.ind.first ;
  TDL.Licor.times$Licor.time.ind.last  <- Licor.time.ind.last  ;

  return( TDL.Licor.times );
  ### TDL.Licor.times
}


  ## commented 10/4/2011, new code is above
  # if (sw$use.TDL & sw$use.Licor) {
  #
  #   ## pre-timewindow way of finding start/end times  9/8/2011
  #   # indices of overlap window for each file
  #   TinL <- TDL.time %in% Licor.time; # indices in TDL also in Licor
  #   LinT <- Licor.time %in% TDL.time;
  #
  #   if ( (sum(TinL) == 0) || (sum(LinT) == 0) ) {
  #     p.o <- paste("ERROR -- ERROR -- TDL and Licor times do not overlap \n"); wWw <- write_out(p.o);
  #     return (NULL);
  #   }
  #
  #   # indices of first and last in overlap window
  #   temp.TDL.time.ind.first   <- min(seq(1,TDL.n)[TinL])  ;
  #   temp.TDL.time.ind.last    <- max(seq(1,TDL.n)[TinL])  ;
  #   temp.Licor.time.ind.first <- min(seq(1,Licor.n)[LinT]);
  #   temp.Licor.time.ind.last  <- max(seq(1,Licor.n)[LinT]);
  #
  #   # report first and last matching times
  #   p.o <- paste("            First matching time TDL=",
  #           TDL.time[temp.TDL.time.ind.first],
  #           ",  Licor=",
  #           Licor.time[temp.Licor.time.ind.first], "\n"); wWw <- write_out(p.o);
  #   p.o <- paste("            Last  matching time TDL=",
  #           TDL.time[temp.TDL.time.ind.last],
  #           ",  Licor=",
  #           Licor.time[temp.Licor.time.ind.last], "\n"); wWw <- write_out(p.o);
  #
  #   TDL.time.points   <- temp.TDL.time.ind.last   - temp.TDL.time.ind.first  ;
  #   Licor.time.points <- temp.Licor.time.ind.last - temp.Licor.time.ind.first;
  #   p.o <- paste("            On average there are roughly", format(TDL.time.points/Licor.time.points, digits=4), "TDL timepoints per Licor timepoint.", "\n"); wWw <- write_out(p.o);
  #
  #   # # Time overlap 9/8/2011
  #   # # use timewindow start time
  #   # if ((val.timewindow$start.time > 0) & !is.na(val.timewindow$start.time)) {
  #   #   window.start <-
  #   #     max(c(
  #   #         as.POSIXct(min(TDL.time, na.rm=TRUE))
  #   #       , as.POSIXct(min(Licor.time, na.rm=TRUE))
  #   #       , as.POSIXct(val.timewindow$start.time)
  #   #     ));
  #   # } else {
  #   #   window.start <-
  #   #     max(c(
  #   #         as.POSIXct(min(TDL.time, na.rm=TRUE))
  #   #       , as.POSIXct(min(Licor.time, na.rm=TRUE))
  #   #     ));
  #   # };
  #   #
  #   # if ((val.timewindow$end.time > 0) & !is.na(val.timewindow$end.time)) {
  #   #   window.end <-
  #   #     min(c(
  #   #         as.POSIXct(max(TDL.time, na.rm=TRUE))
  #   #       , as.POSIXct(max(Licor.time, na.rm=TRUE))
  #   #       , as.POSIXct(val.timewindow$end.time)
  #   #     ));
  #   # } else {
  #   #   window.end <-
  #   #     min(c(
  #   #         as.POSIXct(max(TDL.time, na.rm=TRUE))
  #   #       , as.POSIXct(max(Licor.time, na.rm=TRUE))
  #   #     ));
  #   # };
  #   #
  #   # if ( window.start >= window.end ) {
  #   #   p.o <- paste("ERROR -- ERROR -- Time Window start time is after end time \n"); wWw <- write_out(p.o);
  #   #   return (NULL);
  #   # }
  #   #
  #   # # indices of first and last in overlap window
  #   # TDL.time.ind.first   <- min(seq(1,TDL.n)[!is.na(TDL.time)][TDL.time[!is.na(TDL.time)] >= window.start]);
  #   # TDL.time.ind.last    <- max(seq(1,TDL.n)[!is.na(TDL.time)][TDL.time[!is.na(TDL.time)] <= window.end  ]);
  #   # Licor.time.ind.first <- min(seq(1,Licor.n)[!is.na(Licor.time)][Licor.time[!is.na(Licor.time)] >= window.start]);
  #   # Licor.time.ind.last  <- max(seq(1,Licor.n)[!is.na(Licor.time)][Licor.time[!is.na(Licor.time)] <= window.end  ]);
  #   #
  #   # p.o <- paste("            Time window used for analysis is: ", window.start, " to ", window.end, ".", "\n"); wWw <- write_out(p.o);
  #
  # } # if TDL & Licor
  #
  # # if (sw$use.TDL & (sw$use.Licor==0)) { # TDL data only
  # #   TDL.time.ind.first   <- 1;
  # #   TDL.time.ind.last    <- TDL.n;
  # #   Licor.time.ind.first <- NA;
  # #   Licor.time.ind.last  <- NA;
  # # };
  # # if ((sw$use.TDL==0) & sw$use.Licor) { # Licor data only
  # #   TDL.time.ind.first   <- NA;
  # #   TDL.time.ind.last    <- NA;
  # #   Licor.time.ind.first <- 1;
  # #   Licor.time.ind.last  <- Licor.n;
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
  # window.start <-
  #   max(c(
  #       if(sw$use.TDL  )                    {as.POSIXct(min(TDL.time  , na.rm=TRUE))} else {NA}
  #     , if(sw$use.Licor)                    {as.POSIXct(min(Licor.time, na.rm=TRUE))} else {NA}
  #     , if(!is.na(val.timewindow$start.time)) {if((val.timewindow$start.time > 0)) {as.POSIXct(val.timewindow$start.time)}   else {NA}} else {NA}
  #   ), na.rm=TRUE);
  # window.end <-
  #   min(c(
  #       if(sw$use.TDL  )                    {as.POSIXct(max(TDL.time  , na.rm=TRUE))} else {NA}
  #     , if(sw$use.Licor)                    {as.POSIXct(max(Licor.time, na.rm=TRUE))} else {NA}
  #     , if(!is.na(val.timewindow$end.time)) {if((val.timewindow$end.time > 0)) {as.POSIXct(val.timewindow$end.time)}   else {NA}} else {NA}
  #   ), na.rm=TRUE);
  #
  # if ( window.start >= window.end ) {
  #   p.o <- paste("ERROR -- ERROR -- Time Window start time is after end time \n"); wWw <- write_out(p.o);
  #   return (NULL);
  # }
  #
  # p.o <- paste("            Time window used for analysis is: ", window.start, " to ", window.end, ".", "\n"); wWw <- write_out(p.o);
  #
  # # indices of first and last in overlap window
  # if (sw$use.TDL) { # TDL
  #   TDL.time.ind.first   <- min(seq(1,TDL.n)[!is.na(TDL.time)][TDL.time[!is.na(TDL.time)] >= window.start]);
  #   TDL.time.ind.last    <- max(seq(1,TDL.n)[!is.na(TDL.time)][TDL.time[!is.na(TDL.time)] <= window.end  ]);
  # } else {
  #   TDL.time.ind.first   <- NA;
  #   TDL.time.ind.last    <- NA;
  # };
  # if (sw$use.Licor) { # Licor
  #   Licor.time.ind.first <- min(seq(1,Licor.n)[!is.na(Licor.time)][Licor.time[!is.na(Licor.time)] >= window.start]);
  #   Licor.time.ind.last  <- max(seq(1,Licor.n)[!is.na(Licor.time)][Licor.time[!is.na(Licor.time)] <= window.end  ]);
  # } else {
  #   Licor.time.ind.first <- NA;
  #   Licor.time.ind.last  <- NA;
  # };
