time_window_TDL_Licor_interp <-
function# keep only the overlapping time window of TDL and Licor files
### interpolate the Licor measurements via cubic splines to TDL faster sampling rate
### assign_variables names within TDL and Licor.interp
(TDL
###
, Licor
###
, TDL.Licor.times
###
, sw
###
)
{
  #try.zoo <- library(zoo, logical.return = TRUE); # date functions to merge TDL and Licor by time
  #
  ## install the package if not already in the library
  #if ( try.zoo == FALSE ) {
  #  install.packages("zoo");
  #  library(zoo);
  #}

  ##details<<
  ## BEGIN If use TDL
  if (sw$use.TDL) {
    ##details<<
    ## - Indices to keep based on TDL/Licor time window overlap.
    TDL.ind.keep        <- seq(TDL.Licor.times$TDL.time.ind.first,   TDL.Licor.times$TDL.time.ind.last);

    TDL$n               <- length(TDL.ind.keep);
    TDL$time            <- TDL$time[TDL.ind.keep];
    TDL$time_org        <- TDL$time_org[TDL.ind.keep];
    TDL$data            <- TDL$data[TDL.ind.keep,];

    ##details<<
    ## - TDL summary ind to keep.
    TDL.summary.ind        <- (TDL$last.list[,1] %in% TDL.ind.keep);
    TDL$last.list          <- TDL$last.list[TDL.summary.ind,];
    TDL$last.list[,c(1,2)] <- TDL$last.list[,c(1,2)]-TDL.Licor.times$TDL.time.ind.first+1; # reset indices

    ##details<<
    ## - Reduce TDL summary.
    TDL$summary$n         <- sum(TDL.summary.ind);
    TDL$summary$first.ind <- TDL$summary$first.ind[TDL.summary.ind] - TDL.Licor.times$TDL.time.ind.first + 1;
    TDL$summary$ind       <- TDL$summary$ind      [TDL.summary.ind] - TDL.Licor.times$TDL.time.ind.first + 1;
    TDL$summary$site      <- TDL$summary$site     [TDL.summary.ind];
    TDL$summary$n.sam     <- TDL$summary$n.sam    [TDL.summary.ind];
    TDL$summary$time      <- TDL$summary$time     [TDL.summary.ind];
    TDL$summary$mean      <- TDL$summary$mean     [TDL.summary.ind,];
    TDL$summary$var       <- TDL$summary$var      [TDL.summary.ind,];
    TDL$summary$sd        <- TDL$summary$sd       [TDL.summary.ind,];

    ##details<<
    ## - Reduce TDL interp.
    TDL$interp <- TDL$interp[TDL.ind.keep,];

    ## assign TDL variables  moved to read_TDL.R 8/18/2011 10:50AM
    #TDL$ind             <- 1:TDL$n;
  }
  ##details<<
  ## END

  ##details<<
  ## BEGIN If use Licor
  if (sw$use.Licor) {
    ##details<<
    ## - Indices to keep based on TDL/Licor time window overlap.
    Licor.ind.keep      <- seq(TDL.Licor.times$Licor.time.ind.first, TDL.Licor.times$Licor.time.ind.last);

    # note, if change this, also change same lines below in if() statement
    ## Licor
    Licor$n             <- length(Licor.ind.keep);
    Licor$time          <- Licor$time[Licor.ind.keep];
    Licor$data          <- Licor$data[Licor.ind.keep,];

    ### only consider non-NA values in TDL (NAs were substituted for pre-end tank values, transition periods)
    ##TDL.nonNA.ind <- !is.na(TDL$time);

    ##details<<
    ## - Use \code{zoo} to merge the times for TDL and Licor.

    if ((sw$use.TDL==0)) { # not using TDL, replace with Licor values used below
      TDL$n <- Licor$n;
      TDL$time_org <- Licor$time;
    }

    TDL.time.merge      <- zoo(1:TDL$n,   TDL$time_org);
    Licor.time.merge    <- zoo(1:Licor$n, Licor$time);
    TL.merge            <- merge(TDL.time.merge, Licor.time.merge);      # matching TDL with Licor times
    TDL.ind.match.Licor <- TL.merge[is.finite(TL.merge[,2]),1]; # indices of TDL matching Licor times
    #Licor.matched.with.non.NA.TDL <- (1:length(TDL.ind.match.Licor))[!is.na(TDL.ind.match.Licor)]; # 2/9/2012 0:22AM add - if NA in TDL file, skip those
    #ind.match <- TDL.ind.match.Licor[Licor.matched.with.non.NA.TDL];

    ## DEBUG 2/12/2012 9:45PM
    # TL.merge[is.na(TL.merge[,1]),]


    # 2/9/2012 2:01AM
    if (sum(is.na(TL.merge[,1]))) { # NA records in TDL file, see read_TDL.R for # Where TDL$StartSeqFlag!=0 or TDL$SeqActiveFlag==0
      p.o <- paste("WARNING: Times in Licor that do not appear in TDL", "\n"); wWw <- write_out(p.o);
      p.o <- paste("           (maybe an interval when TDL wasn't collecting data?)", "\n"); wWw <- write_out(p.o);
      p.o <- paste("         removing these time points from Licor data", "\n"); wWw <- write_out(p.o);
      p.o <- paste("         list of times:", "\n"); wWw <- write_out(p.o);
      Licor.ind.not.in.TDL <- as.numeric(TL.merge[is.na(TL.merge[,1]),2]);
      p.o <- paste("           ", Licor$time[Licor.ind.not.in.TDL], "\n"); wWw <- write_out(p.o);


        # not ideal, but redo Licor data lines without these observations
        Licor.ind.keep      <- setdiff(1:Licor$n, Licor.ind.not.in.TDL)
        Licor$n             <- length(Licor.ind.keep);
        Licor$time          <- Licor$time[Licor.ind.keep];
        Licor$data          <- Licor$data[Licor.ind.keep,];
        Licor.time.merge    <- zoo(1:Licor$n, Licor$time);
        TL.merge            <- merge(TDL.time.merge, Licor.time.merge);      # matching TDL with Licor times
        TDL.ind.match.Licor <- TL.merge[is.finite(TL.merge[,2]),1]; # indices of TDL matching Licor times
        if (sum(is.na(TL.merge[,1]))) { # NA records in TDL file, see read_TDL.R for # Where TDL$StartSeqFlag!=0 or TDL$SeqActiveFlag==0
          p.o <- paste("THIS WARNING SHOULD NEVER APPEAR, time_window_TDL_Licor_interp.R times1", "\n"); wWw <- write_out(p.o);
        }

      #(1:length(TL.merge[,1]))[is.na(TL.merge[,1])]
    }

    ##details<<
    ## Create \code{Licor.interp} to hold interpolated Licor values at resolution of TDL observations.

    Licor.interp <- as.list(new.env());  # create a list to return with data
    Licor.interp$data <- matrix(NA,nrow=TDL$n,ncol=dim(Licor$data)[2]);  # init matrix
    colnames(Licor.interp$data) <- colnames(Licor$data);                # assign col names
    for (i.col in 1:dim(Licor$data)[2]) {
      ##details<<
      ## populate with observed values in right locations
      Licor.interp$data[TDL.ind.match.Licor,i.col] <- Licor$data[,i.col];
      #Licor.interp$data[ind.match,i.col] <- Licor$data[Licor.matched.with.non.NA.TDL,i.col]; # populate with observed values in right locations # 2/9/2012 0:22AM change - if NA in TDL file, skip those
    }

    Licor.interp$ind    <- 1:TDL$n;
    Licor.interp$time   <- TDL$time_org;  # time is same as in TDL


    Licor.interp$n                <- TDL$n;
    Licor.interp$noninterp.ind    <- rep(0,TDL$n);
    ##details<<
    ## These indices have actual Licor values, the others are interpolated.
    Licor.interp$noninterp.ind[TDL.ind.match.Licor] <- 1;

    ##details<<
    ## Some Licor values maybe shouldn't be interpolated --
    for (i.col in 1:dim(Licor$data)[2]) {
      ##details<<
      ## If all values are NA, then consider column as a missing column and don't interp.
      if (sum(is.na(as.numeric(Licor$data[,i.col]))) != Licor$n) {
        ##details<<
        ## Cubic interpolation of Licor values to TDL sampling time points using \code{nknots}=\code{df}=
        ## Cubic spline interpolation using \code{smooth.spline} with \code{nknots}=\code{df}=number of Licor times matching TDL times.
        ss <- smooth.spline(Licor.interp$ind[TDL.ind.match.Licor], Licor.interp$data[TDL.ind.match.Licor,i.col], nknots=length(TDL.ind.match.Licor), df=length(TDL.ind.match.Licor));
        #ss <- smooth.spline(Licor.interp$ind[ind.match], Licor.interp$data[ind.match,i.col], nknots=length(ind.match), df=length(ind.match));  # 2/9/2012 1:16AM
        Licor.interp$data[,i.col] <- predict(ss,Licor.interp$ind)$y;
      }
      #plot(Licor.interp$ind[TDL.ind.match.Licor], Licor.interp$data[TDL.ind.match.Licor,i.col],pch=20)
      #points(ss,col="red")
      #points(Licor.interp$ind,Licor.interp$data[,i.col],col="green",type="l")
      #points(Licor.interp$ind,Licor.interp$data[,i.col],col="blue",pch=".")
    }
  } else {
    # not using Licor data
    Licor.interp <- Licor;
  }
  ##details<<
  ## END

  ##details<<
  ## Put TDL and Licor.interp together to return.
  TDL.and.Licor.interp <- as.list(new.env());
  TDL.and.Licor.interp$TDL <- TDL;
  TDL.and.Licor.interp$Licor.interp <- Licor.interp;

  return( TDL.and.Licor.interp );
  ### TDL.and.Licor.interp
}

