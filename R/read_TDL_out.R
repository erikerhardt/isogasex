read_TDL <-
function# read_TDL file
###
(TDL.fn
###
, sw
###
)
{
  ##details<<
  ## See \code{\link{val_TDL_Licor_variables}} for expected column names.

  ##details<<
  ## Create \code{TDL} to hold data.
  TDL <- as.list(new.env());  # create a list to return with data

  if (sw$use.TDL  ) {
    ####################
    ##details<<
    ## Read TDL file
    p.o <- paste("               Reading TDL   file: ", TDL.fn, "\n"); wWw <- write_out(p.o);
    TDL$program.name <- read.csv(TDL.fn, header=FALSE, nrows=1)[6]; # program name from header
    TDL$header.names <- read.csv(TDL.fn, header=FALSE, nrows=1, skip=1); # column headers names
    TDL$data <- read.csv(TDL.fn, header=FALSE, skip=4, na.strings=c("\"NaN\"","NaN","\"NAN\"","NAN"));
    colnames(TDL$data) <- t(TDL$header.names); # assign column header names

       # 10 Hz data from TDL: TOA5_2257.RawData_2009_08_18_0956.dat
       #    Columns
       #       3,4:  unique identifiers for each site.
       #       5:    0=functioning, -1=not functioning
       #       6:    1 minus [col5]


    ##details<<
    ## Report times where cycle times when TDL program was not running 2/9/2012 1:18AM.
    TDL.notrun.ind <- (TDL$data[, 6] == 0);   # Where TDL$StartSeqFlag!=0 or TDL$SeqActiveFlag==0
    #if (length(TDL.notrun.ind)) { # if there are non functioning cycle times
    # CHANGED from "length" to "sum" 3/1/2012 11:45AM
    if (sum(TDL.notrun.ind)) { # if there are non functioning cycle times
      TDL.notrun.ind.num <- (1:length(TDL$data[,6]))[TDL.notrun.ind]; # row numbers
      TDL.notrun.ind.num.diff <- diff(TDL.notrun.ind.num); # difference between numbers to find blocks edges
      TDL.notrun.ind.num.diff.nonone <- (1:length(TDL.notrun.ind.num.diff))[(TDL.notrun.ind.num.diff != 1)]; # where do new blocks begin
      TDL.remove.blocks.n <- length(TDL.notrun.ind.num.diff.nonone)+1;

      # first index, all other starting indices, and last index+1
      TDL.remove.first.to.last.subind <- c(1, TDL.notrun.ind.num.diff.nonone+1, length(TDL.notrun.ind.num)+1);

      p.o <- paste("WARNING: There are ", TDL.remove.blocks.n, " block(s) of time in TDL file where SeqActiveFlag==0", "\n"); wWw <- write_out(p.o);
      p.o <- paste("            These blocks of time will NOT BE USED only if at beginning or end of file:", "\n"); wWw <- write_out(p.o);
      for (i in 1:TDL.remove.blocks.n) {
        ind.first <- TDL.notrun.ind.num[TDL.remove.first.to.last.subind[i]];
        ind.last  <- TDL.notrun.ind.num[TDL.remove.first.to.last.subind[i+1]-1];
        p.o <- paste("            ", (ind.last-ind.first+1),
                  " records: ", (1:length(TDL$data[,6]))[ind.first], " to ", (1:length(TDL$data[,6]))[ind.last],
                  " times: ", TDL$data$TIMESTAMP[ind.first], " to ", TDL$data$TIMESTAMP[ind.last], "\n"); wWw <- write_out(p.o);

        if (!((ind.first==1) | (ind.last==length(TDL$data[,6])))) { # if not the beginning or end of file, include those points
          TDL.notrun.ind[ind.first:ind.last] <- FALSE; # keep points all points (including where TDL program was not running in middle) 2/12/2012 7:49PM
          p.o <- paste("            NOTE: keeping these points as part of analysis", "\n"); wWw <- write_out(p.o);
        }

      }
      p.o <- paste("            This MAY CAUSE AN ERROR during \"interp Licor values to TDL timepoints\" below", "\n"); wWw <- write_out(p.o);
      p.o <- paste("            If this error happens, then you must do separate analyses", "\n"); wWw <- write_out(p.o);
      p.o <- paste("            excluding the (long, roughly 100+ record) time windows above.", "\n"); wWw <- write_out(p.o);
    }

    ##details<<
    ## Remove non-cycle times from head of TDL file.
    #TDL.keep.ind <- (TDL$data[, 6] != 0);   # Where TDL$StartSeqFlag==0 or TDL$SeqActiveFlag!=0, removed 2/12/2012 7:44PM
    ##details<<
    ## Keep all points (including where TDL program was not running in middle).
    TDL.keep.ind <- !TDL.notrun.ind;  # keep all points (including where TDL program was not running in middle) 2/12/2012 7:49PM

    TDL$n <- sum(TDL.keep.ind); # number of observations
    TDL$data  <- TDL$data[TDL.keep.ind,];

    TDL$ind             <- 1:TDL$n;  # moved from interp_TDL_tanks_ref.R 8/18/2011 10:50AM
    # time
    TDL.TIMESTAMP       <- TDL$data[, 1];
      TDL$time          <- strptime(paste(TDL.TIMESTAMP), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
      TDL$time          <- TDL$time + 0; # convert to times
    ##details<<
    ## Keep all the time values available for Licor/TDL time merge.
    TDL$time_org <- TDL$time; # keep all the time values available for Licor/TDL time merge
  } else {
    p.o <- paste("            Not using TDL file", "\n"); wWw <- write_out(p.o);
    TDL$data <- as.list(new.env());

    TDL$data$TIMESTAMP       <- NA  ;
    TDL$data$RECORD          <- NA  ;
    TDL$data$PrevSite        <- NA  ;
    TDL$data$SiteOutput      <- NA  ;
    TDL$data$StartSeqFlag    <- NA  ;
    TDL$data$SeqActiveFlag   <- NA  ;
    TDL$data$SiteCount       <- NA  ;
    TDL$data$ConcA           <- NA  ;
    TDL$data$ConcB           <- NA  ;
    TDL$data$ConcC           <- NA  ;
    TDL$data$TGAStatus       <- NA  ;
    TDL$data$TGApressure     <- NA  ;
    TDL$data$MassFlow1       <- NA  ;
    TDL$data$Pressure1       <- NA  ;
    TDL$data$MassFlow2       <- NA  ;
    TDL$data$Pressure2       <- NA  ;
    TDL$data$PressureProMan  <- NA  ;

    TDL$n    <- NULL;
    TDL$time <- NULL;
    TDL$ind  <- NULL;
  };

  return( TDL );
  ### TDL
}

