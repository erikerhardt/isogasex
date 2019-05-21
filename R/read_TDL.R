#' read TDL file
#'
#' See \code{\link{val_TDL_Licor_variables}} for expected column names.
#'
#' Create \code{TDL} to hold data.
#'
#' Read TDL file
#'
#' Report times where cycle times when TDL program was not running 2/9/2012 1:18AM.
#'
#' Remove non-cycle times from head of TDL file.
#'
#' Keep all points (including where TDL program was not running in middle).
#'
#' Keep all the time values available for Licor/TDL time merge.
#'
#' @param TDL_fn
#' @param sw
#'
#' @return TDL
#'
#' @examples
read_TDL <-
function# read_TDL file
###
(TDL_fn
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

  if (sw$use_TDL  ) {
    ####################
    ##details<<
    ## Read TDL file
    p_o <- paste("               Reading TDL   file: ", TDL_fn, "\n"); wWw <- write_out(p_o);
    TDL$program_name <- read.csv(TDL_fn, header=FALSE, nrows=1)[6]; # program name from header
    TDL$header_names <- read.csv(TDL_fn, header=FALSE, nrows=1, skip=1); # column headers names
    TDL$data <- read.csv(TDL_fn, header=FALSE, skip=4, na.strings=c("\"NaN\"","NaN","\"NAN\"","NAN"));
    colnames(TDL$data) <- t(TDL$header_names); # assign column header names

       # 10 Hz data from TDL: TOA5_2257.RawData_2009_08_18_0956.dat
       #    Columns
       #       3,4:  unique identifiers for each site.
       #       5:    0=functioning, -1=not functioning
       #       6:    1 minus [col5]


    ##details<<
    ## Report times where cycle times when TDL program was not running 2/9/2012 1:18AM.
    TDL_notrun_ind <- (TDL$data[, 6] == 0);   # Where TDL$StartSeqFlag!=0 or TDL$SeqActiveFlag==0
    #if (length(TDL_notrun_ind)) { # if there are non functioning cycle times
    # CHANGED from "length" to "sum" 3/1/2012 11:45AM
    if (sum(TDL_notrun_ind)) { # if there are non functioning cycle times
      TDL_notrun_ind_num <- (1:length(TDL$data[,6]))[TDL_notrun_ind]; # row numbers
      TDL_notrun_ind_num_diff <- diff(TDL_notrun_ind_num); # difference between numbers to find blocks edges
      TDL_notrun_ind_num_diff_nonone <- (1:length(TDL_notrun_ind_num_diff))[(TDL_notrun_ind_num_diff != 1)]; # where do new blocks begin
      TDL_remove_blocks_n <- length(TDL_notrun_ind_num_diff_nonone)+1;

      # first index, all other starting indices, and last index+1
      TDL_remove_first_to_last_subind <- c(1, TDL_notrun_ind_num_diff_nonone+1, length(TDL_notrun_ind_num)+1);

      p_o <- paste("WARNING: There are ", TDL_remove_blocks_n, " block(s) of time in TDL file where SeqActiveFlag==0", "\n"); wWw <- write_out(p_o);
      p_o <- paste("            These blocks of time will NOT BE USED only if at beginning or end of file:", "\n"); wWw <- write_out(p_o);
      for (i in 1:TDL_remove_blocks_n) {
        ind_first <- TDL_notrun_ind_num[TDL_remove_first_to_last_subind[i]];
        ind_last  <- TDL_notrun_ind_num[TDL_remove_first_to_last_subind[i+1]-1];
        p_o <- paste("            ", (ind_last-ind_first+1),
                  " records: ", (1:length(TDL$data[,6]))[ind_first], " to ", (1:length(TDL$data[,6]))[ind_last],
                  " times: ", TDL$data$TIMESTAMP[ind_first], " to ", TDL$data$TIMESTAMP[ind_last], "\n"); wWw <- write_out(p_o);

        if (!((ind_first==1) | (ind_last==length(TDL$data[,6])))) { # if not the beginning or end of file, include those points
          TDL_notrun_ind[ind_first:ind_last] <- FALSE; # keep points all points (including where TDL program was not running in middle) 2/12/2012 7:49PM
          p_o <- paste("            NOTE: keeping these points as part of analysis", "\n"); wWw <- write_out(p_o);
        }

      }
      p_o <- paste("            This MAY CAUSE AN ERROR during \"interp Licor values to TDL timepoints\" below", "\n"); wWw <- write_out(p_o);
      p_o <- paste("            If this error happens, then you must do separate analyses", "\n"); wWw <- write_out(p_o);
      p_o <- paste("            excluding the (long, roughly 100+ record) time windows above.", "\n"); wWw <- write_out(p_o);
    }

    ##details<<
    ## Remove non-cycle times from head of TDL file.
    #TDL_keep_ind <- (TDL$data[, 6] != 0);   # Where TDL$StartSeqFlag==0 or TDL$SeqActiveFlag!=0, removed 2/12/2012 7:44PM
    ##details<<
    ## Keep all points (including where TDL program was not running in middle).
    TDL_keep_ind <- !TDL_notrun_ind;  # keep all points (including where TDL program was not running in middle) 2/12/2012 7:49PM

    TDL$n <- sum(TDL_keep_ind); # number of observations
    TDL$data  <- TDL$data[TDL_keep_ind,];

    TDL$ind             <- 1:TDL$n;  # moved from interp_TDL_tanks_ref.R 8/18/2011 10:50AM
    # time
    TDL_TIMESTAMP       <- TDL$data[, 1];
      TDL$time          <- strptime(paste(TDL_TIMESTAMP), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
      TDL$time          <- TDL$time + 0; # convert to times
    ##details<<
    ## Keep all the time values available for Licor/TDL time merge.
    TDL$time_org <- TDL$time; # keep all the time values available for Licor/TDL time merge
  } else {
    p_o <- paste("            Not using TDL file", "\n"); wWw <- write_out(p_o);
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

