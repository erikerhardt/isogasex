read_Licor <-
function# read_Licor file
###
(Licor.fn
###
, Licor.TDL.time.offset.seconds
###
, sw
###
)
{
  ##details<<
  ## See \code{\link{val_TDL_Licor_variables}} for expected column names.

  ##details<<
  ## Create \code{Licor} to hold data.
  Licor <- as.list(new.env());  # create a list to return with data

  if (sw$use.Licor ) {

    ####################
    ##details<<
    ## Read Licor file
    p.o <- paste("               Reading Licor file: ", Licor.fn, "\n"); wWw <- write_out(p.o);
    ##details<<
    ## Find the row where the column headers and data begin.
    Licor.head.nrows = 30;  # more rows than we need to check
    Licor.head <- read.delim(Licor.fn, header=FALSE, sep="\n", nrows=Licor.head.nrows);
    ##details<<
    ## Look for "Obs" as the first row (used to look for $STARTOFDATA$, but not in every version of Licor file).
    #Licor.header.skip = seq(1,Licor.head.nrows)[(Licor.head == "$STARTOFDATA$")];
    for (i.nrows in 1:Licor.head.nrows){
      if (substr(Licor.head[i.nrows,],1,3) == "Obs"){
        Licor.header.skip = i.nrows - 1;
      }
    }
    # 11/22/2010 7:40PM changed
    Licor$data <- read.delim(Licor.fn, header=TRUE, sep="", skip=Licor.header.skip);     # any white space is delim
    ##details<<
    ## If any extra lines in Licor file, remove those lines and fix the Obs and HHMMSS columns.
    fix.factor <- is.factor(Licor$data[,1]);
      if(fix.factor){
        p.o <- paste("            Note: Some junk lines in Licor file, removing those lines (may see NA warning)", "\n"); wWw <- write_out(p.o);
        Licor$data2 <- read.delim(Licor.fn, header=TRUE, sep="", as.is=TRUE, skip=Licor.header.skip);     # any white space is delim, as.is does not convert to factors
        na.data <- is.na(as.numeric(Licor$data2[,1]));
        Licor$data <- Licor$data[!na.data,]; # remove any lines that don't begin with a number -- such as lines: "Const=" -52 "Oxygen%" 2.0
        Licor$data[,"Obs"   ] <- as.numeric(Licor$data2[!na.data,"Obs"   ]); # fix the affected columns
        #Licor$data[,"HHMMSS"] <-            Licor$data2[!na.data,"HHMMSS"] ;
        Licor$data[,"FTime" ] <- as.numeric(Licor$data2[!na.data,"FTime" ]);
        Licor$data2 <- NULL; # remove after fixing
      }

    #Licor$data <- read.delim(Licor.fn, header=TRUE, sep="\t", skip=Licor.header.skip);  # only tabs is delim
    Licor$n <- dim(Licor$data)[1]; # number of observations

    # 7/15/2010 no longer adding column names -- processing a core set of variables and ignoring the rest
    ## check whether need to add columns
    #if ("VpdA" %in% colnames(Licor$data)) {
    #  Licor$sw.additional.col <- 1; # do nothing, the additional columns are in this file
    #} else {
    #  Licor$sw.additional.col <- 0;
    #      p.o <- paste("Licor file - Note: adding NA columns for VpdA .. xTemp2 columns not in this file\n"); wWw <- write_progress(p.o, time.start);
    #  temp.add <- matrix(NA, nrow=Licor$n, ncol=11);
    #  colnames(temp.add) <- c("VpdA","Ci.Ca","Ci_Pa","uc_20_mV","uc_21_mV","X.U.S.","Trans","CndCO2","Ref_mV","xTemp1","xTemp2");
    #  Licor$data <- cbind(Licor$data, temp.add);
    #}

    ##details<<
    ## Date (day) of run, fix Thursday (Thr to Thu) representation.
    Licor.date.temp   <- scan(Licor.fn, what="character", skip=1, nlines=1);
      Licor.date.temp <- sub("Thr", "Thu", Licor.date.temp);  # Thursday has an alternate representation in Licor than R
    Licor.date.start  <- strptime(Licor.date.temp, "%a %b %d %Y %H:%M:%S"); #, tz=Sys.timezone());
    Licor.date        <- format(Licor.date.start, "%Y-%m-%d")

    # variables for each column
    Licor.HHMMSS      <- Licor$data[, "HHMMSS"];
    Licor.FTime       <- Licor$data[, "FTime"];

      #Licor.time.no.correction  <- strptime(paste(Licor.date, Licor.HHMMSS), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
      Licor.time.start          <- strptime(paste(Licor.date, Licor.HHMMSS[1]), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
      Licor.time.first.decimal  <- Licor.FTime[1];
      Licor.time.corrected      <- Licor.time.start + as.numeric(Licor.FTime) - as.numeric(Licor.time.first.decimal) + Licor.TDL.time.offset.seconds;
      #seconds.diff.Licor = as.numeric(difftime(Licor.time.corrected,Licor.time.no.correction, tz=Sys.timezone(), units="secs"));
      #seconds.diff.Licor = as.numeric(difftime(Licor.time.corrected[1:(Licor.n-1)],Licor.time.corrected[2:Licor.n], tz=Sys.timezone(), units="secs"));
         # > table(-seconds.diff.Licor)
         #   9.5   10 10.5   11
         #     1   36  136    2

      Licor$time <- Licor.time.corrected;
  } else {
    p.o <- paste("                Not using Licor file", "\n"); wWw <- write_out(p.o);
    Licor$data <- as.list(new.env());

    Licor$n    <- NA;
    Licor$time <- NA;

  };

  return( Licor );
  ### Licor
}

