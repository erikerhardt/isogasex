#' get Licor file and format into usable 6400 format
#'
#' See \code{\link{val_TDL_Licor_variables}} for expected column names.
#'
#' Create \code{Licor} to hold data.
#'
#' Read Licor file
#'
#' Find the row where the column headers and data begin.
#'
#' Look for "Obs" as the first row (used to look for $STARTOFDATA$, but not in every version of Licor file).
#'
#' If any extra lines in Licor file, remove those lines and fix the Obs and HHMMSS columns.
#'
#' Date (day) of run, fix Thursday (Thr to Thu) representation.
#'
#' @param Licor_fn Licor file to read
#' @param Licor_TDL_time_offset_seconds seconds to adjust to match up with TDL
#' @param sw isogasex switch settings
#'
#' @return Licor xxxRETURNxxx
#' @importFrom utils read.delim
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom RLicor read_Licor
#'
get_Licor <-
function# read_Licor file
###
(Licor_fn
###
, Licor_TDL_time_offset_seconds
###
, sw
###
)
{
  ## See \code{\link{val_TDL_Licor_variables}} for expected column names.

  ## Create \code{Licor} to hold data.
  Licor <- list();  # create a list to return with data

  if (!sw$use_Licor) {
    p_o <- paste("                Not using Licor file", "\n"); write_out(p_o);
    Licor$data <- list();

    Licor$n    <- NA;
    Licor$time <- NA;

    return( Licor );
  }

  # Read licor data from 6400 or 6800
  dat_Licor <- RLicor::read_Licor(Licor_fn)

  # If Licor 6800, convert required columns to 6400 format for compatability
  if (dat_Licor$model == 6800) {





  }


  # Remove remark rows and change to data.frame (tibbles work differently and break the code)
  Licor$data <-
    dat_Licor$data %>%
    dplyr::select(
      -remark
    ) %>%
    dplyr::filter(
      !is.na(Obs)
    ) %>%
    as.data.frame()


  # ## If any extra lines in Licor file, remove those lines and fix the Obs and HHMMSS columns.
  # fix_factor <- is.factor(Licor$data[,1]);
  #   if(fix_factor){
  #     p_o <- paste("            Note: Some junk lines in Licor file, removing those lines (may see NA warning)", "\n"); write_out(p_o);
  #     Licor$data2 <- utils::read.delim(Licor_fn, header=TRUE, sep="", as.is=TRUE, skip=Licor_header_skip);     # any white space is delim, as.is does not convert to factors
  #     na_data <- is.na(as.numeric(Licor$data2[,1]));
  #     Licor$data <- Licor$data[!na_data,]; # remove any lines that don't begin with a number -- such as lines: "Const=" -52 "Oxygen%" 2.0
  #     Licor$data[,"Obs"   ] <- as.numeric(Licor$data2[!na_data,"Obs"   ]); # fix the affected columns
  #     #Licor$data[,"HHMMSS"] <-            Licor$data2[!na_data,"HHMMSS"] ;
  #     Licor$data[,"FTime" ] <- as.numeric(Licor$data2[!na_data,"FTime" ]);
  #     Licor$data2 <- NULL; # remove after fixing
  #   }



  #Licor$data <- read.delim(Licor_fn, header=TRUE, sep="\t", skip=Licor_header_skip);  # only tabs is delim
  Licor$n <- nrow(Licor$data) # number of observations

  ## Date (day) of run, fix Thursday (Thr to Thu) representation.
  #Licor_date_temp   <- scan(Licor_fn, what="character", skip=1, nlines=1);
  #  Licor_date_temp <- sub("Thr", "Thu", Licor_date_temp);  # Thursday has an alternate representation in Licor than R
  #Licor_date_start  <- strptime(Licor_date_temp, "%a %b %d %Y %H:%M:%S"); #, tz=Sys.timezone());
  #Licor_date        <- format(Licor_date_start, "%Y-%m-%d")

  Licor_date_start  <- dat_Licor$datetime
  Licor_date        <- as.Date(dat_Licor$datetime)


  # variables for each column
  Licor_HHMMSS      <- Licor$data[, "HHMMSS"];
  Licor_FTime       <- Licor$data[, "FTime"];

    #Licor_time_no_correction  <- strptime(paste(Licor_date, Licor_HHMMSS), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
    Licor_time_start          <- strptime(paste(Licor_date, Licor_HHMMSS[1]), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
    Licor_time_first_decimal  <- Licor_FTime[1];
    Licor_time_corrected      <- Licor_time_start + as.numeric(Licor_FTime) - as.numeric(Licor_time_first_decimal) + Licor_TDL_time_offset_seconds;
    #seconds_diff_Licor = as.numeric(difftime(Licor_time_corrected,Licor_time_no_correction, tz=Sys.timezone(), units="secs"));
    #seconds_diff_Licor = as.numeric(difftime(Licor_time_corrected[1:(Licor_n-1)],Licor_time_corrected[2:Licor_n], tz=Sys.timezone(), units="secs"));
       # > table(-seconds_diff_Licor)
       #   9.5   10 10.5   11
       #     1   36  136    2

    Licor$time <- Licor_time_corrected;

  return( Licor );
}

