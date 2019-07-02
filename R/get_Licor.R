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
#' @importFrom dplyr select rename
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

  # If Licor 6400
  if (dat_Licor$model == 6400) {
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
  }

  # If Licor 6800, convert required columns to 6400 format for compatability
  # See vignettes/isogasex_user_guide.Rmd for column matching
  if (dat_Licor$model == 6800) {

    Licor$data <-
      dat_Licor$data %>%
      dplyr::select(
        obs
      , hhmmss
      , elapsed
      , A
      , E
      , Tair
      , Tleaf
      , Tirga
      , CO2_r
      , CO2_s
      , H2O_r
      , H2O_s
      , RHcham
      , Flow
      , Qin
      , Qamb_out
      , Pa
      , gtc
      , Ci
      , VPDleaf
      , S
      , gbw
      ) %>%
      dplyr::rename(
        Obs     = obs
      , HHMMSS  = hhmmss
      , FTime   = elapsed
      , Photo   = A
      , Trmmol  = E
      , Tair    = Tair
      , Tleaf   = Tleaf
      , TBlk    = Tirga
      , CO2R    = CO2_r
      , CO2S    = CO2_s
      , H2OR    = H2O_r
      , H2OS    = H2O_s
      , RH_S    = RHcham
      , Flow    = Flow
      , PARi    = Qin
      , PARo    = Qamb_out
      , Press   = Pa
      , Cond    = gtc
      , Ci      = Ci
      , VpdL    = VPDleaf
      , Area    = S
      , BLCond  = gbw
      ) %>%
      dplyr::mutate(
        Trmmol = Trmmol * 1000 # mol to mmol
      ) %>%
      as.data.frame() # change to data.frame (tibbles work differently and break the code)

  }


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

