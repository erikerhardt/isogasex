#' Cycle timing
#'
#' Write cycle timing to the timing file.
#'
#' @param TDL
#' @param TDL_cycle
#' @param TDL_site_timing_filename
#'
#' @return NULL
#'
#' @examples
extract_cycle_timing <-
function# Cycle timing
###
(TDL
###
, TDL_cycle
###
, TDL_site_timing_filename
###
)
{
  ##details<<
  ##
  i_time <- 1; old_time <- i_time;
  TDL_site_timing <- paste("date","time.begin","time.seconds","time.minutes","site","name", sep=",");
  for (i_time in 2:TDL$n) {
    if ((TDL$data[i_time-1,"PrevSite"] != TDL$data[i_time,"PrevSite"]) || (i_time == TDL$n)) {
      TDL_site_timing <-
        rbind( TDL_site_timing
          ,paste(
           # Changed to format() when adding full TDL interp 12/12/2009 2:03PM
           # strftime(TDL$time[old_time],format="%Y-%m-%d")
            format(TDL$time[old_time],format="%Y-%m-%d")
           ,format(TDL$time[old_time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
           ,round(as.numeric(difftime(TDL$time[i_time],TDL$time[old_time],units="secs")),1)
           ,round(as.numeric(difftime(TDL$time[i_time],TDL$time[old_time],units="mins")),2)
           ,TDL$data[old_time,"PrevSite"]
           ,TDL_cycle$table_name[(TDL_cycle$table[,1] == TDL$data[old_time,"PrevSite"])]
           , sep=","
          )
        )
      old_time <- i_time;
    };
  };
  write(TDL_site_timing, file = TDL_site_timing_filename, append = FALSE);

  invisible(NULL);
  ### NULL
}

