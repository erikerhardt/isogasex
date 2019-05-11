extract.cycle.timing <-
function# Cycle timing
###
(TDL
###
, TDL.cycle
###
, TDL.site.timing.filename
###
)
{
  ##details<<
  ##
  i.time <- 1; old.time <- i.time;
  TDL.site.timing <- paste("date","time.begin","time.seconds","time.minutes","site","name", sep=",");
  for (i.time in 2:TDL$n) {
    if ((TDL$data[i.time-1,"PrevSite"] != TDL$data[i.time,"PrevSite"]) || (i.time == TDL$n)) {
      TDL.site.timing <-
        rbind( TDL.site.timing
          ,paste(
           # Changed to format() when adding full TDL interp 12/12/2009 2:03PM
           # strftime(TDL$time[old.time],format="%Y-%m-%d")
           #,strftime(TDL$time[old.time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
            format(TDL$time[old.time],format="%Y-%m-%d")
           ,format(TDL$time[old.time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
           ,round(as.numeric(difftime(TDL$time[i.time],TDL$time[old.time],units="secs")),1)
           ,round(as.numeric(difftime(TDL$time[i.time],TDL$time[old.time],units="mins")),2)
           ,TDL$data[old.time,"PrevSite"]
           ,TDL.cycle$table.name[(TDL.cycle$table[,1] == TDL$data[old.time,"PrevSite"])]
           , sep=","
          )
        )
      old.time <- i.time;
    };
  };
  write(TDL.site.timing, file = TDL.site.timing.filename, append = FALSE);

  return( NULL );
  ### NULL
}

