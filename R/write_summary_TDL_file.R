#' write output section: TDL file
#'
#' Write TDL summary values for all time points.
#'
#' @param val xxxPARAMxxx
#' @param TDL_cycle xxxPARAMxxx
#' @param output_summary_TDL_fn xxxPARAMxxx
#'
#' @return val$write xxxRETURNxxx
#'
write_summary_TDL_file <-
function# write_output section: TDL file
### Write TDL summary values for all time points.
(val
###
, TDL_cycle
###
, output_summary_TDL_fn
###
)
{

  val$write$summary_TDL <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site_name"
    ,"first_ind"
    ,"ind"
    ,"n_sam"
    ,"conc12CO2"
    ,"conc13CO2"
    ,"TGApressure"
    ,"MassFlow1"
    ,"Pressure1"
    ,"MassFlow2"
    ,"Pressure2"
    ,"PressureProMan"
    ,"interp_tank_hi_12"
    ,"interp_tank_hi_13"
    ,"interp_tank_low_12"
    ,"interp_tank_low_13"
    ,"interp_reference_12"
    ,"interp_reference_13"
    ,"chamber_12"
    ,"chamber_13"
    , sep=",");

  for (i_time in 1:val$sum$TDL$n) {
    val$write$summary_TDL <-
      rbind( val$write$summary_TDL
        ,paste(
           format(val$sum$TDL$time[i_time],format="%Y-%m-%d")
          ,format(val$sum$TDL$time[i_time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$sum$TDL$site[i_time]
          ,TDL_cycle$table_name[(TDL_cycle$table[,1] == val$sum$TDL$site[i_time])]
          ,val$sum$TDL$first_ind[i_time]
          ,val$sum$TDL$ind[i_time]
          ,val$sum$TDL$n_sam[i_time]
          ,val$sum$TDL$conc12CO2[i_time]
          ,val$sum$TDL$conc13CO2[i_time]
          ,val$sum$TDL$TGApressure[i_time]
          ,val$sum$TDL$MassFlow1[i_time]
          ,val$sum$TDL$Pressure1[i_time]
          ,val$sum$TDL$MassFlow2[i_time]
          ,val$sum$TDL$Pressure2[i_time]
          ,val$sum$TDL$PressureProMan[i_time]
          ,val$sum$TDL$interp_tank_hi_12[i_time]
          ,val$sum$TDL$interp_tank_hi_13[i_time]
          ,val$sum$TDL$interp_tank_low_12[i_time]
          ,val$sum$TDL$interp_tank_low_13[i_time]
          ,val$sum$TDL$interp_reference_12[i_time]
          ,val$sum$TDL$interp_reference_13[i_time]
          ,val$sum$TDL$chamber_12[i_time]
          ,val$sum$TDL$chamber_13[i_time]
          , sep=",")
      )
  }

  write(val$write$summary_TDL, file=output_summary_TDL_fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

