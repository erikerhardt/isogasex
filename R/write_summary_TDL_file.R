write_summary_TDL_file <-
function# write_output section: TDL file
### Write TDL summary values for all time points.
(val
###
, TDL.cycle
###
, output.summary.TDL.fn
###
)
{

  val$write$summary_TDL <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site.name"
    ,"first.ind"
    ,"ind"
    ,"n.sam"
    ,"conc12CO2"
    ,"conc13CO2"
    ,"TGApressure"
    ,"MassFlow1"
    ,"Pressure1"
    ,"MassFlow2"
    ,"Pressure2"
    ,"PressureProMan"
    ,"interp.tank.hi.12"
    ,"interp.tank.hi.13"
    ,"interp.tank.low.12"
    ,"interp.tank.low.13"
    ,"interp.reference.12"
    ,"interp.reference.13"
    ,"chamber.12"
    ,"chamber.13"
    , sep=",");

  for (i.time in 1:val$sum$TDL$n) {
    val$write$summary_TDL <-
      rbind( val$write$summary_TDL
        ,paste(
           format(val$sum$TDL$time[i.time],format="%Y-%m-%d")
          ,format(val$sum$TDL$time[i.time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$sum$TDL$site[i.time]
          ,TDL.cycle$table.name[(TDL.cycle$table[,1] == val$sum$TDL$site[i.time])]
          ,val$sum$TDL$first.ind[i.time]
          ,val$sum$TDL$ind[i.time]
          ,val$sum$TDL$n.sam[i.time]
          ,val$sum$TDL$conc12CO2[i.time]
          ,val$sum$TDL$conc13CO2[i.time]
          ,val$sum$TDL$TGApressure[i.time]
          ,val$sum$TDL$MassFlow1[i.time]
          ,val$sum$TDL$Pressure1[i.time]
          ,val$sum$TDL$MassFlow2[i.time]
          ,val$sum$TDL$Pressure2[i.time]
          ,val$sum$TDL$PressureProMan[i.time]
          ,val$sum$TDL$interp.tank.hi.12[i.time]
          ,val$sum$TDL$interp.tank.hi.13[i.time]
          ,val$sum$TDL$interp.tank.low.12[i.time]
          ,val$sum$TDL$interp.tank.low.13[i.time]
          ,val$sum$TDL$interp.reference.12[i.time]
          ,val$sum$TDL$interp.reference.13[i.time]
          ,val$sum$TDL$chamber.12[i.time]
          ,val$sum$TDL$chamber.13[i.time]
          , sep=",")
      )
  }

  write(val$write$summary_TDL, file=output.summary.TDL.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

