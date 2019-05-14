write_CI_TDL_file <-
function# write_output section: TDL file
### Write observed TDL values with BS CIs for all time points.
(val
###
, TDL.cycle
###
, output.CI.TDL.fn
###
)
{

  val$write$CI_TDL <- paste(
     "date"
    ,"time"
    ,"site"
    ,"site.name"
    ,"first.ind"
    ,"ind"
    ,"n.sam"
    ,"conc12CO2.CI.L"           ,"conc12CO2.CI.U"              #,"conc12CO2"
    ,"conc13CO2.CI.L"           ,"conc13CO2.CI.U"              #,"conc13CO2"
    ,"TGApressure.CI.L"         ,"TGApressure.CI.U"            #,"TGApressure"
    ,"MassFlow1.CI.L"           ,"MassFlow1.CI.U"              #,"MassFlow1"
    ,"Pressure1.CI.L"           ,"Pressure1.CI.U"              #,"Pressure1"
    ,"MassFlow2.CI.L"           ,"MassFlow2.CI.U"              #,"MassFlow2"
    ,"Pressure2.CI.L"           ,"Pressure2.CI.U"              #,"Pressure2"
    ,"PressureProMan.CI.L"      ,"PressureProMan.CI.U"         #,"PressureProMan"
    ,"interp.tank.hi.12.CI.L"   ,"interp.tank.hi.12.CI.U"      #,"interp.tank.hi.12"
    ,"interp.tank.hi.13.CI.L"   ,"interp.tank.hi.13.CI.U"      #,"interp.tank.hi.13"
    ,"interp.tank.low.12.CI.L"  ,"interp.tank.low.12.CI.U"     #,"interp.tank.low.12"
    ,"interp.tank.low.13.CI.L"  ,"interp.tank.low.13.CI.U"     #,"interp.tank.low.13"
    ,"interp.reference.12.CI.L" ,"interp.reference.12.CI.U"    #,"interp.reference.12"
    ,"interp.reference.13.CI.L" ,"interp.reference.13.CI.U"    #,"interp.reference.13"
    ,"chamber.12.CI.L"          ,"chamber.12.CI.U"             #,"chamber.12"
    ,"chamber.13.CI.L"          ,"chamber.13.CI.U"             #,"chamber.13"
    , sep=",");

  for (i.time in 1:val$sum$TDL$n) {
    val$write$CI_TDL <-
      rbind( val$write$CI_TDL
        ,paste(
           format(val$sum$TDL$time[i.time],format="%Y-%m-%d")
          ,format(val$sum$TDL$time[i.time],format="%H:%M:%S")  # "%H:%M:%OS" ## can't use %OS format since Excel doesn't display decimal seconds automatically
          ,val$sum$TDL$site[i.time]
          ,TDL.cycle$table.name[(TDL.cycle$table[,1] == val$sum$TDL$site[i.time])]
          ,val$sum$TDL$first.ind[i.time]
          ,val$sum$TDL$ind[i.time]
          ,val$sum$TDL$n.sam[i.time]
          ,val$CI$TDL$conc12CO2[i.time,1]           ,val$CI$TDL$conc12CO2[i.time,2]                   #,val$sum$TDL$conc12CO2[i.time]
          ,val$CI$TDL$conc13CO2[i.time,1]           ,val$CI$TDL$conc13CO2[i.time,2]                   #,val$sum$TDL$conc13CO2[i.time]
          ,val$CI$TDL$TGApressure[i.time,1]         ,val$CI$TDL$TGApressure[i.time,2]                 #,val$sum$TDL$TGApressure[i.time]
          ,val$CI$TDL$MassFlow1[i.time,1]           ,val$CI$TDL$MassFlow1[i.time,2]                   #,val$sum$TDL$MassFlow1[i.time]
          ,val$CI$TDL$Pressure1[i.time,1]           ,val$CI$TDL$Pressure1[i.time,2]                   #,val$sum$TDL$Pressure1[i.time]
          ,val$CI$TDL$MassFlow2[i.time,1]           ,val$CI$TDL$MassFlow2[i.time,2]                   #,val$sum$TDL$MassFlow2[i.time]
          ,val$CI$TDL$Pressure2[i.time,1]           ,val$CI$TDL$Pressure2[i.time,2]                   #,val$sum$TDL$Pressure2[i.time]
          ,val$CI$TDL$PressureProMan[i.time,1]      ,val$CI$TDL$PressureProMan[i.time,2]              #,val$sum$TDL$PressureProMan[i.time]
          ,val$CI$TDL$interp.tank.hi.12[i.time,1]   ,val$CI$TDL$interp.tank.hi.12[i.time,2]           #,val$sum$TDL$interp.tank.hi.12[i.time]
          ,val$CI$TDL$interp.tank.hi.13[i.time,1]   ,val$CI$TDL$interp.tank.hi.13[i.time,2]           #,val$sum$TDL$interp.tank.hi.13[i.time]
          ,val$CI$TDL$interp.tank.low.12[i.time,1]  ,val$CI$TDL$interp.tank.low.12[i.time,2]          #,val$sum$TDL$interp.tank.low.12[i.time]
          ,val$CI$TDL$interp.tank.low.13[i.time,1]  ,val$CI$TDL$interp.tank.low.13[i.time,2]          #,val$sum$TDL$interp.tank.low.13[i.time]
          ,val$CI$TDL$interp.reference.12[i.time,1] ,val$CI$TDL$interp.reference.12[i.time,2]         #,val$sum$TDL$interp.reference.12[i.time]
          ,val$CI$TDL$interp.reference.13[i.time,1] ,val$CI$TDL$interp.reference.13[i.time,2]         #,val$sum$TDL$interp.reference.13[i.time]
          ,val$CI$TDL$chamber.12[i.time,1]          ,val$CI$TDL$chamber.12[i.time,2]                  #,val$sum$TDL$chamber.12[i.time]
          ,val$CI$TDL$chamber.13[i.time,1]          ,val$CI$TDL$chamber.13[i.time,2]                  #,val$sum$TDL$chamber.13[i.time]
          , sep=",")
      )
  }

  write(val$write$CI_TDL, file=output.CI.TDL.fn, append=FALSE);#, colnames=FALSE, row.names=FALSE)

  return( val$write );
  ### val$write
}

