#' Assign TDL and Licor values to val variable names, raw and summarized values
#'
#' Function to assign TDL and Licor values to variable names,
#' summaries and some smart way for individual observations (later for NPBS).
#' If the variable has values, use those, if not then populate with NAs.
#'
#' These are the columns expected from the Licor and TDL.
#' Any columns with headers not listed here will not be used.
#'
#' EXPECTED column names for Licor:
#' \preformatted{
#' (in tdllicor)     (column from Licor)
#' Obs               Obs
#' HHMMSS            HHMMSS
#' FTime             FTime
#' A                 Photo
#' E                 Trmmol
#' temp_air          Tair
#' temp_leaf         Tleaf
#' temp_block        TBlk
#' Ce                CO2R
#' Co                CO2S
#' xin               H2OR
#' xout              H2OS
#' rh_ref            RH_R
#' rh_sam            RH_S
#' uin               Flow
#' par_int           PARi
#' par_ext           PARo
#' Atm_press         Press
#' CsMch             CsMch
#' HsMch             HsMch
#' StableF           StableF
#' Status            Status
#' gsc               Cond
#' Ci                Ci
#' VPD               VpdL
#' La                Area
#' StmRat            StmRat
#' gbw               BLCond
#' VpdA              VpdA
#' Ci_Ca             Ci_Ca
#' pi                Ci_Pa
#' uc_20_mV          uc_20_mV
#' uc_21_mV          uc_21_mV
#' U_S               X.U.S.
#' Trans             Trans
#' CndCO2            CndCO2
#' Ref_mV            Ref_mV
#' xTemp1            xTemp1
#' xTemp2            xTemp2
#' TChamAir          TChamAir
#' }
#'
#' EXPECTED column names for TDL:
#' \preformatted{
#' (in tdllicor)       (column from TDL)
#' TIMESTAMP           TIMESTAMP
#' RECORD              RECORD
#' PrevSite            PrevSite
#' SiteOutput          SiteOutput
#' StartSeqFlag        StartSeqFlag
#' SeqActiveFlag       SeqActiveFlag
#' SiteCount           SiteCount
#' conc12CO2           ConcA
#' conc13CO2           ConcB
#' ConcC               ConcC
#' TGAStatus           TGAStatus
#' TGApressure         TGApressure
#' MassFlow1           MassFlow1
#' Pressure1           Pressure1
#' MassFlow2           MassFlow2
#' Pressure2           Pressure2
#' PressureProMan      PressureProMan
#' }
#'
#' Conventions, put everything in \code{val}.
#' Observed values from TDL or Licor  prefix: \code{val$obs$*}
#'
#' Summarized values from TDL or Licor  prefix: \code{val$sum$*}
#'
#' @param TDL xxxPARAMxxx
#' @param Licor xxxPARAMxxx
#' @param TDL_cycle xxxPARAMxxx
#' @param val xxxPARAMxxx
#' @param sw xxxPARAMxxx
#'
#' @return val xxxRETURNxxx
#'
val_TDL_Licor_variables <-
function# Assign TDL and Licor values to val variable names, raw and summarized values
###
(TDL
###
, Licor
###
, TDL_cycle
###
, val
###
, sw
###
)
{
  ###    !!!!!
  ### Note, the variable names used below were from the CalculationsFile.xls
  ###   in cases in the calculations, variables are referred to by their column header name
  ###   Look at the variable names again and decide whether to use var name or col name.

  ##details<<
  ## Function to assign TDL and Licor values to variable names,
  ##   summaries and some smart way for individual observations (later for NPBS).
  ## If the variable has values, use those, if not then populate with NAs.

  ##details<<
  ## These are the columns expected from the Licor and TDL.
  ## Any columns with headers not listed here will not be used.
  ##
  ## EXPECTED column names for Licor:
  ## \preformatted{
  ## (in isogasex)     (column from Licor)
  ## Obs               Obs
  ## HHMMSS            HHMMSS
  ## FTime             FTime
  ## A                 Photo
  ## E                 Trmmol
  ## temp_air          Tair
  ## temp_leaf         Tleaf
  ## temp_block        TBlk
  ## Ce                CO2R
  ## Co                CO2S
  ## xin               H2OR
  ## xout              H2OS
  ## rh_ref            RH_R
  ## rh_sam            RH_S
  ## uin               Flow
  ## par_int           PARi
  ## par_ext           PARo
  ## Atm_press         Press
  ## CsMch             CsMch
  ## HsMch             HsMch
  ## StableF           StableF
  ## Status            Status
  ## gsc               Cond
  ## Ci                Ci
  ## VPD               VpdL
  ## La                Area
  ## StmRat            StmRat
  ## gbw               BLCond
  ## VpdA              VpdA
  ## Ci_Ca             Ci_Ca
  ## pi                Ci_Pa
  ## uc_20_mV          uc_20_mV
  ## uc_21_mV          uc_21_mV
  ## U_S               X.U.S.
  ## Trans             Trans
  ## CndCO2            CndCO2
  ## Ref_mV            Ref_mV
  ## xTemp1            xTemp1
  ## xTemp2            xTemp2
  ## TChamAir          TChamAir
  ## }
  ##
  ## EXPECTED column names for TDL:
  ## \preformatted{
  ## (in isogasex)       (column from TDL)
  ## TIMESTAMP           TIMESTAMP
  ## RECORD              RECORD
  ## PrevSite            PrevSite
  ## SiteOutput          SiteOutput
  ## StartSeqFlag        StartSeqFlag
  ## SeqActiveFlag       SeqActiveFlag
  ## SiteCount           SiteCount
  ## conc12CO2           ConcA
  ## conc13CO2           ConcB
  ## ConcC               ConcC
  ## TGAStatus           TGAStatus
  ## TGApressure         TGApressure
  ## MassFlow1           MassFlow1
  ## Pressure1           Pressure1
  ## MassFlow2           MassFlow2
  ## Pressure2           Pressure2
  ## PressureProMan      PressureProMan
  ## }


  ##details<<
  ## Conventions, put everything in \code{val}.
  ## Observed values from TDL or Licor  prefix: \code{val$obs$*}
  val$obs       <- as.list(new.env());
  val$obs$TDL   <- as.list(new.env());
  val$obs$Licor <- as.list(new.env());
  ##details<<
  ## Summarized values from TDL or Licor  prefix: \code{val$sum$*}
  val$sum       <- as.list(new.env());
  val$sum$TDL   <- as.list(new.env());
  val$sum$Licor <- as.list(new.env());

  #Licor$data:
  if (sw$use_Licor) {
    temp_n_Licor <- length(Licor$data[,1]); # "Obs"
  } else {
    temp_n_Licor <- 0;
  }

    # Variables in calculations
  if (sum(colnames(Licor$data)=="Obs"      )) { val$obs$Licor$Obs               <- Licor$data[,"Obs"      ]; } else {val$obs$Licor$Obs            <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="HHMMSS"   )) { val$obs$Licor$HHMMSS            <- Licor$data[,"HHMMSS"   ]; } else {val$obs$Licor$HHMMSS         <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="FTime"    )) { val$obs$Licor$FTime             <- Licor$data[,"FTime"    ]; } else {val$obs$Licor$FTime          <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Photo"    )) { val$obs$Licor$A                 <- Licor$data[,"Photo"    ]; } else {val$obs$Licor$A              <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Trmmol"   )) { val$obs$Licor$E                 <- Licor$data[,"Trmmol"   ]; } else {val$obs$Licor$E              <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Tair"     )) { val$obs$Licor$temp_air          <- Licor$data[,"Tair"     ]; } else {val$obs$Licor$temp_air       <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Tleaf"    )) { val$obs$Licor$temp_leaf         <- Licor$data[,"Tleaf"    ]; } else {val$obs$Licor$temp_leaf      <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="TBlk"     )) { val$obs$Licor$temp_block        <- Licor$data[,"TBlk"     ]; } else {val$obs$Licor$temp_block     <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="CO2R"     )) { val$obs$Licor$Ce                <- Licor$data[,"CO2R"     ]; } else {val$obs$Licor$Ce             <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="CO2S"     )) { val$obs$Licor$Co                <- Licor$data[,"CO2S"     ]; } else {val$obs$Licor$Co             <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="H2OR"     )) { val$obs$Licor$xin               <- Licor$data[,"H2OR"     ]; } else {val$obs$Licor$xin            <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="H2OS"     )) { val$obs$Licor$xout              <- Licor$data[,"H2OS"     ]; } else {val$obs$Licor$xout           <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="RH_R"     )) { val$obs$Licor$rh_ref            <- Licor$data[,"RH_R"     ]; } else {val$obs$Licor$rh_ref         <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="RH_S"     )) { val$obs$Licor$rh_sam            <- Licor$data[,"RH_S"     ]; } else {val$obs$Licor$rh_sam         <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Flow"     )) { val$obs$Licor$uin               <- Licor$data[,"Flow"     ]; } else {val$obs$Licor$uin            <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="PARi"     )) { val$obs$Licor$par_int           <- Licor$data[,"PARi"     ]; } else {val$obs$Licor$par_int        <- matrix(NA,temp_n_Licor,1)};  # PAR or PPFD  These two are defined in the same way at top of CalculationsFile.xls
  if (sum(colnames(Licor$data)=="PARo"     )) { val$obs$Licor$par_ext           <- Licor$data[,"PARo"     ]; } else {val$obs$Licor$par_ext        <- matrix(NA,temp_n_Licor,1)};  # PAR or PPFD
  if (sum(colnames(Licor$data)=="Press"    )) { val$obs$Licor$Atm_press         <- Licor$data[,"Press"    ]; } else {val$obs$Licor$Atm_press      <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="CsMch"    )) { val$obs$Licor$CsMch             <- Licor$data[,"CsMch"    ]; } else {val$obs$Licor$CsMch          <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="HsMch"    )) { val$obs$Licor$HsMch             <- Licor$data[,"HsMch"    ]; } else {val$obs$Licor$HsMch          <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="StableF"  )) { val$obs$Licor$StableF           <- Licor$data[,"StableF"  ]; } else {val$obs$Licor$StableF        <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Status"   )) { val$obs$Licor$Status            <- Licor$data[,"Status"   ]; } else {val$obs$Licor$Status         <- matrix(NA,temp_n_Licor,1)};
    # Variables NOT in calculations
  if (sum(colnames(Licor$data)=="Cond"     )) { val$obs$Licor$gsc               <- Licor$data[,"Cond"     ]; } else {val$obs$Licor$gsc            <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Ci"       )) { val$obs$Licor$Ci                <- Licor$data[,"Ci"       ]; } else {val$obs$Licor$Ci             <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="VpdL"     )) { val$obs$Licor$VPD               <- Licor$data[,"VpdL"     ]; } else {val$obs$Licor$VPD            <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Area"     )) { val$obs$Licor$La                <- Licor$data[,"Area"     ]; } else {val$obs$Licor$La             <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="StmRat"   )) { val$obs$Licor$StmRat            <- Licor$data[,"StmRat"   ]; } else {val$obs$Licor$StmRat         <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="BLCond"   )) { val$obs$Licor$gbw               <- Licor$data[,"BLCond"   ]; } else {val$obs$Licor$gbw            <- matrix(NA,temp_n_Licor,1)};
  # additional columns
  if (sum(colnames(Licor$data)=="VpdA"     )) { val$obs$Licor$VpdA              <- Licor$data[,"VpdA"     ]; } else {val$obs$Licor$VpdA           <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Ci_Ca"    )) { val$obs$Licor$Ci_Ca             <- Licor$data[,"Ci_Ca"    ]; } else {val$obs$Licor$Ci_Ca          <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Ci_Pa"    )) { val$obs$Licor$pi                <- Licor$data[,"Ci_Pa"    ]; } else {val$obs$Licor$pi             <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="uc_20_mV" )) { val$obs$Licor$uc_20_mV          <- Licor$data[,"uc_20_mV" ]; } else {val$obs$Licor$uc_20_mV       <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="uc_21_mV" )) { val$obs$Licor$uc_21_mV          <- Licor$data[,"uc_21_mV" ]; } else {val$obs$Licor$uc_21_mV       <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="X.U.S."   )) { val$obs$Licor$U_S               <- Licor$data[,"X.U.S."   ]; } else {val$obs$Licor$U_S            <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Trans"    )) { val$obs$Licor$Trans             <- Licor$data[,"Trans"    ]; } else {val$obs$Licor$Trans          <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="CndCO2"   )) { val$obs$Licor$CndCO2            <- Licor$data[,"CndCO2"   ]; } else {val$obs$Licor$CndCO2         <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="Ref_mV"   )) { val$obs$Licor$Ref_mV            <- Licor$data[,"Ref_mV"   ]; } else {val$obs$Licor$Ref_mV         <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="xTemp1"   )) { val$obs$Licor$xTemp1            <- Licor$data[,"xTemp1"   ]; } else {val$obs$Licor$xTemp1         <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="xTemp2"   )) { val$obs$Licor$xTemp2            <- Licor$data[,"xTemp2"   ]; } else {val$obs$Licor$xTemp2         <- matrix(NA,temp_n_Licor,1)};
  if (sum(colnames(Licor$data)=="TChamAir" )) { val$obs$Licor$TChamAir          <- Licor$data[,"TChamAir" ]; } else {val$obs$Licor$TChamAir       <- matrix(NA,temp_n_Licor,1)};

  #TDL$data:
  val$obs$TDL$TIMESTAMP           <- TDL$data  [,"TIMESTAMP"      ];
  val$obs$TDL$RECORD              <- TDL$data  [,"RECORD"         ];
  val$obs$TDL$PrevSite            <- TDL$data  [,"PrevSite"       ];
  val$obs$TDL$SiteOutput          <- TDL$data  [,"SiteOutput"     ];
  val$obs$TDL$StartSeqFlag        <- TDL$data  [,"StartSeqFlag"   ];
  val$obs$TDL$SeqActiveFlag       <- TDL$data  [,"SeqActiveFlag"  ];
  val$obs$TDL$SiteCount           <- TDL$data  [,"SiteCount"      ];
  val$obs$TDL$conc12CO2           <- TDL$data  [,"ConcA"          ];
  val$obs$TDL$conc13CO2           <- TDL$data  [,"ConcB"          ];
  val$obs$TDL$ConcC               <- TDL$data  [,"ConcC"          ];
  val$obs$TDL$TGAStatus           <- TDL$data  [,"TGAStatus"      ];
  val$obs$TDL$TGApressure         <- TDL$data  [,"TGApressure"    ];
  val$obs$TDL$MassFlow1           <- TDL$data  [,"MassFlow1"      ];
  val$obs$TDL$Pressure1           <- TDL$data  [,"Pressure1"      ];
  val$obs$TDL$MassFlow2           <- TDL$data  [,"MassFlow2"      ];
  val$obs$TDL$Pressure2           <- TDL$data  [,"Pressure2"      ];
  val$obs$TDL$PressureProMan      <- TDL$data  [,"PressureProMan" ];
  val$obs$TDL$interp_tank_hi_12   <- TDL$interp[,"tank_hi_12"     ];
  val$obs$TDL$interp_tank_hi_13   <- TDL$interp[,"tank_hi_13"     ];
  val$obs$TDL$interp_tank_low_12  <- TDL$interp[,"tank_low_12"    ];
  val$obs$TDL$interp_tank_low_13  <- TDL$interp[,"tank_low_13"    ];
  val$obs$TDL$interp_reference_12 <- TDL$interp[,"reference_12"   ];
  val$obs$TDL$interp_reference_13 <- TDL$interp[,"reference_13"   ];
  val$obs$TDL$chamber_12          <- val$obs$TDL$conc12CO2; val$obs$TDL$chamber_12[!(val$obs$TDL$PrevSite == TDL_cycle$number_chamber)] <- NA;
  val$obs$TDL$chamber_13          <- val$obs$TDL$conc13CO2; val$obs$TDL$chamber_13[!(val$obs$TDL$PrevSite == TDL_cycle$number_chamber)] <- NA;

  if (sw$use_Licor==0) {
    Licor$summary$n <- 0;
  }

  #Licor$summary:
  #val$sum$Licor$Obs               <- Licor$summary[,"Obs"];
  #val$sum$Licor$HHMMSS            <- Licor$summary[,"HHMMSS"];
  val$sum$Licor$n                 <- Licor$summary$n;
  val$sum$Licor$first_ind         <- Licor$summary$first_ind;
  val$sum$Licor$ind               <- Licor$summary$ind;
  val$sum$Licor$site              <- Licor$summary$site;
  val$sum$Licor$n_sam             <- Licor$summary$n_sam;
  val$sum$Licor$time              <- Licor$summary$time;

    # Variables in calculations
  if (sum(colnames(Licor$summary$mean)=="FTime"    )) { val$sum$Licor$FTime             <- Licor$summary$mean[,"FTime"    ]; } else {val$sum$Licor$FTime          <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Photo"    )) { val$sum$Licor$A                 <- Licor$summary$mean[,"Photo"    ]; } else {val$sum$Licor$A              <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Trmmol"   )) { val$sum$Licor$E                 <- Licor$summary$mean[,"Trmmol"   ]; } else {val$sum$Licor$E              <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Tair"     )) { val$sum$Licor$temp_air          <- Licor$summary$mean[,"Tair"     ]; } else {val$sum$Licor$temp_air       <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Tleaf"    )) { val$sum$Licor$temp_leaf         <- Licor$summary$mean[,"Tleaf"    ]; } else {val$sum$Licor$temp_leaf      <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="TBlk"     )) { val$sum$Licor$temp_block        <- Licor$summary$mean[,"TBlk"     ]; } else {val$sum$Licor$temp_block     <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="CO2R"     )) { val$sum$Licor$Ce                <- Licor$summary$mean[,"CO2R"     ]; } else {val$sum$Licor$Ce             <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="CO2S"     )) { val$sum$Licor$Co                <- Licor$summary$mean[,"CO2S"     ]; } else {val$sum$Licor$Co             <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="H2OR"     )) { val$sum$Licor$xin               <- Licor$summary$mean[,"H2OR"     ]; } else {val$sum$Licor$xin            <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="H2OS"     )) { val$sum$Licor$xout              <- Licor$summary$mean[,"H2OS"     ]; } else {val$sum$Licor$xout           <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="RH_R"     )) { val$sum$Licor$rh_ref            <- Licor$summary$mean[,"RH_R"     ]; } else {val$sum$Licor$rh_ref         <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="RH_S"     )) { val$sum$Licor$rh_sam            <- Licor$summary$mean[,"RH_S"     ]; } else {val$sum$Licor$rh_sam         <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Flow"     )) { val$sum$Licor$uin               <- Licor$summary$mean[,"Flow"     ]; } else {val$sum$Licor$uin            <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="PARi"     )) { val$sum$Licor$par_int           <- Licor$summary$mean[,"PARi"     ]; } else {val$sum$Licor$par_int        <- matrix(NA,Licor$summary$n,1)};   # PAR or PPFD  These two are defined in the same way at top of CalculationsFile.xls
  if (sum(colnames(Licor$summary$mean)=="PARo"     )) { val$sum$Licor$par_ext           <- Licor$summary$mean[,"PARo"     ]; } else {val$sum$Licor$par_ext        <- matrix(NA,Licor$summary$n,1)};   # PAR or PPFD
  if (sum(colnames(Licor$summary$mean)=="Press"    )) { val$sum$Licor$Atm_press         <- Licor$summary$mean[,"Press"    ]; } else {val$sum$Licor$Atm_press      <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="CsMch"    )) { val$sum$Licor$CsMch             <- Licor$summary$mean[,"CsMch"    ]; } else {val$sum$Licor$CsMch          <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="HsMch"    )) { val$sum$Licor$HsMch             <- Licor$summary$mean[,"HsMch"    ]; } else {val$sum$Licor$HsMch          <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="StableF"  )) { val$sum$Licor$StableF           <- Licor$summary$mean[,"StableF"  ]; } else {val$sum$Licor$StableF        <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Status"   )) { val$sum$Licor$Status            <- Licor$summary$mean[,"Status"   ]; } else {val$sum$Licor$Status         <- matrix(NA,Licor$summary$n,1)};
    # Variables NOT in calculations
  if (sum(colnames(Licor$summary$mean)=="Cond"     )) { val$sum$Licor$gsc               <- Licor$summary$mean[,"Cond"     ]; } else {val$sum$Licor$gsc            <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Ci"       )) { val$sum$Licor$Ci                <- Licor$summary$mean[,"Ci"       ]; } else {val$sum$Licor$Ci             <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="VpdL"     )) { val$sum$Licor$VPD               <- Licor$summary$mean[,"VpdL"     ]; } else {val$sum$Licor$VPD            <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Area"     )) { val$sum$Licor$La                <- Licor$summary$mean[,"Area"     ]; } else {val$sum$Licor$La             <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="StmRat"   )) { val$sum$Licor$StmRat            <- Licor$summary$mean[,"StmRat"   ]; } else {val$sum$Licor$StmRat         <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="BLCond"   )) { val$sum$Licor$gbw               <- Licor$summary$mean[,"BLCond"   ]; } else {val$sum$Licor$gbw            <- matrix(NA,Licor$summary$n,1)};
  # additional columns
  if (sum(colnames(Licor$summary$mean)=="VpdA"     )) { val$sum$Licor$VpdA              <- Licor$summary$mean[,"VpdA"     ]; } else {val$sum$Licor$VpdA           <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Ci_Ca"    )) { val$sum$Licor$Ci_Ca             <- Licor$summary$mean[,"Ci_Ca"    ]; } else {val$sum$Licor$Ci_Ca          <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Ci_Pa"    )) { val$sum$Licor$pi                <- Licor$summary$mean[,"Ci_Pa"    ]; } else {val$sum$Licor$pi             <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="uc_20_mV" )) { val$sum$Licor$uc_20_mV          <- Licor$summary$mean[,"uc_20_mV" ]; } else {val$sum$Licor$uc_20_mV       <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="uc_21_mV" )) { val$sum$Licor$uc_21_mV          <- Licor$summary$mean[,"uc_21_mV" ]; } else {val$sum$Licor$uc_21_mV       <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="X.U.S."   )) { val$sum$Licor$U_S               <- Licor$summary$mean[,"X.U.S."   ]; } else {val$sum$Licor$U_S            <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Trans"    )) { val$sum$Licor$Trans             <- Licor$summary$mean[,"Trans"    ]; } else {val$sum$Licor$Trans          <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="CndCO2"   )) { val$sum$Licor$CndCO2            <- Licor$summary$mean[,"CndCO2"   ]; } else {val$sum$Licor$CndCO2         <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="Ref_mV"   )) { val$sum$Licor$Ref_mV            <- Licor$summary$mean[,"Ref_mV"   ]; } else {val$sum$Licor$Ref_mV         <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="xTemp1"   )) { val$sum$Licor$xTemp1            <- Licor$summary$mean[,"xTemp1"   ]; } else {val$sum$Licor$xTemp1         <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="xTemp2"   )) { val$sum$Licor$xTemp2            <- Licor$summary$mean[,"xTemp2"   ]; } else {val$sum$Licor$xTemp2         <- matrix(NA,Licor$summary$n,1)};
  if (sum(colnames(Licor$summary$mean)=="TChamAir" )) { val$sum$Licor$TChamAir          <- Licor$summary$mean[,"TChamAir" ]; } else {val$sum$Licor$TChamAir       <- matrix(NA,Licor$summary$n,1)};

  #TDL$summary:
  val$sum$TDL$n                   <- TDL$summary$n;
  val$sum$TDL$first_ind           <- TDL$summary$first_ind;
  val$sum$TDL$ind                 <- TDL$summary$ind;
  val$sum$TDL$site                <- TDL$summary$site;
  val$sum$TDL$n_sam               <- TDL$summary$n_sam;
  val$sum$TDL$time                <- TDL$summary$time;
  val$sum$TDL$conc12CO2           <- TDL$summary$mean[,"ConcA"                ];
  val$sum$TDL$conc13CO2           <- TDL$summary$mean[,"ConcB"                ];
  val$sum$TDL$TGApressure         <- TDL$summary$mean[,"TGApressure"          ];
  val$sum$TDL$MassFlow1           <- TDL$summary$mean[,"MassFlow1"            ];
  val$sum$TDL$Pressure1           <- TDL$summary$mean[,"Pressure1"            ];
  val$sum$TDL$MassFlow2           <- TDL$summary$mean[,"MassFlow2"            ];
  val$sum$TDL$Pressure2           <- TDL$summary$mean[,"Pressure2"            ];
  val$sum$TDL$PressureProMan      <- TDL$summary$mean[,"PressureProMan"       ];
  val$sum$TDL$interp_tank_hi_12   <- TDL$summary$mean[,"interp_tank_hi_12"    ];
  val$sum$TDL$interp_tank_hi_13   <- TDL$summary$mean[,"interp_tank_hi_13"    ];
  val$sum$TDL$interp_tank_low_12  <- TDL$summary$mean[,"interp_tank_low_12"   ];
  val$sum$TDL$interp_tank_low_13  <- TDL$summary$mean[,"interp_tank_low_13"   ];
  val$sum$TDL$interp_reference_12 <- TDL$summary$mean[,"interp_reference_12"  ];
  val$sum$TDL$interp_reference_13 <- TDL$summary$mean[,"interp_reference_13"  ];
  val$sum$TDL$chamber_12          <- val$sum$TDL$conc12CO2; val$sum$TDL$chamber_12[!(val$sum$TDL$site == TDL_cycle$number_chamber)] <- NA;
  val$sum$TDL$chamber_13          <- val$sum$TDL$conc13CO2; val$sum$TDL$chamber_13[!(val$sum$TDL$site == TDL_cycle$number_chamber)] <- NA;


  ## "Which values to use" switches 9/5/2012
  # val$obs$Licor
  if (sw$val_const_override_Licor_flow_rate                    ) { val$obs$Licor$uin     <- rep(val$const$flow_rate                   , length(val$obs$Licor$uin   )); };
  if (sw$val_const_override_Licor_leaf_area                    ) { val$obs$Licor$La      <- rep(val$const$leaf_area                   , length(val$obs$Licor$La    )); };
  if (sw$val_const_override_Licor_boundary_layer_cond_to_water ) { val$obs$Licor$gbw     <- rep(val$const$boundary_layer_cond_to_water, length(val$obs$Licor$gbw   )); };
  if (sw$val_const_override_Licor_H2OS                         ) { val$obs$Licor$xout    <- rep(val$const$H2OS                        , length(val$obs$Licor$xout  )); };
  if (sw$val_const_override_Licor_StmRat                       ) { val$obs$Licor$StmRat  <- rep(val$const$StmRat                      , length(val$obs$Licor$StmRat)); };
  # val$sum$Licor
  if (sw$val_const_override_Licor_flow_rate                    ) { val$sum$Licor$uin     <- rep(val$const$flow_rate                   , length(val$sum$Licor$uin   )); };
  if (sw$val_const_override_Licor_leaf_area                    ) { val$sum$Licor$La      <- rep(val$const$leaf_area                   , length(val$sum$Licor$La    )); };
  if (sw$val_const_override_Licor_boundary_layer_cond_to_water ) { val$sum$Licor$gbw     <- rep(val$const$boundary_layer_cond_to_water, length(val$sum$Licor$gbw   )); };
  if (sw$val_const_override_Licor_H2OS                         ) { val$sum$Licor$xout    <- rep(val$const$H2OS                        , length(val$sum$Licor$xout  )); };
  if (sw$val_const_override_Licor_StmRat                       ) { val$sum$Licor$StmRat  <- rep(val$const$StmRat                      , length(val$sum$Licor$StmRat)); };

  return( val );
  ### val
}

