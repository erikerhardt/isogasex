calc.mean.TDL <-
function# Calculate the mean and variance for the TDL/Licor data based on last measurements
###
(TDL
###
, TDL.cycle
###
)
{

  TDL.var.names <- c(
      "ConcA"
     ,"ConcB"
     ,"TGApressure"
     ,"MassFlow1"
     ,"Pressure1"
     ,"MassFlow2"
     ,"Pressure2"
     ,"PressureProMan");

  #Licor.var.names <- c("FTime", "Photo", "Cond", "Ci", "Trmmol", "VpdL", "Area"
  #  , "StmRat", "BLCond", "Tair", "Tleaf", "TBlk", "CO2R", "CO2S", "H2OR"
  #  , "H2OS", "RH_R", "RH_S", "Flow", "PARi", "PARo", "Press", "CsMch"
  #  , "HsMch", "StableF", "Status"
  #  # additional columns
  #  , "VpdA", "Ci.Ca", "Ci_Pa", "uc_20_mV", "uc_21_mV", "X.U.S.", "Trans", "CndCO2", "Ref_mV", "xTemp1", "xTemp2");

  ##details<<
  ## create TDL$summary for numerical summaries
  TDL$summary           <- as.list(new.env());
  TDL$summary$n         <- dim(TDL$last.list)[1];
  TDL$summary$first.ind <- TDL$last.list[,"first.ind"];
  TDL$summary$ind       <- TDL$last.list[,"ind"];
  TDL$summary$site      <- TDL$last.list[,"site"];
  TDL$summary$n.sam     <- rep(0, TDL$summary$n);
  TDL$summary$time      <- TDL$time[TDL$summary$ind];
  TDL$summary$mean      <- matrix(0,nrow=TDL$summary$n,ncol=length(TDL.var.names));
  TDL$summary$var       <- matrix(0,nrow=TDL$summary$n,ncol=length(TDL.var.names));
  TDL$summary$sd        <- matrix(0,nrow=TDL$summary$n,ncol=length(TDL.var.names));

  ## create Licor$summary for numerical summaries
  #Licor$summary           <- as.list(new.env());
  #Licor$summary$n         <- TDL$summary$n;
  #Licor$summary$ind       <- TDL$summary$ind;
  #Licor$summary$first.ind <- TDL$summary$first.ind;
  #Licor$summary$site      <- TDL$summary$site;
  #Licor$summary$time      <- TDL$summary$time;
  #Licor$summary$mean      <- matrix(0,nrow=Licor$summary$n,ncol=length(Licor.var.names));

  colnames(TDL$summary$mean ) <- TDL.var.names;
  colnames(TDL$summary$var  ) <- TDL.var.names;
  colnames(TDL$summary$sd   ) <- TDL.var.names;
  #colnames(Licor$summary$mean ) <- Licor.var.names;

  ##details<<
  ## Calculate mean, var, and sd for each site for retained observations
  ind.temp <- NULL;
  for (i.list in 1:TDL$summary$n) {
    # summarized values
    i.n.obs <- TDL.cycle$table[(TDL$summary$site[i.list] == TDL.cycle$table[,"site"]),"last.n.obs"]; # number of obs
    i.ind.last <- TDL$summary$ind[i.list];                                                           # last index
    i.ind.first <- TDL$summary$first.ind[i.list];                                                    # first index
    TDL$summary$n.sam [i.list]  <- i.n.obs;                                                          # number of obs
    rows.temp <- i.ind.first:i.ind.last;
    if (length(rows.temp) > 1) {
      TDL$summary$mean  [i.list,] <- apply( TDL$data  [rows.temp, TDL.var.names]  , MARGIN=2, mean, na.rm = TRUE); # mean, ignoring NA's
      TDL$summary$var   [i.list,] <- apply( TDL$data  [rows.temp, TDL.var.names]  , MARGIN=2, var , na.rm = TRUE); # var , ignoring NA's
      TDL$summary$sd    [i.list,] <- apply( TDL$data  [rows.temp, TDL.var.names]  , MARGIN=2, sd  , na.rm = TRUE); # sd  , ignoring NA's
    } else { # if only 1 observation, don't use apply "0.1-16" "2012-07-10"
      TDL$summary$mean  [i.list,] <- as.matrix(TDL$data  [rows.temp, TDL.var.names]); # mean
      # var and sd are initialized at 0
    }

    ind.temp <- c(ind.temp, rows.temp);  # indices to keep in data
  }

  ind.temp.NA <- x.wo.y(seq(1,TDL$n), ind.temp); # excluded indices

  # data updated to exclude indices that we don't summarize
  TDL$ind   [ind.temp.NA]  <- NA;
  TDL$time  [ind.temp.NA]  <- NA;
  TDL$data  [ind.temp.NA,] <- NA;
  #Licor$data[ind.temp.NA,] <- NA;

  #TDL.summary <- as.list(new.env());  # create a list to return with data
  #TDL.summary$TDL   <- TDL;
  #TDL.Licor.summary$Licor <- Licor;
  return( TDL );
  ### TDL
}

