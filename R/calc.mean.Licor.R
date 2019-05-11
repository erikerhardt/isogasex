calc.mean.Licor <-
function# Calculate the mean for the Licor data based on last measurements
###
(TDL
###
, Licor
###
, TDL.cycle
###
, sw
###
)
{
  Licor.var.names <- colnames(Licor$data); # 7/15/2010 don't hardset column names
    #c("FTime", "Photo", "Cond", "Ci", "Trmmol", "VpdL", "Area"
    #, "StmRat", "BLCond", "Tair", "Tleaf", "TBlk", "CO2R", "CO2S", "H2OR"
    #, "H2OS", "RH_R", "RH_S", "Flow", "PARi", "PARo", "Press", "CsMch"
    #, "HsMch", "StableF", "Status"
    ## additional columns
    #, "VpdA", "Ci.Ca", "Ci_Pa", "uc_20_mV", "uc_21_mV", "X.U.S.", "Trans", "CndCO2", "Ref_mV", "xTemp1", "xTemp2");  # , "TChamAir"

  ##details<<
  ## create Licor$summary for numerical summaries
  Licor$summary           <- as.list(new.env());
  Licor$summary$n         <- TDL$summary$n;
  Licor$summary$ind       <- TDL$summary$ind;
  Licor$summary$first.ind <- TDL$summary$first.ind;
  Licor$summary$site      <- TDL$summary$site;
  Licor$summary$time      <- TDL$summary$time;
  Licor$summary$mean      <- matrix(0,nrow=Licor$summary$n,ncol=length(Licor.var.names));

  colnames(Licor$summary$mean ) <- Licor.var.names;

  ##details<<
  ## Calculate mean for each site for retained observations
  ind.temp <- NULL;
  for (i.list in 1:Licor$summary$n) {
    # summarized values
    i.n.obs <- TDL.cycle$table[(TDL$summary$site[i.list] == TDL.cycle$table[,"site"]),"last.n.obs"]; # number of obs
    i.ind.first <- TDL$summary$first.ind[i.list];                                                    # first index
    i.ind.last  <- TDL$summary$ind[i.list];                                                          # last index
    if (i.ind.last > Licor$n) {i.ind.last <- Licor$n;};
    rows.temp <- i.ind.first:i.ind.last;
    if (length(rows.temp) > 1) {
      Licor$summary$mean[i.list,] <- apply( Licor$data[rows.temp, Licor.var.names], MARGIN=2, mean, na.rm = TRUE); # mean, ignoring NA's
    } else { # if only 1 observation, don't use apply "0.1-16" "2012-07-10"
      Licor$summary$mean[i.list,] <- as.matrix(Licor$data[rows.temp, Licor.var.names]); # mean
    }

    #ind.temp <- c(ind.temp, i.ind.first:i.ind.last);  # indices to keep in data
  }

  #ind.temp.NA <- x.wo.y(seq(1,TDL$n), ind.temp); # excluded indices
  ind.temp.NA <- is.na(TDL$time); # excluded indices

  # data updated to exclude indices that we don't summarize
  Licor$data[ind.temp.NA,] <- NA;

  return( Licor );
  ### Licor
}

