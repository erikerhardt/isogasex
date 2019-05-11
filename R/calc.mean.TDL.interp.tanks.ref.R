calc.mean.TDL.interp.tanks.ref <-
function# Summary values for TDL interpolated tanks and reference
###
(TDL
###
, TDL.cycle
###
)
{
  TDL.var.names <- colnames(TDL$interp);

  TDL.interp.summary.mean <- matrix(0, nrow=TDL$summary$n, ncol=length(TDL.var.names));
  TDL.interp.summary.var  <- matrix(0, nrow=TDL$summary$n, ncol=length(TDL.var.names));
  TDL.interp.summary.sd   <- matrix(0, nrow=TDL$summary$n, ncol=length(TDL.var.names));

  ##details<<
  ## Calculate mean, var, and sd for each site for interpolated ranges.
  ind.temp <- NULL;
  for (i.list in 1:TDL$summary$n) {
    rows.temp <- TDL$summary$first.ind[i.list]:TDL$summary$ind[i.list];
    if (length(rows.temp) > 1) {
      TDL.interp.summary.mean[i.list,] <- apply( TDL$interp[rows.temp,]  , MARGIN=2, mean, na.rm = TRUE); # mean, ignoring NA's
      TDL.interp.summary.var [i.list,] <- apply( TDL$interp[rows.temp,]  , MARGIN=2, var , na.rm = TRUE); # var , ignoring NA's
      TDL.interp.summary.sd  [i.list,] <- apply( TDL$interp[rows.temp,]  , MARGIN=2, sd  , na.rm = TRUE); # sd  , ignoring NA's
    } else { # if only 1 observation, don't use apply "0.1-16" "2012-07-10"
      TDL.interp.summary.mean[i.list,] <- as.matrix(TDL$interp[rows.temp,]); # mean
      # var and sd are initialized at 0
    }
    ind.temp <- c(ind.temp, TDL$summary$first.ind[i.list]:TDL$summary$ind[i.list]);  # indices to keep in data
  }

  ind.temp.NA <- x.wo.y(seq(1,TDL$n), ind.temp); # excluded indices

  ##details<<
  ## Data updated to exclude indices that we don't summarize.
  TDL$interp[ind.temp.NA,] <- NA;

  colnames(TDL.interp.summary.mean ) <- paste("interp.",TDL.var.names,sep="");
  colnames(TDL.interp.summary.var  ) <- paste("interp.",TDL.var.names,sep="");
  colnames(TDL.interp.summary.sd   ) <- paste("interp.",TDL.var.names,sep="");

  TDL$summary$mean <- cbind(TDL$summary$mean , TDL.interp.summary.mean);
  TDL$summary$var  <- cbind(TDL$summary$var  , TDL.interp.summary.var );
  TDL$summary$sd   <- cbind(TDL$summary$sd   , TDL.interp.summary.sd  );

  return( TDL );
  ### TDL
}

