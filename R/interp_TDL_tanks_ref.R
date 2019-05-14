#' Title
#'
#' @param TDL
#' @param TDL.cycle
#' @param plot.format.list
#' @param output.fn.prefix
#' @param sw
#'
#' @return
#' @export
#'
#' @examples
interp_TDL_tanks_ref <-
function# Interpolate TDL tank and reference values
###
(TDL
###
, TDL.cycle
###
, plot.format.list
###
, output.fn.prefix
###
, sw
###
)
{

  ##details<<
  ## Create TDL$interp to hold interpolated values between measurement cycles and observed values within measurement cycles

  TDL$interp <- matrix(0,nrow=TDL$n,ncol=6);  # create a list to return with data
  colnames(TDL$interp) <- c("tank.hi.12", "tank.hi.13", "tank.low.12" , "tank.low.13", "reference.12", "reference.13");

  ##details<<
  ## List of sites (hi, low, ref) to perform smooth interpolation (1).
  i.site.list.smooth = NULL;
  if (sw$interp.tank.hi   == 1) { i.site.list.smooth <- c(i.site.list.smooth, 1); };
  if (sw$interp.tank.low  == 1) { i.site.list.smooth <- c(i.site.list.smooth, 2); };
  if (sw$interp.reference == 1) { i.site.list.smooth <- c(i.site.list.smooth, 3); };

  ##details<<
  ## For those sites with interp=1, perform smooth interpolation using \code{smooth.spline}.
  for (i.site in i.site.list.smooth) {
    #n.sites <- sum(TDL$summary$site == TDL.cycle$table[i.site,1]); # set spline nknots and df to n.sites, smooth not overfit
    n.sites <- sum(TDL$last.list[,3] == TDL.cycle$table[i.site,1]); # set spline nknots and df to n.sites, smooth not overfit
    ind <- (TDL$data[,"PrevSite"] == TDL.cycle$table[i.site,1]);
      # the TRUE values and non-NA values
    ind <- intersect((1:TDL$n)[(ind == TRUE)], (1:TDL$n)[!is.na(ind)]);
    ind.first <- ind[1];
    ind.last  <- ind[length(ind)];

    # 12CO2
      ##details<<
      ## Cubic spline interpolation using \code{smooth.spline} with \code{nknots}=\code{df}=number of cycles for current site.
    ssA <- smooth.spline(ind,TDL$data[ind,"ConcA"], nknots=n.sites, df=n.sites);
    TDL$interp[,i.site*2-1] <- predict(ssA,1:TDL$n)$y;   # TDL$time
      ##details<<
      ## Extrapolate the first/last cal tank to the first/last time values for the run.
    TDL$interp[1:ind.first,i.site*2-1]    <- TDL$interp[ind.first,i.site*2-1];
    TDL$interp[ind.last:TDL$n,i.site*2-1] <- TDL$interp[ind.last,i.site*2-1];

    # 13CO2
    ssB <- smooth.spline(ind,TDL$data[ind,"ConcB"], nknots=n.sites, df=n.sites);
    TDL$interp[,i.site*2] <- predict(ssB,1:TDL$n)$y;   # TDL$time
      # extrapolate the first/last cal tank to the beginning/ending time values
    TDL$interp[1:ind.first,i.site*2]    <- TDL$interp[ind.first,i.site*2];
    TDL$interp[ind.last:TDL$n,i.site*2] <- TDL$interp[ind.last,i.site*2];

    ##details<<
    ## Fill within block times with data
    # Fill within block times with data (was 0s before) "0.1-16" "2012-07-10"
    ind.summary <- (TDL$last.list[,3] == TDL.cycle$table[i.site,1]);
    ind.table <- TDL$last.list[ind.summary,]; # table of start/end indices for site
    n.meas <- dim(ind.table)[1];
    ind.first <- ind.table[1,1];
    ind.last  <- ind.table[n.meas,2];
    for (i.block in 1:(n.meas-1)) {
      # fill within block times with data (was 0s before) "0.1-16" "2012-07-10"
      ind.block.in <- seq(ind.table[i.block,1], ind.table[i.block,2]); # indices to fill with flat interp values
      TDL$interp[ind.block.in, i.site*2-1] <- TDL$data$ConcA[ind.block.in]; # 12CO2
      TDL$interp[ind.block.in, i.site*2]   <- TDL$data$ConcB[ind.block.in]; # 13CO2
    }
  }

  ##details<<
  ## List of sites (hi, low, ref) to perform "last mean value" flat interpolation (2).
  i.site.list.flat = NULL;
  if (sw$interp.tank.hi   == 2) { i.site.list.flat <- c(i.site.list.flat, 1); };
  if (sw$interp.tank.low  == 2) { i.site.list.flat <- c(i.site.list.flat, 2); };
  if (sw$interp.reference == 2) { i.site.list.flat <- c(i.site.list.flat, 3); };

  ##details<<
  ## For those sites with interp=2, perform last mean value flat interpolation.
  for (i.site in i.site.list.flat) {
    ind.summary <- (TDL$last.list[,3] == TDL.cycle$table[i.site,1]);
    ind.table <- TDL$last.list[ind.summary,]; # table of start/end indices for site
    site.means <- TDL$summary$mean[ind.summary,]; # summarized means
    n.meas <- dim(ind.table)[1];
    ind.first <- ind.table[1,1];
    ind.last  <- ind.table[n.meas,2];

    for (i.block in 1:(n.meas-1)) {
      last.mean <- site.means[i.block,c("ConcA","ConcB")];
      ind.block.out <- seq(ind.table[i.block,2]+1, ind.table[i.block+1,1]-1); # indices to fill with flat interp values
      TDL$interp[ind.block.out,i.site*2-1] <- last.mean["ConcA"]; # 12CO2
      TDL$interp[ind.block.out,i.site*2]   <- last.mean["ConcB"]; # 13CO2

      # fill within block times with data (was 0s before) "0.1-16" "2012-07-10"
      ind.block.in <- seq(ind.table[i.block,1], ind.table[i.block,2]); # indices to fill with flat interp values
      TDL$interp[ind.block.in, i.site*2-1] <- TDL$data$ConcA[ind.block.in]; # 12CO2
      TDL$interp[ind.block.in, i.site*2]   <- TDL$data$ConcB[ind.block.in]; # 13CO2
    }

  }

  i.site.list.none <- setdiff(1:3, c(i.site.list.smooth, i.site.list.flat)); # these were not interpolated!

  ##details<<
  ## Plot the interpolated values.
  for (i.plot in plot.format.list)
  {
    plot.filename <- "plot_TDL_interp";
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    for (i.site in 1:3) {
      #n.sites <- sum(TDL$summary$site == TDL.cycle$table[i.site,1]); # set spline nknots and df to n.sites, smooth not overfit
      n.sites <- sum(TDL$last.list[,3] == TDL.cycle$table[i.site,1]); # set spline nknots and df to n.sites, smooth not overfit
      ind <- (TDL$data[,"PrevSite"] == TDL.cycle$table[i.site,1]);
        # the TRUE values and non-NA values
      ind <- intersect((1:TDL$n)[(ind == TRUE)], (1:TDL$n)[!is.na(ind)]);
      ind.first <- ind[1];
      ind.last  <- ind[length(ind)];

      # 12CO2
      plot(TDL$time,TDL$interp[,i.site*2-1],type="l" , ylab="ConcA 12CO2", xlab="");
      points(TDL$time[ind],TDL$data[ind,"ConcA"], pch=20);  # ind   ,pch=20, type="l",ylim=c(475.3,476)
      title(main=paste("Interpolation of TDL ", TDL.cycle$table.name[i.site], ", 12CO2", sep=""));
      # 7/20/2012 used indices -- updated to times on x-axis
      #plot(ind,TDL$data[ind,"ConcA"], ylab="ConcA 12CO2", xlim=c(1,TDL$n), xlab="", pch=20);  # ind   ,pch=20, type="l",ylim=c(475.3,476)
      #points(1:TDL$n,TDL$interp[,i.site*2-1],type="l");
      #title(main=paste("Interpolation of TDL ", TDL.cycle$table.name[i.site], ", 12CO2", sep=""));

      # 13CO2
      plot(TDL$time,TDL$interp[,i.site*2],type="l" , ylab="ConcB 13CO2", xlab="");
      points(TDL$time[ind],TDL$data[ind,"ConcB"], pch=20);  # ind   ,pch=20, type="l",ylim=c(475.3,476)
      title(main=paste("Interpolation of TDL ", TDL.cycle$table.name[i.site], ", 13CO2", sep=""));
      # 7/20/2012 used indices -- updated to times on x-axis
      #plot(ind,TDL$data[ind,"ConcB"], ylab="ConcB 13CO2", xlim=c(1,TDL$n), xlab="", pch=20);  # ,pch=20, type="l" ,ylim=c(475.3,476)
      #points(1:TDL$n,TDL$interp[,i.site*2],type="l");
      #title(main=paste("Interpolation of TDL ", TDL.cycle$table.name[i.site], ", 13CO2", sep=""));
    }

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end");
  } # plotting loop

  return(TDL);
  ### TDL
}

