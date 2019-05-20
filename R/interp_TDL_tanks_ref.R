#' Title
#'
#' @param TDL
#' @param TDL_cycle
#' @param plot_format_list
#' @param output_fn_prefix
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
, TDL_cycle
###
, plot_format_list
###
, output_fn_prefix
###
, sw
###
)
{

  ##details<<
  ## Create TDL$interp to hold interpolated values between measurement cycles and observed values within measurement cycles

  TDL$interp <- matrix(0,nrow=TDL$n,ncol=6);  # create a list to return with data
  colnames(TDL$interp) <- c("tank_hi_12", "tank_hi_13", "tank_low_12" , "tank_low_13", "reference_12", "reference_13");

  ##details<<
  ## List of sites (hi, low, ref) to perform smooth interpolation (1).
  i_site_list_smooth = NULL;
  if (sw$interp_tank_hi   == 1) { i_site_list_smooth <- c(i_site_list_smooth, 1); };
  if (sw$interp_tank_low  == 1) { i_site_list_smooth <- c(i_site_list_smooth, 2); };
  if (sw$interp_reference == 1) { i_site_list_smooth <- c(i_site_list_smooth, 3); };

  ##details<<
  ## For those sites with interp=1, perform smooth interpolation using \code{smooth.spline}.
  for (i_site in i_site_list_smooth) {
    #n_sites <- sum(TDL$summary$site == TDL_cycle$table[i_site,1]); # set spline nknots and df to n_sites, smooth not overfit
    n_sites <- sum(TDL$last_list[,3] == TDL_cycle$table[i_site,1]); # set spline nknots and df to n_sites, smooth not overfit
    ind <- (TDL$data[,"PrevSite"] == TDL_cycle$table[i_site,1]);
      # the TRUE values and non-NA values
    ind <- intersect((1:TDL$n)[(ind == TRUE)], (1:TDL$n)[!is.na(ind)]);
    ind_first <- ind[1];
    ind_last  <- ind[length(ind)];

    # 12CO2
      ##details<<
      ## Cubic spline interpolation using \code{smooth.spline} with \code{nknots}=\code{df}=number of cycles for current site.
    ssA <- smooth.spline(ind,TDL$data[ind,"ConcA"], nknots=n_sites, df=n_sites);
    TDL$interp[,i_site*2-1] <- predict(ssA,1:TDL$n)$y;   # TDL$time
      ##details<<
      ## Extrapolate the first/last cal tank to the first/last time values for the run.
    TDL$interp[1:ind_first,i_site*2-1]    <- TDL$interp[ind_first,i_site*2-1];
    TDL$interp[ind_last:TDL$n,i_site*2-1] <- TDL$interp[ind_last,i_site*2-1];

    # 13CO2
    ssB <- smooth.spline(ind,TDL$data[ind,"ConcB"], nknots=n_sites, df=n_sites);
    TDL$interp[,i_site*2] <- predict(ssB,1:TDL$n)$y;   # TDL$time
      # extrapolate the first/last cal tank to the beginning/ending time values
    TDL$interp[1:ind_first,i_site*2]    <- TDL$interp[ind_first,i_site*2];
    TDL$interp[ind_last:TDL$n,i_site*2] <- TDL$interp[ind_last,i_site*2];

    ##details<<
    ## Fill within block times with data
    # Fill within block times with data (was 0s before) "0.1-16" "2012-07-10"
    ind_summary <- (TDL$last_list[,3] == TDL_cycle$table[i_site,1]);
    ind_table <- TDL$last_list[ind_summary,]; # table of start/end indices for site
    n_meas <- dim(ind_table)[1];
    ind_first <- ind_table[1,1];
    ind_last  <- ind_table[n_meas,2];
    for (i_block in 1:(n_meas-1)) {
      # fill within block times with data (was 0s before) "0.1-16" "2012-07-10"
      ind_block_in <- seq(ind_table[i_block,1], ind_table[i_block,2]); # indices to fill with flat interp values
      TDL$interp[ind_block_in, i_site*2-1] <- TDL$data$ConcA[ind_block_in]; # 12CO2
      TDL$interp[ind_block_in, i_site*2]   <- TDL$data$ConcB[ind_block_in]; # 13CO2
    }
  }

  ##details<<
  ## List of sites (hi, low, ref) to perform "last mean value" flat interpolation (2).
  i_site_list_flat = NULL;
  if (sw$interp_tank_hi   == 2) { i_site_list_flat <- c(i_site_list_flat, 1); };
  if (sw$interp_tank_low  == 2) { i_site_list_flat <- c(i_site_list_flat, 2); };
  if (sw$interp_reference == 2) { i_site_list_flat <- c(i_site_list_flat, 3); };

  ##details<<
  ## For those sites with interp=2, perform last mean value flat interpolation.
  for (i_site in i_site_list_flat) {
    ind_summary <- (TDL$last_list[,3] == TDL_cycle$table[i_site,1]);
    ind_table <- TDL$last_list[ind_summary,]; # table of start/end indices for site
    site_means <- TDL$summary$mean[ind_summary,]; # summarized means
    n_meas <- dim(ind_table)[1];
    ind_first <- ind_table[1,1];
    ind_last  <- ind_table[n_meas,2];

    for (i_block in 1:(n_meas-1)) {
      last_mean <- site_means[i_block,c("ConcA","ConcB")];
      ind_block_out <- seq(ind_table[i_block,2]+1, ind_table[i_block+1,1]-1); # indices to fill with flat interp values
      TDL$interp[ind_block_out,i_site*2-1] <- last_mean["ConcA"]; # 12CO2
      TDL$interp[ind_block_out,i_site*2]   <- last_mean["ConcB"]; # 13CO2

      # fill within block times with data (was 0s before) "0.1-16" "2012-07-10"
      ind_block_in <- seq(ind_table[i_block,1], ind_table[i_block,2]); # indices to fill with flat interp values
      TDL$interp[ind_block_in, i_site*2-1] <- TDL$data$ConcA[ind_block_in]; # 12CO2
      TDL$interp[ind_block_in, i_site*2]   <- TDL$data$ConcB[ind_block_in]; # 13CO2
    }

  }

  i_site_list_none <- setdiff(1:3, c(i_site_list_smooth, i_site_list_flat)); # these were not interpolated!

  ##details<<
  ## Plot the interpolated values.
  for (i_plot in plot_format_list)
  {
    plot_filename <- "plot_TDL_interp";
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    for (i_site in 1:3) {
      #n_sites <- sum(TDL$summary$site == TDL_cycle$table[i_site,1]); # set spline nknots and df to n_sites, smooth not overfit
      n_sites <- sum(TDL$last_list[,3] == TDL_cycle$table[i_site,1]); # set spline nknots and df to n_sites, smooth not overfit
      ind <- (TDL$data[,"PrevSite"] == TDL_cycle$table[i_site,1]);
        # the TRUE values and non-NA values
      ind <- intersect((1:TDL$n)[(ind == TRUE)], (1:TDL$n)[!is.na(ind)]);
      ind_first <- ind[1];
      ind_last  <- ind[length(ind)];

      # 12CO2
      plot(TDL$time,TDL$interp[,i_site*2-1],type="l" , ylab="ConcA 12CO2", xlab="");
      points(TDL$time[ind],TDL$data[ind,"ConcA"], pch=20);  # ind   ,pch=20, type="l",ylim=c(475.3,476)
      title(main=paste("Interpolation of TDL ", TDL_cycle$table_name[i_site], ", 12CO2", sep=""));
      # 7/20/2012 used indices -- updated to times on x-axis
      #plot(ind,TDL$data[ind,"ConcA"], ylab="ConcA 12CO2", xlim=c(1,TDL$n), xlab="", pch=20);  # ind   ,pch=20, type="l",ylim=c(475.3,476)
      #points(1:TDL$n,TDL$interp[,i_site*2-1],type="l");
      #title(main=paste("Interpolation of TDL ", TDL_cycle$table_name[i_site], ", 12CO2", sep=""));

      # 13CO2
      plot(TDL$time,TDL$interp[,i_site*2],type="l" , ylab="ConcB 13CO2", xlab="");
      points(TDL$time[ind],TDL$data[ind,"ConcB"], pch=20);  # ind   ,pch=20, type="l",ylim=c(475.3,476)
      title(main=paste("Interpolation of TDL ", TDL_cycle$table_name[i_site], ", 13CO2", sep=""));
      # 7/20/2012 used indices -- updated to times on x-axis
      #plot(ind,TDL$data[ind,"ConcB"], ylab="ConcB 13CO2", xlim=c(1,TDL$n), xlab="", pch=20);  # ,pch=20, type="l" ,ylim=c(475.3,476)
      #points(1:TDL$n,TDL$interp[,i_site*2],type="l");
      #title(main=paste("Interpolation of TDL ", TDL_cycle$table_name[i_site], ", 13CO2", sep=""));
    }

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end");
  } # plotting loop

  return(TDL);
  ### TDL
}

