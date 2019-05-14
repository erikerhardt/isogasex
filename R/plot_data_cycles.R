#' Title
#'
#' @param TDL
#' @param TDL.cycle
#' @param plot.format.list
#' @param output.fn.prefix
#' @param Rstd.13C
#' @param full.or.window
#'
#' @return
#' @export
#'
#' @examples
plot_data_cycles <-
function# Plot each cycle of TDL measurements (visual diagnostics)
###
(TDL
###
, TDL.cycle
###
, plot.format.list
###
, output.fn.prefix
###
, Rstd.13C
###
, full.or.window
###
)
{

  ##details<<
  ## Plots the input variables.

  ## DEBUG
  # TDL               <-  TDL               ;
  # TDL.cycle         <-  TDL.cycle         ;
  # plot.format.list  <-  plot.format.list  ;
  # output.fn.prefix  <-  output.fn.prefix  ;
  # Rstd.13C          <-  val$const$Rstd.13C;
  # full.or.window    <-  "full"            ;

  # plot full data
  for (i.plot in plot.format.list)
  {
    plot.filename <- "plot_TDL_all_data_full";
    if (full.or.window == "full")   { plot.filename <- paste(plot.filename,"_full",sep="");   };
    if (full.or.window == "window") { plot.filename <- paste(plot.filename,"_window",sep=""); };
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    # full time
    par(mfrow=c(2,1), mar=c(2,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    plot(TDL$time, TDL$data[,"ConcA"],pch=20, ylab="12CO2", type="l");
    title(main="TDL full data, 12CO2 and 13CO2");
    plot(TDL$time, TDL$data[,"ConcB"],pch=20, ylab="13CO2", type="l");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end");
  } # plotting loop

  # plot tanks and reference cycles
  for (i.plot in plot.format.list)
  {
    plot.filename <- "plot_TDL_tanks_ref";
    if (full.or.window == "full")   { plot.filename <- paste(plot.filename,"_full",sep="");   };
    if (full.or.window == "window") { plot.filename <- paste(plot.filename,"_window",sep=""); };
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    for (i.site in 1:3) {
      ind2 <- TDL$last.list[(TDL$last.list[,"site"] == TDL.cycle$table[i.site,1]),];
        # 4/8/2014 If only 1 row, need to reshape.
        if (is.null(dim(ind2))) { ind2.temp <- matrix(ind2, ncol = 3); colnames(ind2.temp) <- names(ind2); ind2 <- ind2.temp; };
      ind.all <- NULL;
        for (i.ind.all in 1:dim(ind2)[1]) { ind.all <- c(ind.all, ind2[i.ind.all,1]:ind2[i.ind.all,2]); };
      ind.vline <- c(0,seq(1,length(ind.all))[ind.all %in% ind2[,2]]);

      plot(TDL$data[,"ConcA"][ind.all],pch=20, ylab="ConcA", xlab="");
        abline(v=ind.vline);
        for (i.ind.vline in 1:(length(ind.vline)-1)) {
          # 7/20/2012 1:22PM doesn't work for last plot when fewer than the full number of points
          ## NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          #temp.y <- TDL$data[seq(ind2[i.ind.vline,1],ind2[i.ind.vline,2]),"ConcA"];
          #temp.x <- seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]);
          #  #cat(i.plot, i.site, i.ind.vline, "temp.x=", temp.x, "\n");
          ##line.lm <- lm( temp.y ~ temp.x );
          ##line.lm <- lm(TDL$data[seq(ind2[i.ind.vline,1],ind2[i.ind.vline,2]),"ConcA"] ~ seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]));
          #line.x <- c(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]);
          #line.y <- my_reg_pred(my_reg_fit(temp.y, temp.x), line.x);
          ##line.y <- predict(line.lm, 1)[c(1,line.x[2]-line.x[1]+1)];
          #lines(line.x, line.y, lwd=2, col="red");
          # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          temp.y <- TDL$data[seq(ind2[i.ind.vline,1],ind2[i.ind.vline,2]),"ConcA"];
          temp.x <- seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]);

          # 7/20/2012 1:22PM now works for last plot when fewer than the full number of points
          ind.temp.y <- !is.na(temp.y);
          temp.y <- temp.y[ind.temp.y];
          temp.x <- temp.x[ind.temp.y];
          line.x <- range(temp.x);

          line.y <- my_reg_pred(my_reg_fit(temp.y, temp.x), line.x);
          lines(line.x, line.y, lwd=2, col="red");
        }
      title(main=paste("TDL cycles:  ", TDL.cycle$table.name[i.site], ", 12CO2", sep="")
          , xlab=paste("last ", TDL.cycle$table[i.site,2], " measurements from each cycle", sep=""));

      plot(TDL$data[,"ConcB"][ind.all],pch=20, ylab="ConcB", xlab="");
        abline(v=ind.vline);
        for (i.ind.vline in 1:(length(ind.vline)-1)) {
          # 7/20/2012 1:22PM doesn't work for last plot when fewer than the full number of points
          ## NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          #temp.y <- TDL$data[seq(ind2[i.ind.vline,1],ind2[i.ind.vline,2]),"ConcB"];
          #temp.x <- seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]);
          ##  cat(i.plot, i.site, i.ind.vline, "temp.x=", temp.x, "\n");
          ##line.lm <- lm( temp.y ~ temp.x );
          ##line.lm <- lm(TDL$data[seq(ind2[i.ind.vline,1],ind2[i.ind.vline,2]),"ConcB"] ~ seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]));
          #line.x <- c(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]);
          ##line.y <- predict(line.lm, 1)[c(1,line.x[2]-line.x[1]+1)];
          #line.y <- my_reg_pred(my_reg_fit(temp.y, temp.x), line.x);
          #lines(line.x, line.y, lwd=2, col="red");
          # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          temp.y <- TDL$data[seq(ind2[i.ind.vline,1],ind2[i.ind.vline,2]),"ConcB"];
          temp.x <- seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]);

          # 7/20/2012 1:22PM now works for last plot when fewer than the full number of points
          ind.temp.y <- !is.na(temp.y);
          temp.y <- temp.y[ind.temp.y];
          temp.x <- temp.x[ind.temp.y];
          line.x <- range(temp.x);

          line.y <- my_reg_pred(my_reg_fit(temp.y, temp.x), line.x);
          lines(line.x, line.y, lwd=2, col="red");
        }
      title(main=paste("TDL cycles:  ", TDL.cycle$table.name[i.site], ", 13CO2", sep="")
          , xlab=paste("last ", TDL.cycle$table[i.site,2], " measurements from each cycle", sep=""));
    } # i.site

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end");
  } # plotting loop

  # plot chamber cycles
  for (i.plot in plot.format.list)
  {
    plot.filename <- "plot_TDL_chamber";
    if (full.or.window == "full")   { plot.filename <- paste(plot.filename,"_full",sep="");   };
    if (full.or.window == "window") { plot.filename <- paste(plot.filename,"_window",sep=""); };
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(2,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    i.site <- 4;
    ind2 <- TDL$last.list[(TDL$last.list[,"site"] == TDL.cycle$table[i.site,1]),];
    if (length(ind2)){ # only if cycles for chamber (don't run if no chamber cycles)
      ind.all <- NULL;
        for (i.ind.all in 1:dim(ind2)[1]) { ind.all <- c(ind.all, ind2[i.ind.all,1]:ind2[i.ind.all,2]); };

      plot(TDL$time[ind.all],TDL$data[,"ConcA"][ind.all],pch=20, ylab="ConcA", xlab="", type="l");
      title(main=paste("TDL cycles:  ", TDL.cycle$table.name[i.site], ", 12CO2", sep=""));

      plot(TDL$time[ind.all],TDL$data[,"ConcB"][ind.all],pch=20, ylab="ConcB", xlab="", type="l");
      title(main=paste("TDL cycles:  ", TDL.cycle$table.name[i.site], ", 13CO2", sep=""));
    } else {  # if no chamber cycles
      plot(0,0);
      title(main=paste("TDL cycles:  ** NONE ** ", TDL.cycle$table.name[i.site], ", 12CO2", sep=""));
      plot(0,0);
      title(main=paste("TDL cycles:  ** NONE ** ", TDL.cycle$table.name[i.site], ", 13CO2", sep=""));
    }
    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end");
  } # plotting loop

  # delta ratio should remain constant for tanks and reference
  for (i.plot in plot.format.list)
  {
    plot.filename <- "plot_TDL_tanks_ref_delta";
    if (full.or.window == "full")   { plot.filename <- paste(plot.filename,"_full",sep="");   };
    if (full.or.window == "window") { plot.filename <- paste(plot.filename,"_window",sep=""); };
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "begin", plot.format = i.plot);

    par(mfrow=c(3,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    for (i.site in 1:3) {
      ind2 <- TDL$last.list[(TDL$last.list[,"site"] == TDL.cycle$table[i.site,1]),];
        # 4/8/2014 If only 1 row, need to reshape.
        if (is.null(dim(ind2))) { ind2.temp <- matrix(ind2, ncol = 3); colnames(ind2.temp) <- names(ind2); ind2 <- ind2.temp; };
      ind.all <- NULL;
        for (i.ind.all in 1:dim(ind2)[1]) { ind.all <- c(ind.all, ind2[i.ind.all,1]:ind2[i.ind.all,2]); };
      ind.vline <- c(0,seq(1,length(ind.all))[ind.all %in% ind2[,2]]);

      delta.permil <-
        f_val_calc_delta_permil( TDL$data[,"ConcA"][ind.all], TDL$data[,"ConcB"][ind.all], Rstd.13C);

      plot(delta.permil,pch=20, ylab="delta.permil", xlab="");
        abline(v=ind.vline);
        for (i.ind.vline in 1:(length(ind.vline)-1)) {
          # 7/20/2012 1:22PM doesn't work for last plot when fewer than the full number of points
          ## NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          #temp.y <- delta.permil[seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1])];
          #temp.x <- seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]);
          ##  cat(i.plot, i.site, i.ind.vline, "temp.x=", temp.x, "\n");
          ##line.lm <- lm( temp.y ~ temp.x );
          ##line.lm <- lm(delta.permil[seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1])] ~ seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]));
          #line.x <- c(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]);
          ##line.y <- predict(line.lm, 1)[c(1,line.x[2]-line.x[1]+1)];
          #line.y <- my_reg_pred(my_reg_fit(temp.y, temp.x), line.x);
          #lines(line.x, line.y, lwd=2, col="red");
          # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.

          temp.y <- delta.permil[seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1])];
          temp.x <- seq(ind.vline[i.ind.vline]+1,ind.vline[i.ind.vline+1]);

          # 7/20/2012 1:22PM now works for last plot when fewer than the full number of points
          ind.temp.y <- !is.na(temp.y);
          temp.y <- temp.y[ind.temp.y];
          temp.x <- temp.x[ind.temp.y];
          line.x <- range(temp.x);

          line.y <- my_reg_pred(my_reg_fit(temp.y, temp.x), line.x);
          lines(line.x, line.y, lwd=2, col="red");
        }
      title(main=paste("TDL cycles:  ", TDL.cycle$table.name[i.site], ", delta.permil", sep="")
          , xlab=paste("last ", TDL.cycle$table[i.site,2], " measurements from each cycle", sep=""));

    } # i.site

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output.fn.prefix, plot.filename, plot.mode = "end");
  } # plotting loop

  return( NULL );
  ### NULL
}

