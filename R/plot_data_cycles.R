#' Plot each cycle of TDL measurements (visual diagnostics)
#'
#' Plots the input variables.
#'
#' @param TDL xxxPARAMxxx
#' @param TDL_cycle xxxPARAMxxx
#' @param plot_format_list xxxPARAMxxx
#' @param output_fn_prefix xxxPARAMxxx
#' @param Rstd_13C xxxPARAMxxx
#' @param full_or_window xxxPARAMxxx
#'
#' @return NULL xxxRETURNxxx
#' @importFrom graphics par plot points lines abline title
#'
plot_data_cycles <-
function# Plot each cycle of TDL measurements (visual diagnostics)
###
(TDL
###
, TDL_cycle
###
, plot_format_list
###
, output_fn_prefix
###
, Rstd_13C
###
, full_or_window
###
)
{

  ##details<<
  ## Plots the input variables.

  ## DEBUG
  # TDL               <-  TDL               ;
  # TDL_cycle         <-  TDL_cycle         ;
  # plot_format_list  <-  plot_format_list  ;
  # output_fn_prefix  <-  output_fn_prefix  ;
  # Rstd_13C          <-  val$const$Rstd_13C;
  # full_or_window    <-  "full"            ;

  # plot full data
  for (i_plot in plot_format_list)
  {
    plot_filename <- "plot_TDL_all_data_full";
    if (full_or_window == "full")   { plot_filename <- paste(plot_filename,"_full",sep="");   };
    if (full_or_window == "window") { plot_filename <- paste(plot_filename,"_window",sep=""); };
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    # full time
    graphics::par(mfrow=c(2,1), mar=c(2,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    graphics::plot(TDL$time, TDL$data[,"ConcA"],pch=20, ylab="12CO2", type="l");
    graphics::title(main="TDL full data, 12CO2 and 13CO2");
    graphics::plot(TDL$time, TDL$data[,"ConcB"],pch=20, ylab="13CO2", type="l");

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end");
  } # plotting loop

  # plot tanks and reference cycles
  for (i_plot in plot_format_list)
  {
    plot_filename <- "plot_TDL_tanks_ref";
    if (full_or_window == "full")   { plot_filename <- paste(plot_filename,"_full",sep="");   };
    if (full_or_window == "window") { plot_filename <- paste(plot_filename,"_window",sep=""); };
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    graphics::par(mfrow=c(3,2), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    for (i_site in 1:3) {
      ind2 <- TDL$last_list[(TDL$last_list[,"site"] == TDL_cycle$table[i_site,1]),];
        # 4/8/2014 If only 1 row, need to reshape.
        if (is.null(dim(ind2))) { ind2.temp <- matrix(ind2, ncol = 3); colnames(ind2.temp) <- names(ind2); ind2 <- ind2.temp; };
      ind_all <- NULL;
        for (i_ind_all in 1:dim(ind2)[1]) { ind_all <- c(ind_all, ind2[i_ind_all,1]:ind2[i_ind_all,2]); };
      ind_vline <- c(0,seq(1,length(ind_all))[ind_all %in% ind2[,2]]);

      graphics::plot(TDL$data[,"ConcA"][ind_all],pch=20, ylab="ConcA", xlab="");
        graphics::abline(v=ind_vline);
        for (i_ind_vline in 1:(length(ind_vline)-1)) {
          # 7/20/2012 1:22PM doesn't work for last plot when fewer than the full number of points
          ## NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          #temp_y <- TDL$data[seq(ind2[i_ind_vline,1],ind2[i_ind_vline,2]),"ConcA"];
          #temp_x <- seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]);
          #  #cat(i_plot, i_site, i_ind_vline, "temp_x=", temp_x, "\n");
          ##line_lm <- lm( temp_y ~ temp_x );
          ##line_lm <- lm(TDL$data[seq(ind2[i_ind_vline,1],ind2[i_ind_vline,2]),"ConcA"] ~ seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]));
          #line_x <- c(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]);
          #line_y <- my_reg_pred(my_reg_fit(temp_y, temp_x), line_x);
          ##line_y <- predict(line_lm, 1)[c(1,line_x[2]-line_x[1]+1)];
          #graphics::lines(line_x, line_y, lwd=2, col="red");
          # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          temp_y <- TDL$data[seq(ind2[i_ind_vline,1],ind2[i_ind_vline,2]),"ConcA"];
          temp_x <- seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]);

          # 7/20/2012 1:22PM now works for last plot when fewer than the full number of points
          ind_temp_y <- !is.na(temp_y);
          temp_y <- temp_y[ind_temp_y];
          temp_x <- temp_x[ind_temp_y];
          line_x <- range(temp_x);

          line_y <- my_reg_pred(my_reg_fit(temp_y, temp_x), line_x);
          graphics::lines(line_x, line_y, lwd=2, col="red");
        }
      graphics::title(main=paste("TDL cycles:  ", TDL_cycle$table_name[i_site], ", 12CO2", sep="")
          , xlab=paste("last ", TDL_cycle$table[i_site,2], " measurements from each cycle", sep=""));

      graphics::plot(TDL$data[,"ConcB"][ind_all],pch=20, ylab="ConcB", xlab="");
        graphics::abline(v=ind_vline);
        for (i_ind_vline in 1:(length(ind_vline)-1)) {
          # 7/20/2012 1:22PM doesn't work for last plot when fewer than the full number of points
          ## NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          #temp_y <- TDL$data[seq(ind2[i_ind_vline,1],ind2[i_ind_vline,2]),"ConcB"];
          #temp_x <- seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]);
          ##  cat(i_plot, i_site, i_ind_vline, "temp_x=", temp_x, "\n");
          ##line_lm <- lm( temp_y ~ temp_x );
          ##line_lm <- lm(TDL$data[seq(ind2[i_ind_vline,1],ind2[i_ind_vline,2]),"ConcB"] ~ seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]));
          #line_x <- c(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]);
          ##line_y <- predict(line_lm, 1)[c(1,line_x[2]-line_x[1]+1)];
          #line_y <- my_reg_pred(my_reg_fit(temp_y, temp_x), line_x);
          #graphics::lines(line_x, line_y, lwd=2, col="red");
          # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          temp_y <- TDL$data[seq(ind2[i_ind_vline,1],ind2[i_ind_vline,2]),"ConcB"];
          temp_x <- seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]);

          # 7/20/2012 1:22PM now works for last plot when fewer than the full number of points
          ind_temp_y <- !is.na(temp_y);
          temp_y <- temp_y[ind_temp_y];
          temp_x <- temp_x[ind_temp_y];
          line_x <- range(temp_x);

          line_y <- my_reg_pred(my_reg_fit(temp_y, temp_x), line_x);
          graphics::lines(line_x, line_y, lwd=2, col="red");
        }
      graphics::title(main=paste("TDL cycles:  ", TDL_cycle$table_name[i_site], ", 13CO2", sep="")
          , xlab=paste("last ", TDL_cycle$table[i_site,2], " measurements from each cycle", sep=""));
    } # i_site

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end");
  } # plotting loop

  # plot chamber cycles
  for (i_plot in plot_format_list)
  {
    plot_filename <- "plot_TDL_chamber";
    if (full_or_window == "full")   { plot_filename <- paste(plot_filename,"_full",sep="");   };
    if (full_or_window == "window") { plot_filename <- paste(plot_filename,"_window",sep=""); };
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    graphics::par(mfrow=c(2,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    i_site <- 4;
    ind2 <- TDL$last_list[(TDL$last_list[,"site"] == TDL_cycle$table[i_site,1]),];
    if (length(ind2)){ # only if cycles for chamber (don't run if no chamber cycles)
      ind_all <- NULL;
        for (i_ind_all in 1:dim(ind2)[1]) { ind_all <- c(ind_all, ind2[i_ind_all,1]:ind2[i_ind_all,2]); };

      graphics::plot(TDL$time[ind_all],TDL$data[,"ConcA"][ind_all],pch=20, ylab="ConcA", xlab="", type="l");
      graphics::title(main=paste("TDL cycles:  ", TDL_cycle$table_name[i_site], ", 12CO2", sep=""));

      graphics::plot(TDL$time[ind_all],TDL$data[,"ConcB"][ind_all],pch=20, ylab="ConcB", xlab="", type="l");
      graphics::title(main=paste("TDL cycles:  ", TDL_cycle$table_name[i_site], ", 13CO2", sep=""));
    } else {  # if no chamber cycles
      graphics::plot(0,0);
      graphics::title(main=paste("TDL cycles:  ** NONE ** ", TDL_cycle$table_name[i_site], ", 12CO2", sep=""));
      graphics::plot(0,0);
      graphics::title(main=paste("TDL cycles:  ** NONE ** ", TDL_cycle$table_name[i_site], ", 13CO2", sep=""));
    }
    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end");
  } # plotting loop

  # delta ratio should remain constant for tanks and reference
  for (i_plot in plot_format_list)
  {
    plot_filename <- "plot_TDL_tanks_ref_delta";
    if (full_or_window == "full")   { plot_filename <- paste(plot_filename,"_full",sep="");   };
    if (full_or_window == "window") { plot_filename <- paste(plot_filename,"_window",sep=""); };
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "begin", plot_format = i_plot);

    graphics::par(mfrow=c(3,1), mar=c(4,4,2,2), oma=c(1,1,1,1));  # mar allows the histograms to touch top-bottom c(bot,lef,top,rig)
    for (i_site in 1:3) {
      ind2 <- TDL$last_list[(TDL$last_list[,"site"] == TDL_cycle$table[i_site,1]),];
        # 4/8/2014 If only 1 row, need to reshape.
        if (is.null(dim(ind2))) { ind2.temp <- matrix(ind2, ncol = 3); colnames(ind2.temp) <- names(ind2); ind2 <- ind2.temp; };
      ind_all <- NULL;
        for (i_ind_all in 1:dim(ind2)[1]) { ind_all <- c(ind_all, ind2[i_ind_all,1]:ind2[i_ind_all,2]); };
      ind_vline <- c(0,seq(1,length(ind_all))[ind_all %in% ind2[,2]]);

      delta_permil <-
        f_val_calc_delta_permil( TDL$data[,"ConcA"][ind_all], TDL$data[,"ConcB"][ind_all], Rstd_13C);

      graphics::plot(delta_permil,pch=20, ylab="delta_permil", xlab="");
        graphics::abline(v=ind_vline);
        for (i_ind_vline in 1:(length(ind_vline)-1)) {
          # 7/20/2012 1:22PM doesn't work for last plot when fewer than the full number of points
          ## NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
          #temp_y <- delta_permil[seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1])];
          #temp_x <- seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]);
          ##  cat(i_plot, i_site, i_ind_vline, "temp_x=", temp_x, "\n");
          ##line_lm <- lm( temp_y ~ temp_x );
          ##line_lm <- lm(delta_permil[seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1])] ~ seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]));
          #line_x <- c(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]);
          ##line_y <- predict(line_lm, 1)[c(1,line_x[2]-line_x[1]+1)];
          #line_y <- my_reg_pred(my_reg_fit(temp_y, temp_x), line_x);
          #graphics::lines(line_x, line_y, lwd=2, col="red");
          # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.

          temp_y <- delta_permil[seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1])];
          temp_x <- seq(ind_vline[i_ind_vline]+1,ind_vline[i_ind_vline+1]);

          # 7/20/2012 1:22PM now works for last plot when fewer than the full number of points
          ind_temp_y <- !is.na(temp_y);
          temp_y <- temp_y[ind_temp_y];
          temp_x <- temp_x[ind_temp_y];
          line_x <- range(temp_x);

          line_y <- my_reg_pred(my_reg_fit(temp_y, temp_x), line_x);
          graphics::lines(line_x, line_y, lwd=2, col="red");
        }
      graphics::title(main=paste("TDL cycles:  ", TDL_cycle$table_name[i_site], ", delta_permil", sep="")
          , xlab=paste("last ", TDL_cycle$table[i_site,2], " measurements from each cycle", sep=""));

    } # i_site

    #axis(3); axis(4); # add axis labels to top and right sides
    s_plot_settings_begin_end(output_fn_prefix, plot_filename, plot_mode = "end");
  } # plotting loop

  invisible(NULL);
  ### NULL
}

