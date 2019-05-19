#' Title
#'
#' @param input.fn
#' @param path
#'
#' @return
#' @export
#'
#' @examples
isogasex <-
function# Reads TDL and Licor files, aligns them, calculates quantities of interest with bootstrap intervals.
### Edit \file{isogasex_templateX.xls} and input parameters and TDL/Licor filenames. Run \code{\link{isogasex}}. See output in \file{./out} directory.
### Edit isogasex_templateX.xls and input parameters and TDL/Licor filenames.
### Run isogasex().
### See output in ./out directory.
(input.fn
### The Excel workbook name.  A modified version of isogasex_templateX.xls
, path=getwd()
### Directory where TDL and Licor data are to be read from, and where ./out directory for results are to be written to.
)
{
  isogasex_logo()
  # DRIVER FUNCTION -------------------------------------------------------------
  # DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
  # #library(isogasex)
  # #path <- "C:/Dropbox/StatAcumen/consult/Authorship/DavidHanson_Isotopes_2009/Sharing with Erhardt/Daves problem July 19";
  # #input.fn <- "isogasex_template3.xls"
  # #setwd(path);
  # rm(list=ls()); # clear workspace
  # library(isogasex)
  # path <- "c:\\Dropbox\\StatAcumen\\consult\\Authorship\\DavidHanson_Isotopes_2009\\Sharing\ with\ Erhardt\\201207_Photoresp_conf_paper\\Pater2011round2data";
  # setwd(path);
  # input.fn <- "2011Pater_isogasex_05262011AT_PMDH201.xls"
  # #isogasex(input.fn)
  #
  # DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG

  # install required packages if not in R already
  #list.of.packages <- c("gdata", "zoo")
  #new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  #if(length(new.packages)) {
  #  install.packages(new.packages);
  #  if (.Platform$OS.type == "windows") {
  #    library(xlsReadWrite);  xls.getshlib();  # get shared library
  #  }
  #}

  ##details<<
  ## SECTION Preamble
  ## Set version.
  ## Display header.
  isogasex.version  <- "0.1-23";
  isogasex.date     <- "2014-04-08"; # yyyy-mm-dd
  isogasex.template <- 4;

  ##details<<
  ## Save original working directory, change to data dir.
  ## (This is a little hokey.)  Change to data dir, then to out dir, then back to original at end.
    path.original <- getwd(); # changed back at very end of isogasex()
  ##details<<
  ## Create output directory.
  setwd(path);

  p.o.temp <- NULL; # initial output buffer to print after we create prefix output directory

  time.start <- proc.time()[3];    # start timer
    p.o.temp <- rbind(p.o.temp, paste("isogasex: TDL/Licor processing", "\n\n", sep=""));
    p.o.temp <- rbind(p.o.temp, paste("  Version: ", isogasex.version, ", Date: ", isogasex.date,", Template ", isogasex.template, "\n", sep=""));
    p.o.temp <- rbind(p.o.temp, paste("  Concept by David T. Hanson", "\n"));
    p.o.temp <- rbind(p.o.temp, paste("  Written by Erik B. Erhardt", "\n"));
    p.o.temp <- rbind(p.o.temp, paste("  Professors at the University of New Mexico", "\n\n"));
    p.o.temp <- rbind(p.o.temp, paste("Starting ", Sys.time(), "\n\n"));

    p.o.temp <- rbind(p.o.temp, paste("\n\n"));
    p.o.temp <- rbind(p.o.temp, paste("SESSION INFO AND PACKAGE VERSION ==== BEGIN ===================================="));
    p.o.temp <- rbind(p.o.temp, paste("\n\n"));
    p.o.temp <- rbind(p.o.temp, as.matrix(capture.output(sessionInfo()), ncol=1));
    p.o.temp <- rbind(p.o.temp, paste("\n\n"));
    p.o.temp <- rbind(p.o.temp, as.matrix(capture.output(packageDescription("isogasex")), ncol=1));
    p.o.temp <- rbind(p.o.temp, paste("\n\n"));
    p.o.temp <- rbind(p.o.temp, paste("SESSION INFO AND PACKAGE VERSION ==== END   ===================================="));
    p.o.temp <- rbind(p.o.temp, paste("\n\n"));

  # convert date/time from character to POSIX
    options(digits.secs=1); # set resolution of seconds in time to 0.1

  ##details<<
  ## SECTION Read data.
  ##details<<
  ## Input Excel workbook. \code{\link{get_data}}
  filename <- paste(path, "/", input.fn, sep="");
    #p.o = paste("Reading workbook: ", input.fn, "\n"); wWw <- write_progress(p.o, time.start);
    p.o.temp <- rbind(p.o.temp, paste("Reading workbook: ", input.fn, "\n"));
  DATA <- get_data(filename);   # read data in

  ##details<<
  ## Assign variables. \code{\link{assign_variables}}
    #p.o = paste("Assign variables", "\n"); wWw <- write_progress(p.o, time.start);
    p.o.temp <- rbind(p.o.temp, paste("Assign variables", "\n"));
  VARIABLES <- assign_variables(DATA, path)
  val                             <- VARIABLES$val                          ;
  sw                              <- VARIABLES$sw                           ;
  TDL.fn                          <- VARIABLES$TDL.fn                       ;
  Licor.fn                        <- VARIABLES$Licor.fn                     ;
  plot.format.list                <- VARIABLES$plot.format.list             ;
  output.fn.prefix                <- VARIABLES$output.fn.prefix             ;
  output.summary.TDL.fn           <- VARIABLES$output.summary.TDL.fn        ;
  output.summary.Licor.fn         <- VARIABLES$output.summary.Licor.fn      ;
  output.summary.Calc.fn          <- VARIABLES$output.summary.Calc.fn       ;
  output.summary.Calc.last.fn     <- VARIABLES$output.summary.Calc.last.fn  ;
  output.all.Calc.fn              <- VARIABLES$output.all.Calc.fn           ; # "0.1-16" "2012-07-11"
  output.CI.TDL.fn                <- VARIABLES$output.CI.TDL.fn             ;
  output.CI.Licor.fn              <- VARIABLES$output.CI.Licor.fn           ;
  output.CI.Calc.fn               <- VARIABLES$output.CI.Calc.fn            ;
  output.CI.Calc.last.fn          <- VARIABLES$output.CI.Calc.last.fn       ;
  R.bootstrap                     <- VARIABLES$R.bootstrap                  ;
  sig.CI                          <- VARIABLES$sig.CI                       ;
  seed                            <- VARIABLES$seed                         ;
  TDL.cycle                       <- VARIABLES$TDL.cycle                    ;
  Licor.TDL.time.offset.seconds   <- VARIABLES$Licor.TDL.time.offset.seconds;
  rm(VARIABLES);

  ##details<<
  ## Create output directory (prefix) and update the process_info.txt.
  path.output.fn.prefix <- paste("out_",output.fn.prefix,sep="");
  path.prefix <- paste(path, "/", path.output.fn.prefix, sep="");
  #path.out <- paste(path, "/outtemp", sep="");
  path.out <- path.prefix;
  if (!file.exists(path.output.fn.prefix)) {
    dir.create(path.out);
  }
  setwd(path.out);

  process.filename <- "process_info.txt";  # hardcoded in write_out() and write_progress()
  if (!is.na(file.info(process.filename)$size)) {
    file.remove(process.filename);
  }; # delete old process_info file
  wWw <- write_out(p.o.temp); # write log so far

  ##details<<
  ## Check that template version matches version of isogasex.
  if (sw$template.version != isogasex.template) {
    error.message <- paste("ERROR: Template version mismatch! \n",
                           "       Template version of ", input.fn, " is ", sw$template.version, ". isogasex version ", isogasex.version, "requires template ", isogasex.template, " \n", sep="");
    stop(error.message);
  };

  ##details<<
  ## Set random seed for samples.
  if (seed == 0 | !is.numeric(seed)) { seed <- round(as.numeric(Sys.time())); };
    p.o = paste("Set random seed =", seed, "\n"); wWw <- write_progress(p.o, time.start);
  set.seed(seed);

  ##details<<
  ## Read TDL and Licor data.  \code{\link{read_TDL}} and \code{\link{read_Licor}}
    p.o <- paste("Reading TDL and/or Licor data \n"); wWw <- write_progress(p.o, time.start);
    p.o <- paste("Reading TDL data \n"); wWw <- write_progress(p.o, time.start);
  TDL   <- read_TDL(TDL.fn, sw); # read_TDL file,   reference with TDL$[col.name]

    p.o <- paste("Reading Licor data \n"); wWw <- write_progress(p.o, time.start);
  Licor <- read_Licor(Licor.fn, Licor.TDL.time.offset.seconds, sw); # read_Licor file, reference with Licor$[col.name]

  ##details<<
  ## SECTION TDL Plot and interpolate.
  ##details<<
  ## BEGIN If using TDL:
  if (sw$use.TDL  ) {

    if (sw$print.TDL.missing){
      ##details<<
      ## _ Print lines with missing values in TDL file and fix them. \code{\link{fix_missing_TDL_values}}
      bad.ind <- TDL$ind[TDL$ind*ceiling(apply(is.na(TDL$data), MARGIN=1, mean))];
      if (length(bad.ind) > 0) {
        p.o <- paste("Printing", length(bad.ind), "lines with missing values in TDL file (a few are normal, CHECK THAT NOT TOO MANY ARE CLOSE IN TIME)\n"); wWw <- write_progress(p.o, time.start);
        TDL.missing <- TDL$data[bad.ind,];
        p.o <- TDL.missing; wWw <- write_progress(p.o, time.start, type = "matrix");
        rm(TDL.missing);
        p.o <- paste("  Note: Replacing bad TDL values in ", length(bad.ind), "lines with previous value in the file\n"); wWw <- write_progress(p.o, time.start);
        TDL <- fix_missing_TDL_values(TDL, bad.ind);
      }
      rm(bad.ind);
    }

    if (sw$print.TDL.cycle.timing){
      ##details<<
      ## _ Extract cycle timing, write to file. \code{\link{extract_cycle_timing}}
      TDL.site.timing.filename <- paste(output.fn.prefix,"_TDL_site_timing.csv",sep="");
        p.o <- paste("Extract cycle timing, write to file: ", TDL.site.timing.filename, "\n"); wWw <- write_progress(p.o, time.start);
        p.o <- paste("  (takes a LONG time)", "\n"); wWw <- write_out(p.o);
      extract_cycle_timing(TDL, TDL.cycle, TDL.site.timing.filename); # usually not run because it takes a long time
    }

    ##details<<
    ## _ Determine the last index for admissible values to calculate means. \code{\link{last_TDL_index_for_means}}
      p.o <- paste("Determine the last index for admissible values to calculate means\n"); wWw <- write_progress(p.o, time.start);
    TDL <- last_TDL_index_for_means(TDL, TDL.cycle); # creates TDL$last.list

    ##details<<
    ## _ Plot each cycle of TDL measurements (visual diagnostics). \code{\link{plot_data_cycles}}
      p.o <- paste("Plot data cycles for full TDL\n"); wWw <- write_progress(p.o, time.start);
      # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
    plot_data_cycles(TDL, TDL.cycle, plot.format.list, output.fn.prefix, val$const$Rstd.13C, "full");

    ##details<<
    ## _ Calculate the mean and variance for the TDL data based on last measurements. \code{\link{calc_mean_TDL}}
      p.o <- paste("Calculate the mean and variance for the TDL data based on last measurements\n"); wWw <- write_progress(p.o, time.start);
    TDL <- calc_mean_TDL(TDL, TDL.cycle);

    ##details<<
    ## _ Interpolate TDL tank and reference values. \code{\link{interp_TDL_tanks_ref}}
      p.o <- paste("Interpolate TDL tank and reference values\n"); wWw <- write_progress(p.o, time.start);
    TDL <- interp_TDL_tanks_ref(TDL, TDL.cycle, plot.format.list, output.fn.prefix, sw)

    ##details<<
    ## _ Summary values for TDL interpolated tanks and reference. \code{\link{calc_mean_TDL_interp_tanks_ref}}
      p.o <- paste("Calculate the mean and variance for the interpolated TDL tank and reference values\n"); wWw <- write_progress(p.o, time.start);
    TDL <- calc_mean_TDL_interp_tanks_ref(TDL, TDL.cycle);

    ##details<<
    ## _ Calculate mean Variance for interp values for Par BS of tanks and reference.
    var.interp <- as.list(new.env());
        ind.tank.hi           <- (TDL$summary$site == TDL.cycle$number.tank.hi);
      var.interp$tank.hi.12   <- mean(TDL$summary$var[ind.tank.hi,"interp.tank.hi.12"]);
      var.interp$tank.hi.13   <- mean(TDL$summary$var[ind.tank.hi,"interp.tank.hi.13"]);
        ind.tank.low          <- (TDL$summary$site == TDL.cycle$number.tank.low);
      var.interp$tank.low.12  <- mean(TDL$summary$var[ind.tank.low,"interp.tank.low.12"]);
      var.interp$tank.low.13  <- mean(TDL$summary$var[ind.tank.low,"interp.tank.low.13"]);
        ind.reference         <- (TDL$summary$site == TDL.cycle$number.reference);
      var.interp$reference.12 <- mean(TDL$summary$var[ind.reference,"interp.reference.12"]);
      var.interp$reference.13 <- mean(TDL$summary$var[ind.reference,"interp.reference.13"]);
  }; # if sw$use.TDL
  ##details<<
  ## END

  ##details<<
  ## SECTION Align TDL and Licor
  ##details<<
  ## Determine overlapping time window of TDL and Licor measurements. \code{\link{align_TDL_Licor_times}}
    p.o <- paste("Determine overlapping times for TDL, Licor, and Time Window \n"); wWw <- write_progress(p.o, time.start);
  # 7/19/2010 need to test for TDL or Licor only
  TDL.Licor.times <- align_TDL_Licor_times(TDL$time, TDL$n, Licor$time, Licor$n, val$timewindow, sw);

  ##details<<
  ## Reduce data to overlapping time window and interp Licor values to TDL timepoints. \code{\link{time_window_TDL_Licor_interp}}
    p.o <- paste("Reduce data to overlapping time window and interp Licor values to TDL timepoints \n"); wWw <- write_progress(p.o, time.start);
  TDL.and.Licor.interp   <- time_window_TDL_Licor_interp(TDL, Licor, TDL.Licor.times, sw);
    TDL           <- TDL.and.Licor.interp$TDL;           # TDL data
    Licor         <- TDL.and.Licor.interp$Licor.interp;  # Licor interp data
    rm(TDL.and.Licor.interp);

  ##details<<
  ## Plot each cycle of TDL measurements (visual diagnostics). \code{\link{plot_data_cycles}}
    p.o <- paste("Plot data cycles for TDL/Licor overlap window\n"); wWw <- write_progress(p.o, time.start);
    # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
  plot_data_cycles(TDL, TDL.cycle, plot.format.list, output.fn.prefix, val$const$Rstd.13C, "window");

  if (sw$use.Licor ) {
    ##details<<
    ## Calculate the mean and variance for the Licor data based on last measurements. \code{\link{calc_mean_Licor}}
      p.o <- paste("Calculate the mean and variance for the TDL/Licor data based on last measurements\n"); wWw <- write_progress(p.o, time.start);
    Licor <- calc_mean_Licor(TDL, Licor, TDL.cycle, sw);
  };


  #-------------------
  ##details<<
  ## SECTION Mean Calculations

  ##details<<
  ## Assign TDL and Licor values to val variable names, raw and summarized values. \code{\link{val_TDL_Licor_variables}}
    p.o <- paste("Assign TDL and Licor values to val variable names, raw and summarized values\n"); wWw <- write_progress(p.o, time.start);
  val <- val_TDL_Licor_variables(TDL, Licor, TDL.cycle, val, sw);

  ##details<<
  ## summary values (mean). \code{\link{f_val_calc_all_driver}}
    p.o <- paste("Calculate mean values\n"); wWw <- write_progress(p.o, time.start);
  val$calc$sum <- f_val_calc_all_driver(val$sum$TDL, val$sum$Licor, val$const, sw);

  # do val$calc$all "0.1-16" "2012-07-10"
  ##details<<
  ## All time point values. \code{\link{f_val_calc_all_driver}}
    p.o <- paste("Calculate all values\n"); wWw <- write_progress(p.o, time.start);
  val$calc$all <- f_val_calc_all_driver(val$obs$TDL, val$obs$Licor, val$const, sw);
  val$calc$all$site <- val$obs$TDL$PrevSite;
  val$calc$all$time <- TDL$time;  # strptime(paste(val$obs$TDL$TIMESTAMP), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
  val$calc$all$n    <- TDL$n;
  val$calc$all$ind  <- 1:val$calc$all$n;


  #-------------------
  ##details<<
  ## SECTION Write out results
    p.o <- paste("Write mean summary files\n"); wWw <- write_progress(p.o, time.start);
  ##details<<
  ## Summary files
  if (sw$use.TDL ) {
    ##details<<
    ## TDL file. \code{\link{write_summary_TDL_file}}
    val$write <- write_summary_TDL_file(val, TDL.cycle, output.summary.TDL.fn);
  }
  if (sw$use.Licor ) {
    ##details<<
    ## Licor file. \code{\link{write_summary_Licor_file}}
    val$write <- write_summary_Licor_file(val, TDL.cycle, output.summary.Licor.fn);
  }

  ##details<<
  ## Summary Calculation file. \code{\link{write_summary_Calc_file}}
  val$write <- write_summary_Calc_file(val, TDL.cycle, output.summary.Calc.fn, output.summary.Calc.last.fn);

  if (sw$write.all.obs.file) {
      p.o <- paste("Write all observations file (big file -- takes a while)\n"); wWw <- write_progress(p.o, time.start);
    ##details<<
    ## All Calculation file. \code{\link{write_all_Calc_file}}
    val$write <- write_all_Calc_file(val, TDL.cycle, output.all.Calc.fn);
  }

  ##details<<
  ## plot all the calculated values. \code{\link{plot_val_calc_sum}}
    p.o <- paste("Plot mean values\n"); wWw <- write_progress(p.o, time.start);
  plot_val_calc_sum(val$sum$TDL, val$sum$Licor, val$const, val$calc$sum, plot.format.list, output.fn.prefix, sw);

  #-------------------
  ##details<<
  ## SECTION Bootstrap
  ##details<<
  ## BEGIN Bootstrap Calculations
  if (R.bootstrap > 0) {
      p.o <- paste("Begin Bootstrap\n"); wWw <- write_progress(p.o, time.start);

    ##details<<
    ## _ Generate NP and Par BS samples for observed values.

    val$bs            <- as.list(new.env());

    ##details<< . \code{\link{f_init_bs_matrix}}
    # init bs values to zero
    val$calc$bs <- f_init_bs_matrix(val$sum$TDL$n, R.bootstrap);

      p.o <- paste("Calculating the ",R.bootstrap," bootstrap iterates\n"); wWw <- write_progress(p.o, time.start);
    for (i.bs in 1:R.bootstrap) {
        if (i.bs == 1 ) { time.bs.01 <- progress_time(time.start); };
        if (i.bs == 21) { time.bs.21 <- progress_time(time.start);
                          p.o <- paste("  **  Average time per BS sample (based on first 20):", sprintf("%4.2f",(time.bs.21-time.bs.01)/20),"s \n"); wWw <- write_out(p.o);
        };
        p.o <- paste(i.bs," "); wWw <- write_out(p.o);
        if ((i.bs %% 20) == 0) {p.o <- paste("\n"); wWw <- write_out(p.o);};

      ##details<< . \code{\link{f_bs_iter_TDL_Licor}}
      # one BS resample of TDL and Licor means
      val.bs.sum <- f_bs_iter_TDL_Licor(val$obs$TDL, val$sum$TDL, val$obs$Licor, val$sum$Licor, var.interp);
      ##details<< . \code{\link{f_bs_save_TDL_Licor}}
      # save BS resample of TDL and Licor means
      val$bs <- f_bs_save_TDL_Licor(val.bs.sum, val$bs, i.bs, R.bootstrap, sw);
      ##details<< . \code{\link{f_val_calc_all_driver}}
      # This function calls all the calc files
      val.calc.bs.temp <- f_val_calc_all_driver(val.bs.sum$TDL, val.bs.sum$Licor, val$const, sw);
      ##details<< . \code{\link{f_val_bs_matrix}}
      # update values for each bs iterate
      val$calc$bs <- f_val_bs_matrix(val$calc$bs, val.calc.bs.temp, i.bs);
    }
      p.o <- paste("\n"); wWw <- write_out(p.o);
      p.o <- paste("Bootstrap complete\n"); wWw <- write_progress(p.o, time.start);

    ##details<<
    ## _ calculate central 95% intervals.
    ##details<<
    ## _ create CI for each calculated value. \code{\link{f_val_bs_CI}}
      p.o <- paste("Calculate the endpoints of the central ", sig.CI, "bootstrap CI\n"); wWw <- write_progress(p.o, time.start);
    val.CI <- f_val_bs_CI(val$calc$bs, val$bs$TDL, val$bs$Licor, R.bootstrap, sig.CI, sw);
      val$CI        <- as.list(new.env());
      val$CI$TDL    <- val.CI$TDL;
      val$CI$Licor  <- val.CI$Licor;
      val$calc$CI   <- val.CI$calc;
      rm(val.CI);

    ##details<<
    ## _ plot all variables with bs values, mean value, and CI intervals. \code{\link{f_plot_CI_individuals_driver}}
      p.o <- paste("Plot all variables with bs values, mean value, and CI intervals\n"); wWw <- write_progress(p.o, time.start);
    f_plot_CI_individuals_driver(val, R.bootstrap, plot.format.list, output.fn.prefix);

    ##details<<
    ## _ write_out central 95% interval, SD,
      p.o <- paste("Write central bootstrap confidence intervals\n"); wWw <- write_progress(p.o, time.start);
    ##details<<
    ## _ Summary files. \code{\link{write_CI_TDL_file}}
    if (sw$use.TDL ) {
      ##details<<
      ## _ TDL file
      val$write <- write_CI_TDL_file(val, TDL.cycle, output.CI.TDL.fn);
    }
    if (sw$use.Licor ) {
      ##details<<
      ## _ Licor file. \code{\link{write_CI_Licor_file}}
      val$write <- write_CI_Licor_file(val, TDL.cycle, output.CI.Licor.fn);
    }
    ##details<<
    ## _ Calculation file. \code{\link{write_CI_Calc_file}}
    val$write <- write_CI_Calc_file(val, TDL.cycle, output.CI.Calc.fn, output.CI.Calc.last.fn);

  } else {
    p.o <- paste("No bootstrap\n"); wWw <- write_progress(p.o, time.start);
  }
  ##details<<
  ## END

  ##details<<
  ## Save workspace.
  if (sw$save.RData) {
      R.workspace.fn <- paste(output.fn.prefix,".RData",sep="");
      p.o <- paste("Saving R workspace:", R.workspace.fn, "\n"); wWw <- write_progress(p.o, time.start);
    save(list = ls(all=TRUE), file = R.workspace.fn, version = NULL, ascii = FALSE, compress = TRUE, compression_level=9) #, safe = TRUE)
  }

  ##details<<
  ## SECTION Wrap it up.
  ## Move selected plot files into subdirectories. \code{\link{move_plot_files}}
    p.o <- paste("Move selected plot files to subdirectories\n"); wWw <- write_progress(p.o, time.start);
    move_plot_files(plot.format.list);

  #path.prefix <- paste(path, "/", path.output.fn.prefix, sep="");
    p.o <- paste("\n"); wWw <- write_out(p.o);
    p.o <- paste("Note: See output directory for results:", "\n"); wWw <- write_out(p.o);
    p.o <- paste("Output:", path.prefix, "\n"); wWw <- write_out(p.o);

    p.o <- paste("\n"); wWw <- write_out(p.o);
    p.o <- paste("Note: If there are WARNINGS below (except existing directories), please report to Erik by", "\n"); wWw <- write_out(p.o);
    p.o <- paste("        sending email with full console text above copied into the email body and", "\n"); wWw <- write_out(p.o);
    p.o <- paste("        creating folder in dropbox folder /Sharing with Erhardt/ and copying your data and template file there.", "\n\n"); wWw <- write_out(p.o);

    p.o <- paste("COMPLETE\n\n"); wWw <- write_progress(p.o, time.start);

  # rename outtemp directory to prefix name
  #file.rename(path.out,path.prefix);
  ##details<<
  ## Copy template file to out dir.
  file.copy(filename, paste(path.out, "/", input.fn, sep=""));
  ##details<<
  ## Change back to original dir.
  setwd(path.original);

  ##details<<
  ## Complete.

  return( NULL );
  ### NULL
}

