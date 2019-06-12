#' Reads TDL and Licor files, aligns them, calculates quantities of interest with bootstrap intervals.
#'
#' Edit \file{isogasex_template4.yaml} or \file{isogasex_template4.xls} and input parameters and TDL/Licor filenames.
#' Run \code{\link{isogasex}}.
#' See output in \file{./out} directory.
#'
#' SECTION Preamble
#' Set version.
#' Display header.
#'
#' Save original working directory, change to data dir.
#' (This is a little hokey.)  Change to data dir, then to out dir, then back to original at end.
#'
#' Create output directory.
#'
#' SECTION Read data.
#'
#' Input Excel workbook. \code{\link{read_template_param}}
#'
#' Assign variables. \code{\link{assign_param_variables}}
#'
#' Create output directory (prefix) and update the process_info.txt.
#'
#' Check that template version matches version of isogasex.
#'
#' Set random seed for samples.
#'
#' Read TDL and Licor data.  \code{\link{read_TDL}} and \code{\link{read_Licor}}
#'
#' SECTION TDL Plot and interpolate.
#'
#' BEGIN If using TDL:
#'
#' _ Print lines with missing values in TDL file and fix them. \code{\link{fix_missing_TDL_values}}
#'
#' _ Extract cycle timing, write to file. \code{\link{extract_cycle_timing}}
#'
#' _ Determine the last index for admissible values to calculate means. \code{\link{last_TDL_index_for_means}}
#'
#' _ Plot each cycle of TDL measurements (visual diagnostics). \code{\link{plot_data_cycles}}
#'
#' _ Calculate the mean and variance for the TDL data based on last measurements. \code{\link{calc_mean_TDL}}
#'
#' _ Interpolate TDL tank and reference values. \code{\link{interp_TDL_tanks_ref}}
#'
#' _ Summary values for TDL interpolated tanks and reference. \code{\link{calc_mean_TDL_interp_tanks_ref}}
#'
#' _ Calculate mean Variance for interp values for Par BS of tanks and reference.
#'
#' END
#'
#' SECTION Align TDL and Licor
#'
#' Determine overlapping time window of TDL and Licor measurements. \code{\link{align_TDL_Licor_times}}
#'
#' Reduce data to overlapping time window and interp Licor values to TDL timepoints. \code{\link{time_window_TDL_Licor_interp}}
#'
#' Plot each cycle of TDL measurements (visual diagnostics). \code{\link{plot_data_cycles}}
#'
#' Calculate the mean and variance for the Licor data based on last measurements. \code{\link{calc_mean_Licor}}
#'
#' SECTION Mean Calculations
#'
#' Assign TDL and Licor values to val variable names, raw and summarized values. \code{\link{val_TDL_Licor_variables}}
#'
#' summary values (mean). \code{\link{f_val_calc_all_driver}}
#'
#' All time point values. \code{\link{f_val_calc_all_driver}}
#'
#' SECTION Write out results
#'
#' Summary files
#'
#' TDL file. \code{\link{write_summary_TDL_file}}
#'
#' Licor file. \code{\link{write_summary_Licor_file}}
#'
#' Summary Calculation file. \code{\link{write_summary_Calc_file}}
#'
#' All Calculation file. \code{\link{write_all_Calc_file}}
#'
#' plot all the calculated values. \code{\link{plot_val_calc_sum}}
#'
#' SECTION Bootstrap
#'
#' BEGIN Bootstrap Calculations
#'
#' _ Generate NP and Par BS samples for observed values.
#'
#' . \code{\link{f_init_bs_matrix}}
#'
#' . \code{\link{f_bs_iter_TDL_Licor}}
#'
#' . \code{\link{f_bs_save_TDL_Licor}}
#'
#' . \code{\link{f_val_calc_all_driver}}
#'
#' . \code{\link{f_val_bs_matrix}}
#'
#' _ calculate central 95\% intervals.
#'
#' _ create CI for each calculated value. \code{\link{f_val_bs_CI}}
#'
#' _ plot all variables with bs values, mean value, and CI intervals. \code{\link{f_plot_CI_individuals_driver}}
#'
#' _ write out central 95\% interval, SD,
#'
#' _ Summary files. \code{\link{write_CI_TDL_file}}
#'
#' _ TDL file
#'
#' _ Licor file. \code{\link{write_CI_Licor_file}}
#'
#' _ Calculation file. \code{\link{write_CI_Calc_file}}
#'
#' END
#'
#' Save workspace.
#'
#' SECTION Wrap it up.
#' Move selected plot files into subdirectories. \code{\link{move_plot_files}}
#'
#' Copy template file to out dir.
#'
#' Change back to original dir.
#'
#' Complete.
#'
#' @param input_fn The parameter template file name.  A modified version of isogasex_template4.yaml (text) or isogasex_template4.xls (Excel worksheet)
#' @param path Directory path to input data files and the location where the ./out folder will be saved with all results.
#'
#' @return NULL invisibly, all results are saved as files in the ./out folder
#' @importFrom utils capture.output sessionInfo packageDescription
#'
#' @export
#'
isogasex <-
function(
  input_fn = c("isogasex_template4.yaml", "isogasex_template4.xls")
, path = getwd()
### Directory where TDL and Licor data are to be read from, and where ./out directory for results are to be written to.
)
{
  # DRIVER FUNCTION -------------------------------------------------------------
  # DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
  # #library(isogasex)
  # #path <- "C:/Dropbox/StatAcumen/consult/Authorship/DavidHanson_Isotopes_2009/Sharing with Erhardt/Daves problem July 19";
  # #input_fn <- "isogasex_template3.xls"
  # #setwd(path);
  # rm(list=ls()); # clear workspace
  # library(isogasex)
  # path <- "c:\\Dropbox\\StatAcumen\\consult\\Authorship\\DavidHanson_Isotopes_2009\\Sharing\ with\ Erhardt\\201207_Photoresp_conf_paper\\Pater2011round2data";
  # setwd(path);
  # input_fn <- "2011Pater_isogasex_05262011AT_PMDH201.xls"
  # #isogasex(input_fn)
  #
  # DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG

  # install required packages if not in R already
  #list_of_packages <- c("gdata", "zoo")
  #new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  #if(length(new_packages)) {
  #  install.packages(new_packages);
  #  if (.Platform$OS.type == "windows") {
  #    library(xlsReadWrite);  xls_getshlib();  # get shared library
  #  }
  #}

  ##details<<
  ## SECTION Preamble
  ## Set version.
  ## Display header.
  #isogasex_version  <- "0.1-23";
  #isogasex_date     <- "2014-04-08"; # yyyy-mm-dd

  isogasex_version  <- utils::packageDescription("isogasex")$Version
  isogasex_template <- 4;

  ##details<<
  ## Save original working directory, change to data dir.
  ## (This is a little hokey.)  Change to data dir, then to out dir, then back to original at end.
    path_original <- getwd(); # changed back at very end of isogasex()
  ##details<<
  ## Create output directory.
  setwd(path);

  p_o_temp <- NULL; # initial output buffer to print after we create prefix output directory

  time_start <- proc.time()[3];    # start timer

    p_o_temp <- rbind(p_o_temp, paste("isogasex: TDL/Licor processing", "\n\n", sep=""));
    #p_o_temp <- rbind(p_o_temp, paste("  Version: ", isogasex_version, ", Date: ", isogasex_date,", Template ", isogasex_template, "\n", sep=""));
    p_o_temp <- rbind(p_o_temp, paste("  Concept by David T. Hanson", "\n"));
    p_o_temp <- rbind(p_o_temp, paste("  Written by Erik B. Erhardt", "\n"));
    p_o_temp <- rbind(p_o_temp, paste("  Professors at the University of New Mexico", "\n\n"));
    p_o_temp <- rbind(p_o_temp, paste("Starting ", Sys.time(), "\n\n"));

    p_o_temp <- rbind(p_o_temp, paste("\n\n"));
    p_o_temp <- rbind(p_o_temp, paste("================================================================================\n"));
    p_o_temp <- rbind(p_o_temp, paste("SESSION INFO AND PACKAGE VERSION ---- BEGIN ------------------------------------\n"));
    p_o_temp <- rbind(p_o_temp, paste("\n\n"));
    p_o_temp <- rbind(p_o_temp, as.matrix(paste0(utils::capture.output(utils::sessionInfo()), "\n"), ncol=1));
    p_o_temp <- rbind(p_o_temp, paste("\n\n"));
    p_o_temp <- rbind(p_o_temp, as.matrix(paste0(utils::capture.output(utils::packageDescription("isogasex")), "\n"), ncol=1));
    p_o_temp <- rbind(p_o_temp, paste("\n\n"));
    p_o_temp <- rbind(p_o_temp, paste("SESSION INFO AND PACKAGE VERSION ---- END   ------------------------------------\n"));
    p_o_temp <- rbind(p_o_temp, paste("================================================================================\n"));
    p_o_temp <- rbind(p_o_temp, paste("\n\n"));

    print(isogasex_logo())

  # convert date/time from character to POSIX
    options(digits_secs=1); # set resolution of seconds in time to 0.1



  ## SECTION Read template input parameters.

  ## Input Excel workbook. \code{\link{read_template_param}}


  param_fn <- paste(path, "/", input_fn, sep="");
    #p_o = paste("Reading workbook: ", input_fn, "\n"); wWw <- write_progress(p_o, time_start);
    p_o_temp <- rbind(p_o_temp, paste("Reading template parameters from: ", input_fn, "\n"));
  PARAM_RAW <- read_template_param(param_fn)

  ##details<<
  ## Assign variables. \code{\link{assign_param_variables}}
    #p_o = paste("Assign variables", "\n"); wWw <- write_progress(p_o, time_start);
    p_o_temp <- rbind(p_o_temp, paste("Assign parameter to variables", "\n"));
  PARAM <- assign_param_variables(PARAM_RAW, path)
  val                             <- PARAM$val                          ;
  sw                              <- PARAM$sw                           ;
  TDL_fn                          <- PARAM$TDL_fn                       ;
  Licor_fn                        <- PARAM$Licor_fn                     ;
  plot_format_list                <- PARAM$plot_format_list             ;
  output_fn_prefix                <- PARAM$output_fn_prefix             ;
  output_summary_TDL_fn           <- PARAM$output_summary_TDL_fn        ;
  output_summary_Licor_fn         <- PARAM$output_summary_Licor_fn      ;
  output_summary_Calc_fn          <- PARAM$output_summary_Calc_fn       ;
  output_summary_Calc_last_fn     <- PARAM$output_summary_Calc_last_fn  ;
  output_all_Calc_fn              <- PARAM$output_all_Calc_fn           ; # "0.1-16" "2012-07-11"
  output_CI_TDL_fn                <- PARAM$output_CI_TDL_fn             ;
  output_CI_Licor_fn              <- PARAM$output_CI_Licor_fn           ;
  output_CI_Calc_fn               <- PARAM$output_CI_Calc_fn            ;
  output_CI_Calc_last_fn          <- PARAM$output_CI_Calc_last_fn       ;
  R_bootstrap                     <- PARAM$R_bootstrap                  ;
  sig_CI                          <- PARAM$sig_CI                       ;
  seed                            <- PARAM$seed                         ;
  TDL_cycle                       <- PARAM$TDL_cycle                    ;
  Licor_TDL_time_offset_seconds   <- PARAM$Licor_TDL_time_offset_seconds;
  rm(PARAM);

  ##details<<
  ## Create output directory (prefix) and update the process_info.txt.
  path_output_fn_prefix <- paste("out_",output_fn_prefix,sep="");
  path_prefix <- paste(path, "/", path_output_fn_prefix, sep="");
  #path_out <- paste(path, "/outtemp", sep="");
  path_out <- path_prefix;
  if (!file.exists(path_output_fn_prefix)) {
    dir.create(path_out);
  }
  setwd(path_out);

  process_filename <- "process_info.txt";  # hardcoded in write_out() and write_progress()
  if (!is.na(file.info(process_filename)$size)) {
    file.remove(process_filename);
  }; # delete old process_info file
  write_out(p_o_temp); # write log so far

  ##details<<
  ## Check that template version matches version of isogasex.
  if (sw$template_version != isogasex_template) {
    error.message <- paste("ERROR: Template version mismatch! \n",
                           "       Template version of ", input_fn, " is ", sw$template_version, ". isogasex version ", isogasex_version, "requires template ", isogasex_template, " \n", sep="");
    stop(error.message);
  };

  ##details<<
  ## Set random seed for samples.
  if (seed == 0 | !is.numeric(seed)) { seed <- round(as.numeric(Sys.time())); };
    p_o = paste("Set random seed =", seed, "\n"); wWw <- write_progress(p_o, time_start);
  set.seed(seed);

  ##details<<
  ## Read TDL and Licor data.  \code{\link{read_TDL}} and \code{\link{read_Licor}}
    p_o <- paste("Reading TDL and/or Licor data \n"); wWw <- write_progress(p_o, time_start);
    p_o <- paste("Reading TDL data \n"); wWw <- write_progress(p_o, time_start);
  TDL   <- read_TDL(TDL_fn, sw); # read_TDL file,   reference with TDL$[col.name]

    p_o <- paste("Reading Licor data \n"); wWw <- write_progress(p_o, time_start);
  Licor <- read_Licor(Licor_fn, Licor_TDL_time_offset_seconds, sw); # read_Licor file, reference with Licor$[col.name]

  ##details<<
  ## SECTION TDL Plot and interpolate.
  ##details<<
  ## BEGIN If using TDL:
  if (sw$use_TDL  ) {

    if (sw$print_TDL_missing){
      ##details<<
      ## _ Print lines with missing values in TDL file and fix them. \code{\link{fix_missing_TDL_values}}
      bad_ind <- TDL$ind[TDL$ind*ceiling(apply(is.na(TDL$data), MARGIN=1, mean))];
      if (length(bad_ind) > 0) {
        p_o <- paste("Printing", length(bad_ind), "lines with missing values in TDL file (a few are normal, CHECK THAT NOT TOO MANY ARE CLOSE IN TIME)\n"); wWw <- write_progress(p_o, time_start);
        TDL_missing <- TDL$data[bad_ind,];
        p_o <- TDL_missing; wWw <- write_progress(p_o, time_start, type_print = "matrix");
        rm(TDL_missing);
        p_o <- paste("  Note: Replacing bad TDL values in ", length(bad_ind), "lines with previous value in the file\n"); wWw <- write_progress(p_o, time_start);
        TDL <- fix_missing_TDL_values(TDL, bad_ind);
      }
      rm(bad_ind);
    }

    if (sw$print_TDL_cycle_timing){
      ##details<<
      ## _ Extract cycle timing, write to file. \code{\link{extract_cycle_timing}}
      TDL_site_timing_filename <- paste(output_fn_prefix,"_TDL_site_timing.csv",sep="");
        p_o <- paste("Extract cycle timing, write to file: ", TDL_site_timing_filename, "\n"); wWw <- write_progress(p_o, time_start);
        p_o <- paste("  (takes a LONG time)", "\n"); write_out(p_o);
      extract_cycle_timing(TDL, TDL_cycle, TDL_site_timing_filename); # usually not run because it takes a long time
    }

    ##details<<
    ## _ Determine the last index for admissible values to calculate means. \code{\link{last_TDL_index_for_means}}
      p_o <- paste("Determine the last index for admissible values to calculate means\n"); wWw <- write_progress(p_o, time_start);
    TDL <- last_TDL_index_for_means(TDL, TDL_cycle); # creates TDL$last_list

    ##details<<
    ## _ Plot each cycle of TDL measurements (visual diagnostics). \code{\link{plot_data_cycles}}
      p_o <- paste("Plot data cycles for full TDL\n"); wWw <- write_progress(p_o, time_start);
      # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
    plot_data_cycles(TDL, TDL_cycle, plot_format_list, output_fn_prefix, val$const$Rstd_13C, "full");

    ##details<<
    ## _ Calculate the mean and variance for the TDL data based on last measurements. \code{\link{calc_mean_TDL}}
      p_o <- paste("Calculate the mean and variance for the TDL data based on last measurements\n"); wWw <- write_progress(p_o, time_start);
    TDL <- calc_mean_TDL(TDL, TDL_cycle);

    ##details<<
    ## _ Interpolate TDL tank and reference values. \code{\link{interp_TDL_tanks_ref}}
      p_o <- paste("Interpolate TDL tank and reference values\n"); wWw <- write_progress(p_o, time_start);
    TDL <- interp_TDL_tanks_ref(TDL, TDL_cycle, plot_format_list, output_fn_prefix, sw)

    ##details<<
    ## _ Summary values for TDL interpolated tanks and reference. \code{\link{calc_mean_TDL_interp_tanks_ref}}
      p_o <- paste("Calculate the mean and variance for the interpolated TDL tank and reference values\n"); wWw <- write_progress(p_o, time_start);
    TDL <- calc_mean_TDL_interp_tanks_ref(TDL, TDL_cycle);

    ##details<<
    ## _ Calculate mean Variance for interp values for Par BS of tanks and reference.
    var_interp <- list();
        ind_tank_hi           <- (TDL$summary$site == TDL_cycle$number_tank_hi);
      var_interp$tank_hi_12   <- mean(TDL$summary$var[ind_tank_hi,"interp_tank_hi_12"]);
      var_interp$tank_hi_13   <- mean(TDL$summary$var[ind_tank_hi,"interp_tank_hi_13"]);
        ind_tank_low          <- (TDL$summary$site == TDL_cycle$number_tank_low);
      var_interp$tank_low_12  <- mean(TDL$summary$var[ind_tank_low,"interp_tank_low_12"]);
      var_interp$tank_low_13  <- mean(TDL$summary$var[ind_tank_low,"interp_tank_low_13"]);
        ind_reference         <- (TDL$summary$site == TDL_cycle$number_reference);
      var_interp$reference_12 <- mean(TDL$summary$var[ind_reference,"interp_reference_12"]);
      var_interp$reference_13 <- mean(TDL$summary$var[ind_reference,"interp_reference_13"]);
  }; # if sw$use_TDL
  ##details<<
  ## END

  ##details<<
  ## SECTION Align TDL and Licor
  ##details<<
  ## Determine overlapping time window of TDL and Licor measurements. \code{\link{align_TDL_Licor_times}}
    p_o <- paste("Determine overlapping times for TDL, Licor, and Time Window \n"); wWw <- write_progress(p_o, time_start);
  # 7/19/2010 need to test for TDL or Licor only
  TDL_Licor_times <- align_TDL_Licor_times(TDL$time, TDL$n, Licor$time, Licor$n, val$timewindow, sw);

  ##details<<
  ## Reduce data to overlapping time window and interp Licor values to TDL timepoints. \code{\link{time_window_TDL_Licor_interp}}
    p_o <- paste("Reduce data to overlapping time window and interp Licor values to TDL timepoints \n"); wWw <- write_progress(p_o, time_start);
  TDL_and_Licor_interp   <- time_window_TDL_Licor_interp(TDL, Licor, TDL_Licor_times, sw);
    TDL           <- TDL_and_Licor_interp$TDL;           # TDL data
    Licor         <- TDL_and_Licor_interp$Licor_interp;  # Licor interp data
    rm(TDL_and_Licor_interp);

  ##details<<
  ## Plot each cycle of TDL measurements (visual diagnostics). \code{\link{plot_data_cycles}}
    p_o <- paste("Plot data cycles for TDL/Licor overlap window\n"); wWw <- write_progress(p_o, time_start);
    # NOTE, THE lm() does not recognize the value after the ~ if run driver script with source(*.R), but works if copy lines by line.
  plot_data_cycles(TDL, TDL_cycle, plot_format_list, output_fn_prefix, val$const$Rstd_13C, "window");

  if (sw$use_Licor ) {
    ##details<<
    ## Calculate the mean and variance for the Licor data based on last measurements. \code{\link{calc_mean_Licor}}
      p_o <- paste("Calculate the mean and variance for the TDL/Licor data based on last measurements\n"); wWw <- write_progress(p_o, time_start);
    Licor <- calc_mean_Licor(TDL, Licor, TDL_cycle, sw);
  };


  #-------------------
  ## SECTION Mean Calculations

  ## Assign TDL and Licor values to val variable names, raw and summarized values. \code{\link{val_TDL_Licor_variables}}
    p_o <- paste("Assign TDL and Licor values to val variable names, raw and summarized values\n"); wWw <- write_progress(p_o, time_start);
  val <- val_TDL_Licor_variables(TDL, Licor, TDL_cycle, val, sw);

  ## summary values (mean). \code{\link{f_val_calc_all_driver}}
    p_o <- paste("Calculate mean values\n"); wWw <- write_progress(p_o, time_start);
  val$calc$sum <- f_val_calc_all_driver(val$sum$TDL, val$sum$Licor, val$const, sw);

  # do val$calc$all "0.1-16" "2012-07-10"
  ## All time point values. \code{\link{f_val_calc_all_driver}}
    p_o <- paste("Calculate all values\n"); wWw <- write_progress(p_o, time_start);
  val$calc$all <- f_val_calc_all_driver(val$obs$TDL, val$obs$Licor, val$const, sw);
  val$calc$all$site <- val$obs$TDL$PrevSite;
  val$calc$all$time <- TDL$time;  # strptime(paste(val$obs$TDL$TIMESTAMP), "%Y-%m-%d %H:%M:%OS"); #, tz=Sys.timezone());
  val$calc$all$n    <- TDL$n;
  val$calc$all$ind  <- 1:val$calc$all$n;


  #-------------------
  ## SECTION Write out results
    p_o <- paste("Write mean summary files\n"); wWw <- write_progress(p_o, time_start);
  ## Summary files
  if (sw$use_TDL ) {
    ## TDL file. \code{\link{write_summary_TDL_file}}
    val$write <- write_summary_TDL_file(val, TDL_cycle, output_summary_TDL_fn);
  }
  if (sw$use_Licor ) {
    ## Licor file. \code{\link{write_summary_Licor_file}}
    val$write <- write_summary_Licor_file(val, TDL_cycle, output_summary_Licor_fn);
  }

  ## Summary Calculation file. \code{\link{write_summary_Calc_file}}
  val$write <- write_summary_Calc_file(val, TDL_cycle, output_summary_Calc_fn, output_summary_Calc_last_fn);

  if (sw$write_all_obs_file) {
      p_o <- paste("Write all observations file (big file -- takes a while)\n"); wWw <- write_progress(p_o, time_start);
    ## All Calculation file. \code{\link{write_all_Calc_file}}
    val$write <- write_all_Calc_file(val, TDL_cycle, output_all_Calc_fn);
  }

  ## plot all the calculated values. \code{\link{plot_val_calc_sum}}
    p_o <- paste("Plot mean values\n"); wWw <- write_progress(p_o, time_start);
  plot_val_calc_sum(val$sum$TDL, val$sum$Licor, val$const, val$calc$sum, plot_format_list, output_fn_prefix, sw);

  #-------------------
  ## SECTION Bootstrap
  ## BEGIN Bootstrap Calculations
  if (R_bootstrap > 0) {
      p_o <- paste("Begin Bootstrap\n"); wWw <- write_progress(p_o, time_start);

    ## _ Generate NP and Par BS samples for observed values.

    val$bs            <- list();

    ## . \code{\link{f_init_bs_matrix}}
    # init bs values to zero
    val$calc$bs <- f_init_bs_matrix(val$sum$TDL$n, R_bootstrap);

      p_o <- paste("Calculating the ",R_bootstrap," bootstrap iterates\n"); wWw <- write_progress(p_o, time_start);
    for (i_bs in 1:R_bootstrap) {
        if (i_bs == 1 ) { time.bs.01 <- progress_time(time_start); };
        if (i_bs == 21) { time.bs.21 <- progress_time(time_start);
                          p_o <- paste("  **  Average time per BS sample (based on first 20):", sprintf("%4.2f",(time.bs.21-time.bs.01)/20),"s \n"); write_out(p_o);
        };
        p_o <- paste(i_bs," "); write_out(p_o);
        if ((i_bs %% 20) == 0) {p_o <- paste("\n"); write_out(p_o);};

      ## . \code{\link{f_bs_iter_TDL_Licor}}
      # one BS resample of TDL and Licor means
      val_bs_sum <- f_bs_iter_TDL_Licor(val$obs$TDL, val$sum$TDL, val$obs$Licor, val$sum$Licor, var_interp);
      ##. \code{\link{f_bs_save_TDL_Licor}}
      # save BS resample of TDL and Licor means
      val$bs <- f_bs_save_TDL_Licor(val_bs_sum, val$bs, i_bs, R_bootstrap, sw);
      ##. \code{\link{f_val_calc_all_driver}}
      # This function calls all the calc files
      val_calc_bs_temp <- f_val_calc_all_driver(val_bs_sum$TDL, val_bs_sum$Licor, val$const, sw);
      ##. \code{\link{f_val_bs_matrix}}
      # update values for each bs iterate
      val$calc$bs <- f_val_bs_matrix(val$calc$bs, val_calc_bs_temp, i_bs);
    }
      p_o <- paste("\n"); write_out(p_o);
      p_o <- paste("Bootstrap complete\n"); wWw <- write_progress(p_o, time_start);

    ## _ calculate central 95% intervals.
    ## _ create CI for each calculated value. \code{\link{f_val_bs_CI}}
      p_o <- paste("Calculate the endpoints of the central ", sig_CI, "bootstrap CI\n"); wWw <- write_progress(p_o, time_start);
    val_CI <- f_val_bs_CI(val$calc$bs, val$bs$TDL, val$bs$Licor, R_bootstrap, sig_CI, sw);
      val$CI        <- list();
      val$CI$TDL    <- val_CI$TDL;
      val$CI$Licor  <- val_CI$Licor;
      val$calc$CI   <- val_CI$calc;
      rm(val_CI);

    ## _ plot all variables with bs values, mean value, and CI intervals. \code{\link{f_plot_CI_individuals_driver}}
      p_o <- paste("Plot all variables with bs values, mean value, and CI intervals\n"); wWw <- write_progress(p_o, time_start);
    f_plot_CI_individuals_driver(val, R_bootstrap, plot_format_list, output_fn_prefix);

    ## _ write_out central 95% interval, SD,
      p_o <- paste("Write central bootstrap confidence intervals\n"); wWw <- write_progress(p_o, time_start);
    ## _ Summary files. \code{\link{write_CI_TDL_file}}
    if (sw$use_TDL ) {
      ## _ TDL file
      val$write <- write_CI_TDL_file(val, TDL_cycle, output_CI_TDL_fn);
    }
    if (sw$use_Licor ) {
      ## _ Licor file. \code{\link{write_CI_Licor_file}}
      val$write <- write_CI_Licor_file(val, TDL_cycle, output_CI_Licor_fn);
    }
    ## _ Calculation file. \code{\link{write_CI_Calc_file}}
    val$write <- write_CI_Calc_file(val, TDL_cycle, output_CI_Calc_fn, output_CI_Calc_last_fn);

  } else {
    p_o <- paste("No bootstrap\n"); wWw <- write_progress(p_o, time_start);
  }
  ## END

  ## Save workspace.
  if (sw$save_RData) {
      R_workspace_fn <- paste(output_fn_prefix,".RData",sep="");
      p_o <- paste("Saving R workspace:", R_workspace_fn, "\n"); wWw <- write_progress(p_o, time_start);
    save(list = ls(all.names = TRUE), file = R_workspace_fn, version = NULL, ascii = FALSE, compress = TRUE, compression_level=9) #, safe = TRUE)
  }

  ## SECTION Wrap it up.
  ## Move selected plot files into subdirectories. \code{\link{move_plot_files}}
    p_o <- paste("Move selected plot files to subdirectories\n"); wWw <- write_progress(p_o, time_start);
    move_plot_files(plot_format_list);

  #path_prefix <- paste(path, "/", path_output_fn_prefix, sep="");
    p_o <- paste("\n"); write_out(p_o);
    p_o <- paste("Note: See output directory for results:", "\n"); write_out(p_o);
    p_o <- paste("Output:", path_prefix, "\n"); write_out(p_o);

    p_o <- paste("\n"); write_out(p_o);
    #p_o <- paste("Note: If there are WARNINGS below (except existing directories), please report to Erik by", "\n"); write_out(p_o);
    #p_o <- paste("        sending email with full console text above copied into the email body and", "\n"); write_out(p_o);
    #p_o <- paste("        creating folder in dropbox folder /Sharing with Erhardt/ and copying your data and template file there.", "\n\n"); write_out(p_o);
    p_o <- paste("Note: If there are WARNINGS below (except existing directories), please report to Erik", "\n"); write_out(p_o);

    p_o <- paste("COMPLETE\n\n"); wWw <- write_progress(p_o, time_start);

  # rename outtemp directory to prefix name
  #file.rename(path_out,path_prefix);
  ## Copy template file to out dir.
  file.copy(param_fn, paste(path_out, "/", input_fn, sep=""));
  ## Change back to original dir.
  setwd(path_original);

  ## Complete.

  invisible( NULL );
  ### NULL
}

