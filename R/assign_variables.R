#' Assign variables from input file to variable names
#'
#' initialize lists to hold all data/output
#'
#' Template version
#'
#' switches to use and filenames for TDL and Licor files
#'
#' Prefix for output filenames
#'
#' TIME WINDOW SECTION
#'
#' OUTPUT SECTION
#'
#' plot formats
#'
#' interpolation:  1 = smooth_spline() like tanks
#' 2 = use last reference mean until next measurement (horizontal interp) - for abrupt ref gas changes
#'
#' Bootstrap
#'
#' TDL sampling information
#'
#' CONSTANTS SECTION
#'
#' True calibration tank values
#'
#' Which values to use
#'
#' constants in formulas not given names
#'
#' Calculation information
#'
#' create a list to all returned parameters
#'
#' @param D
#' @param path
#'
#' @return VARIABLES list of all inputs from Excel template
#'
#' @examples
assign_variables <-
function# Assign variables from input file to variable names
###
(D
###
, path
###
)
{
  ################################################################################
  # Assign variables from input file to variable names
  #   When making changes to the workbook, need to:
  #     1. get_data.R define sheet names
  #     2. assign_variables.R define variables
  ## DEBUG
  # D <- DATA

  ##details<<
  ## initialize lists to hold all data/output
  sw  <- as.list(new.env());  # create a list for option switches
  # Conventions, ultimately put everything in val
  val <- as.list(new.env());

  # Constants   prefix: val$const$*
  val$const     <- as.list(new.env());
  ## Results (output) are in val$calc variables
  # Calculated  prefix: val$calc$*
  val$calc      <- as.list(new.env());
  val$calc$obs  <- as.list(new.env()); # for calculated observed values to use in NP bootstrap
  val$calc$all  <- as.list(new.env()); # for all calculated values in time window "0.1-16" "2012-07-11"
  val$calc$sum  <- as.list(new.env()); # for calculated summarized (mean) values to use as estimates
  val$write     <- as.list(new.env()); # to create tables to write_out to files

  # column 2 of the spreadsheet has the values
  c_v <- 2;

  # first row is header, then count from there
  r_v <- 0;

  ##details<<
  ## Template version
  r_v<-r_v+1; sw$template_version <- as.numeric(D[r_v,c_v]); # Version of template must match isogasex
  r_v<-r_v+1; # blank spreadsheet line

  # Parameter
  ##details<<
  ## switches to use and filenames for TDL and Licor files
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; sw$use_TDL   <- as.numeric(D[r_v,c_v]); # 1 = process TDL   data, 0 = do not use TDL   data
  r_v<-r_v+1; TDL_fn   <- paste(path, "/", as.character(D[r_v,c_v]), sep="");
  r_v<-r_v+1; sw$use_Licor <- as.numeric(D[r_v,c_v]); # 1 = process Licor data, 0 = do not use Licor data
  r_v<-r_v+1; Licor_fn <- paste(path, "/", as.character(D[r_v,c_v]), sep="");
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  ##details<<
  ## Prefix for output filenames
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; output_fn_prefix <- as.character(D[r_v,c_v]); # this prefix will appear on all output files



  ##details<<
  ## TIME WINDOW SECTION
  r_v<-r_v+1; # blank spreadsheet line
                                                            # two rows at a time
  r_v<-r_v+1; val$timewindow$start_time   <- strptime(paste(D[r_v,c_v],D[r_v+1,c_v]), "%m/%d/%y %H:%M:%S");
  r_v<-r_v+1; # row in start_time
  r_v<-r_v+1; val$timewindow$end_time     <- strptime(paste(D[r_v,c_v],D[r_v+1,c_v]), "%m/%d/%y %H:%M:%S");
  r_v<-r_v+1; # row in end_time


  ##details<<
  ## OUTPUT SECTION
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; sw$print_TDL_missing       <- as.numeric(D[r_v,c_v]); # high tank
  r_v<-r_v+1; sw$print_TDL_cycle_timing  <- as.numeric(D[r_v,c_v]); # low tank
  r_v<-r_v+1; sw$write_all_obs_file      <- as.numeric(D[r_v,c_v]); # leaf reference
  r_v<-r_v+1; sw$save_RData              <- as.numeric(D[r_v,c_v]); # leaf reference
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line


  ##details<<
  ## plot formats
  r_v<-r_v+1; # blank spreadsheet line
  plot_format_list <- NULL;  # 1=png, 2=eps, 3=pdf, 4=bmp, 5=jpeg, 6=tiff
  r_v<-r_v+1; if ( as.numeric(D[r_v,c_v]) ) { plot_format_list <- c(plot_format_list, 1); };  # png
  r_v<-r_v+1; if ( as.numeric(D[r_v,c_v]) ) { plot_format_list <- c(plot_format_list, 2); };  # eps
  r_v<-r_v+1; if ( as.numeric(D[r_v,c_v]) ) { plot_format_list <- c(plot_format_list, 3); };  # pdf
  r_v<-r_v+1; if ( as.numeric(D[r_v,c_v]) ) { plot_format_list <- c(plot_format_list, 4); };  # bmp
  r_v<-r_v+1; if ( as.numeric(D[r_v,c_v]) ) { plot_format_list <- c(plot_format_list, 5); };  # jpeg
  r_v<-r_v+1; if ( as.numeric(D[r_v,c_v]) ) { plot_format_list <- c(plot_format_list, 6); };  # tiff

  #if (sw$use_TDL  ) { output_summary_TDL_fn       <- paste(output_fn_prefix, "_summary_TDL.csv",        sep=""); };
  #if (sw$use_Licor) { output_summary_Licor_fn     <- paste(output_fn_prefix, "_summary_Licor.csv",      sep=""); };
                      output_summary_TDL_fn       <- paste(output_fn_prefix, "_summary_TDL.csv",        sep="");
                      output_summary_Licor_fn     <- paste(output_fn_prefix, "_summary_Licor.csv",      sep="");
                      output_summary_Calc_fn      <- paste(output_fn_prefix, "_summary_Calc.csv",       sep="");
                      output_summary_Calc_last_fn <- paste(output_fn_prefix, "_summary_Calc_last.csv",  sep="");
                      output_all_Calc_fn          <- paste(output_fn_prefix, "_all_Calc.csv",           sep=""); # "0.1-16" "2012-07-11"
                      output_CI_TDL_fn            <- paste(output_fn_prefix, "_CI_TDL.csv",             sep="");
                      output_CI_Licor_fn          <- paste(output_fn_prefix, "_CI_Licor.csv",           sep="");
                      output_CI_Calc_fn           <- paste(output_fn_prefix, "_CI_Calc.csv",            sep="");
                      output_CI_Calc_last_fn      <- paste(output_fn_prefix, "_CI_Calc_last.csv",       sep="");

  ##details<<
  ## interpolation:  1 = smooth.spline() like tanks
  ##                 2 = use last reference mean until next measurement (horizontal interp) - for abrupt ref gas changes
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; sw$interp_tank_hi    <- as.numeric(D[r_v,c_v]); # high tank
  r_v<-r_v+1; sw$interp_tank_low   <- as.numeric(D[r_v,c_v]); # low tank
  r_v<-r_v+1; sw$interp_reference  <- as.numeric(D[r_v,c_v]); # leaf reference


  ##details<<
  ## Bootstrap
  r_v<-r_v+1; # blank spreadsheet line

  r_v<-r_v+1; R_bootstrap <- as.numeric(D[r_v,c_v]); # Number of bootstrap samples to assess uncertainty of estimates
  r_v<-r_v+1; sig_CI      <- as.numeric(D[r_v,c_v]); # Confidence level for central confidence interval
  r_v<-r_v+1; seed        <- as.numeric(D[r_v,c_v]); # pseudorandom number seed, 0 is random based on clock, set to integer for results to be repeatable

  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  ##details<<
  ## TDL sampling information
  TDL_cycle <- as.list(new.env());  # create a list to return with data
          # number.* is TDL field "PrevSite"
  r_v<-r_v+1; TDL_cycle$number_tank_hi    <- as.numeric(D[r_v,c_v]);   # 9; # high tank
  r_v<-r_v+1; TDL_cycle$number_tank_low   <- as.numeric(D[r_v,c_v]);   #10; # low tank
  r_v<-r_v+1; TDL_cycle$number_reference  <- as.numeric(D[r_v,c_v]);   # 1; # leaf reference
  r_v<-r_v+1; TDL_cycle$number_chamber    <- as.numeric(D[r_v,c_v]);   #21; # leaf chamber
  r_v<-r_v+1; TDL_cycle$name_tank_hi      <- as.character(D[r_v,c_v]); #"tank_hi";   # high tank
  r_v<-r_v+1; TDL_cycle$name_tank_low     <- as.character(D[r_v,c_v]); #"tank_low";  # low tank
  r_v<-r_v+1; TDL_cycle$name_reference    <- as.character(D[r_v,c_v]); #"reference"; # leaf reference
  r_v<-r_v+1; TDL_cycle$name_chamber      <- as.character(D[r_v,c_v]); #"chamber";   # leaf chamber

  r_v<-r_v+1; # blank spreadsheet line

          # seconds is time length of measurements to use as data
  r_v<-r_v+1; TDL_cycle$seconds_tank_hi   <- as.numeric(D[r_v,c_v]); # high tank
  r_v<-r_v+1; TDL_cycle$seconds_tank_low  <- as.numeric(D[r_v,c_v]); # low tank
  r_v<-r_v+1; TDL_cycle$seconds_reference <- as.numeric(D[r_v,c_v]); # leaf reference
  r_v<-r_v+1; TDL_cycle$seconds_chamber   <- as.numeric(D[r_v,c_v]); # leaf chamber
  r_v<-r_v+1; TDL_cycle$Hz                <- as.numeric(D[r_v,c_v]); # sampling rate, output of TDL
  r_v<-r_v+1; TDL_cycle$seconds_exclude_first_chamber <- as.numeric(D[r_v,c_v]); # time it takes to equilbrate from reference to leaf chamber
  TDL_cycle <- set_TDL_cycle(TDL_cycle); # TDL_cycle variables
 #if (sw$use_TDL  ) {
 #  # TDL sampling information
 #  TDL_cycle <- as.list(new.env());  # create a list to return with data
 #          # number.* is TDL field "PrevSite"
 #  TDL_cycle$number_tank_hi    <-  9; # high tank
 #  TDL_cycle$number_tank_low   <- 10; # low tank
 #  TDL_cycle$number_reference  <-  1; # leaf reference
 #  TDL_cycle$number_chamber    <- 21; # leaf chamber
 #  TDL_cycle$name_tank_hi      <- "tank_hi"; # high tank
 #  TDL_cycle$name_tank_low     <- "tank_low"; # low tank
 #  TDL_cycle$name_reference    <- "reference"; # leaf reference
 #  TDL_cycle$name_chamber      <- "chamber"; # leaf chamber
 #          # seconds is time length of measurements to use as data
 #  TDL_cycle$seconds_tank_hi   <- 5; # high tank
 #  TDL_cycle$seconds_tank_low  <- 5; # low tank
 #  TDL_cycle$seconds_reference <- 5; # leaf reference
 #  TDL_cycle$seconds_chamber   <- 5; # leaf chamber
 #  TDL_cycle$Hz                <- 10; # sampling rate, output of TDL
 #  TDL_cycle$seconds_exclude_first_chamber <- 20; # time it takes to equilbrate from reference to leaf chamber
 #  TDL_cycle <- set_TDL_cycle(TDL_cycle); # TDL_cycle variables
 #};

  r_v<-r_v+1; # blank spreadsheet line

  # If using Licor file
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; Licor_TDL_time_offset_seconds <- as.numeric(D[r_v,c_v]);
                                        # positive indicates Licor is ahead of TDL
                                        # (e.g., 16 has Licor = 12:00:00 matches TDL = 12:00:16)
                                        # (that is, gas takes 16 seconds to get from Licor to TDL)
 #if (sw$use_Licor) {
 #  Licor_TDL_time_offset_seconds <- 0; # positive indicates Licor is ahead of TDL
 #                                      # (e.g., 16 has Licor = 12:00:00 matches TDL = 12:00:16)
 #                                      # (that is, gas takes 16 seconds to get from Licor to TDL)
 #};

  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  ##details<<
  ## CONSTANTS SECTION
  ##details<<
  ## True calibration tank values
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; val$const$true_value_hi_12C             <- as.numeric(D[r_v,c_v]); # example tank values
  r_v<-r_v+1; val$const$true_value_hi_13C             <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; val$const$true_value_lo_12C             <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; val$const$true_value_lo_13C             <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; val$const$fo_13C                        <- as.numeric(D[r_v,c_v]); # 0.00474;  # if only 12 and 13CO2 are measured, fo means fraction of other isotopologues not measured
  r_v<-r_v+1; val$const$fo_18O                        <- as.numeric(D[r_v,c_v]); # if 12 and 13CO2, and 12C18O16O are measured (not ususal for Dave's machine)
  r_v<-r_v+1; val$const$Rstd_13C                      <- as.numeric(D[r_v,c_v]); # (Rvpdb)
  r_v<-r_v+1; val$const$Rstd_18O                      <- as.numeric(D[r_v,c_v]); # (Rvsmow) may occaisionally be used with Dave's machine, method not tested yet
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; val$const$Gamma_star                    <- as.numeric(D[r_v,c_v]); # CO2 compensation point in absence of day respiration.  I will need to specify experiment specific values for these constants in the equations below
  r_v<-r_v+1; val$const$f_photo_respiration           <- as.numeric(D[r_v,c_v]); # fractionation associated with photorespiration
  r_v<-r_v+1; val$const$e                             <- as.numeric(D[r_v,c_v]); # fractionation associated with day respiration
  r_v<-r_v+1; val$const$Rd                            <- as.numeric(D[r_v,c_v]); # day respiration
  r_v<-r_v+1; val$const$k                             <- as.numeric(D[r_v,c_v]); # carboxylation efficiency, often initial slope of A/Ci, or better A/Cc
  r_v<-r_v+1; val$const$b_gm                          <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; val$const$b_modeling                    <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; val$const$b_s                           <- as.numeric(D[r_v,c_v]); #  per mil fractionation as CO2 enters solution at 25C bs =  1.1
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; val$const$a                             <- as.numeric(D[r_v,c_v]); #  per mil fractionation in still air (stomatal pore and substomata) a1 =  0.7
  r_v<-r_v+1; val$const$a_b                           <- as.numeric(D[r_v,c_v]); #  per mil fractionation in boundary layer (slightly mixed)  a = 4.44
  r_v<-r_v+1; val$const$a_l                           <- as.numeric(D[r_v,c_v]); #  per mil fractionation during diffusion in water
  r_v<-r_v+1; val$const$a_i                           <- val$const$a_l + val$const$b_s; #  al+bs
  r_v<-r_v+1; # blank spreadsheet line

  ##details<<
  ## Which values to use
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; sw$Licor_or_TDL_A_photosynthesis                          <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; sw$val_const_override_Licor_flow_rate                     <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  r_v<-r_v+1; val$const$flow_rate                                       <- as.numeric(D[r_v,c_v]); #  these values will normally come from the LI6400, but occaisonally I may want to specify different constants,
  r_v<-r_v+1; sw$val_const_override_Licor_leaf_area                     <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  r_v<-r_v+1; val$const$leaf_area                                       <- as.numeric(D[r_v,c_v]); #  so when they are used in equations below,
  r_v<-r_v+1; sw$val_const_override_Licor_boundary_layer_cond_to_water  <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  r_v<-r_v+1; val$const$boundary_layer_cond_to_water                    <- as.numeric(D[r_v,c_v]); #  I would like to have the option to choose these constants instead of the values in the LI64000 file  (gbw)
  r_v<-r_v+1; sw$val_const_override_Licor_H2OS                          <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  r_v<-r_v+1; val$const$H2OS                                            <- as.numeric(D[r_v,c_v]); #  Not in calcs below yet
  r_v<-r_v+1; sw$val_const_override_Licor_StmRat                        <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  r_v<-r_v+1; val$const$StmRat                                          <- as.numeric(D[r_v,c_v]); #  Not in calcs below yet

  ##details<<
  ## constants in formulas not given names
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; val$const$gbc_1.37                      <- as.numeric(D[r_v,c_v]); #  gbc  BLcond/1.37 boudary layer conductance for CO2
  r_v<-r_v+1; val$const$gsc_1.6                       <- as.numeric(D[r_v,c_v]); #  gsc  gsw/1.6 stomatal conductance for CO2

  r_v<-r_v+1; # blank spreadsheet line


  ##details<<
  ## Calculation information
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; sw$calc_pc_to_use <- as.numeric(D[r_v,c_v]);
                          # 1 = pc_using_gm
                          # 2 = pc_using_simple_Delta_for_gm
                          # 3 = pc_using_simple_Delta_for_modeling
                          # 4 = pc_using_complex_Delta_no_decarboxylation
                          # 5 = pc_using_complex_Delta_full_model

  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  # NOT USED
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; sw$calc_Delta_i_to_use <- as.numeric(D[r_v,c_v]);
                          # 1 = Delta_i_simple_for_gm
                          # 2 = Delta_i_simple_for_modeling
                          # 3 = Delta_i_complex_for_gm

  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  r_v<-r_v+1; sw$calc_gm_to_use <- as.numeric(D[r_v,c_v]);
                          # 1 = gm_point_simple
                          # 2 = gm_point_complex

  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  ##############################################################################
  ##details<<
  ## create a list to all returned parameters
  VARIABLES <- new.env();

  VARIABLES$val                             <- val                          ;
  VARIABLES$sw                              <- sw                           ;
  VARIABLES$TDL_fn                          <- TDL_fn                       ;
  VARIABLES$Licor_fn                        <- Licor_fn                     ;
  VARIABLES$plot_format_list                <- plot_format_list             ;
  VARIABLES$output_fn_prefix                <- output_fn_prefix             ;
  VARIABLES$output_summary_TDL_fn           <- output_summary_TDL_fn        ;
  VARIABLES$output_summary_Licor_fn         <- output_summary_Licor_fn      ;
  VARIABLES$output_summary_Calc_fn          <- output_summary_Calc_fn       ;
  VARIABLES$output_summary_Calc_last_fn     <- output_summary_Calc_last_fn  ;
  VARIABLES$output_all_Calc_fn              <- output_all_Calc_fn           ; # "0.1-16" "2012-07-11"
  VARIABLES$output_CI_TDL_fn                <- output_CI_TDL_fn             ;
  VARIABLES$output_CI_Licor_fn              <- output_CI_Licor_fn           ;
  VARIABLES$output_CI_Calc_fn               <- output_CI_Calc_fn            ;
  VARIABLES$output_CI_Calc_last_fn          <- output_CI_Calc_last_fn       ;
  VARIABLES$R_bootstrap                     <- R_bootstrap                  ;
  VARIABLES$sig_CI                          <- sig_CI                       ;
  VARIABLES$seed                            <- seed                         ;
  VARIABLES$TDL_cycle                       <- TDL_cycle                    ;
  VARIABLES$Licor_TDL_time_offset_seconds   <- Licor_TDL_time_offset_seconds;

  return( as.list(VARIABLES) );
  ### VARIABLES list of all inputs from Excel template
} # assign_variables()

