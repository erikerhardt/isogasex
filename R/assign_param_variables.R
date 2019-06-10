#' Assign PARAM_RAW values from input file to variable names
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
#' @param D xxxPARAMxxx
#' @param path xxxPARAMxxx
#'
#' @return PARAM list of all inputs from Excel template
#'
assign_param_variables <-
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
  sw  <- list();  # create a list for option switches
  # Conventions, ultimately put everything in val
  val <- list();

  # Constants   prefix: val$const$*
  val$const     <- list();
  ## Results (output) are in val$calc variables
  # Calculated  prefix: val$calc$*
  val$calc      <- list();
  val$calc$obs  <- list(); # for calculated observed values to use in NP bootstrap
  val$calc$all  <- list(); # for all calculated values in time window "0.1-16" "2012-07-11"
  val$calc$sum  <- list(); # for calculated summarized (mean) values to use as estimates
  val$write     <- list(); # to create tables to write_out to files


  ##details<<
  ## Template version
  sw$template_version <- as.numeric(D$Template_version); # Version of template must match isogasex
  # blank spreadsheet line

  # Parameter
  ##details<<
  ## switches to use and filenames for TDL and Licor files
  # blank spreadsheet line
  sw$use_TDL    <- as.numeric(D$sw_use_TDL); # 1 = process TDL   data, 0 = do not use TDL   data
  TDL_fn        <- paste(path, "/", as.character(D$TDL_fn), sep="");
  sw$use_Licor  <- as.numeric(D$sw_use_Licor); # 1 = process Licor data, 0 = do not use Licor data
  Licor_fn      <- paste(path, "/", as.character(D$Licor_fn), sep="");
  # blank spreadsheet line
  # blank spreadsheet line

  ##details<<
  ## Prefix for output filenames
  # blank spreadsheet line
  output_fn_prefix <- as.character(D$output_fn_prefix); # this prefix will appear on all output files



  ##details<<
  ## TIME WINDOW SECTION
  # blank spreadsheet line
                                                            # two rows at a time
  val$timewindow$start_time   <- strptime(paste(D$Start_date,D$Start_time), "%m/%d/%y %H:%M:%S");
  # row in start_time
  val$timewindow$end_time     <- strptime(paste(D$End_date,D$End_time), "%m/%d/%y %H:%M:%S");
  # row in end_time


  ##details<<
  ## OUTPUT SECTION
  # blank spreadsheet line
  sw$print_TDL_missing       <- as.numeric(D$print_TDL_missing); # high tank
  sw$print_TDL_cycle_timing  <- as.numeric(D$print_TDL_cycle_timing); # low tank
  sw$write_all_obs_file      <- as.numeric(D$write_all_obs_file); # leaf reference
  sw$save_RData              <- as.numeric(D$save_RData); # leaf reference
  # blank spreadsheet line
  # blank spreadsheet line


  ##details<<
  ## plot formats
  # blank spreadsheet line
  plot_format_list <- NULL;  # 1=png, 2=eps, 3=pdf, 4=bmp, 5=jpeg, 6=tiff
  if ( as.numeric(D$

# plot formats
png) ) { plot_format_list <- c(plot_format_list, 1); };  # png
  if ( as.numeric(D$eps) ) { plot_format_list <- c(plot_format_list, 2); };  # eps
  if ( as.numeric(D$pdf) ) { plot_format_list <- c(plot_format_list, 3); };  # pdf
  if ( as.numeric(D$bmp) ) { plot_format_list <- c(plot_format_list, 4); };  # bmp
  if ( as.numeric(D$jpeg) ) { plot_format_list <- c(plot_format_list, 5); };  # jpeg
  if ( as.numeric(D$tiff) ) { plot_format_list <- c(plot_format_list, 6); };  # tiff

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
  # blank spreadsheet line
  # blank spreadsheet line
  sw$interp_tank_hi    <- as.numeric(D$interp_tank_hi); # high tank
  sw$interp_tank_low   <- as.numeric(D$interp_tank_low); # low tank
  sw$interp_reference  <- as.numeric(D$interp_reference); # leaf reference


  ##details<<
  ## Bootstrap
  # blank spreadsheet line

  R_bootstrap <- as.numeric(D$R_bootstrap); # Number of bootstrap samples to assess uncertainty of estimates
  sig_CI      <- as.numeric(D$sig_CI); # Confidence level for central confidence interval
  seed        <- as.numeric(D$seed); # pseudorandom number seed, 0 is random based on clock, set to integer for results to be repeatable

  # blank spreadsheet line
  # blank spreadsheet line

  ##details<<
  ## TDL sampling information
  TDL_cycle <- list();  # create a list to return with data
          # number.* is TDL field "PrevSite"
  TDL_cycle$number_tank_hi    <- as.numeric(D$number_tank_hi);   # 9; # high tank
  TDL_cycle$number_tank_low   <- as.numeric(D$number_tank_low);   #10; # low tank
  TDL_cycle$number_reference  <- as.numeric(D$number_reference);   # 1; # leaf reference
  TDL_cycle$number_chamber    <- as.numeric(D$number_chamber);   #21; # leaf chamber
  TDL_cycle$name_tank_hi      <- as.character(D$name_tank_hi); #"tank_hi";   # high tank
  TDL_cycle$name_tank_low     <- as.character(D$name_tank_low); #"tank_low";  # low tank
  TDL_cycle$name_reference    <- as.character(D$name_reference); #"reference"; # leaf reference
  TDL_cycle$name_chamber      <- as.character(D$name_chamber); #"chamber";   # leaf chamber

  # blank spreadsheet line

          # seconds is time length of measurements to use as data
  TDL_cycle$seconds_tank_hi   <- as.numeric(D$seconds_tank_hi); # high tank
  TDL_cycle$seconds_tank_low  <- as.numeric(D$seconds_tank_low); # low tank
  TDL_cycle$seconds_reference <- as.numeric(D$seconds_reference); # leaf reference
  TDL_cycle$seconds_chamber   <- as.numeric(D$seconds_chamber); # leaf chamber
  TDL_cycle$Hz                <- as.numeric(D$Hz); # sampling rate, output of TDL
  TDL_cycle$seconds_exclude_first_chamber <- as.numeric(D$seconds_exclude_first_chamber); # time it takes to equilbrate from reference to leaf chamber
  TDL_cycle <- set_TDL_cycle(TDL_cycle); # TDL_cycle variables

  # If using Licor file
  # blank spreadsheet line
  Licor_TDL_time_offset_seconds <- as.numeric(D$Licor_TDL_time_offset_seconds);
                                        # positive indicates Licor is ahead of TDL
                                        # (e.g., 16 has Licor = 12:00:00 matches TDL = 12:00:16)
                                        # (that is, gas takes 16 seconds to get from Licor to TDL)
 #if (sw$use_Licor) {
 #  Licor_TDL_time_offset_seconds <- 0; # positive indicates Licor is ahead of TDL
 #                                      # (e.g., 16 has Licor = 12:00:00 matches TDL = 12:00:16)
 #                                      # (that is, gas takes 16 seconds to get from Licor to TDL)
 #};

  # blank spreadsheet line
  # blank spreadsheet line
  # blank spreadsheet line

  ##details<<
  ## CONSTANTS SECTION
  ##details<<
  ## True calibration tank values
  # blank spreadsheet line
  val$const$true_value_hi_12C             <- as.numeric(D$true_value_hi_12C); # example tank values
  val$const$true_value_hi_13C             <- as.numeric(D$true_value_hi_13C);
  val$const$true_value_lo_12C             <- as.numeric(D$true_value_lo_12C);
  val$const$true_value_lo_13C             <- as.numeric(D$true_value_lo_13C);
  # blank spreadsheet line
  val$const$fo_13C                        <- as.numeric(D$fo_13C); # 0.00474;  # if only 12 and 13CO2 are measured, fo means fraction of other isotopologues not measured
  val$const$fo_18O                        <- as.numeric(D$fo_18O); # if 12 and 13CO2, and 12C18O16O are measured (not ususal for Dave's machine)
  val$const$Rstd_13C                      <- as.numeric(D$Rstd_13C); # (Rvpdb)
  val$const$Rstd_18O                      <- as.numeric(D$Rstd_18O); # (Rvsmow) may occaisionally be used with Dave's machine, method not tested yet
  # blank spreadsheet line
  val$const$Gamma_star                    <- as.numeric(D$Gamma_star); # CO2 compensation point in absence of day respiration.  I will need to specify experiment specific values for these constants in the equations below
  val$const$f_photo_respiration           <- as.numeric(D$f_photo_respiration); # fractionation associated with photorespiration
  val$const$e                             <- as.numeric(D$e); # fractionation associated with day respiration
  val$const$Rd                            <- as.numeric(D$Rd); # day respiration
  val$const$k                             <- as.numeric(D$k); # carboxylation efficiency, often initial slope of A/Ci, or better A/Cc
  val$const$b_gm                          <- as.numeric(D$b_gm);
  val$const$b_modeling                    <- as.numeric(D$b_modeling);
  val$const$b_s                           <- as.numeric(D$b_s); #  per mil fractionation as CO2 enters solution at 25C bs =  1.1
  # blank spreadsheet line
  val$const$a                             <- as.numeric(D$a); #  per mil fractionation in still air (stomatal pore and substomata) a1 =  0.7
  val$const$a_b                           <- as.numeric(D$a_b); #  per mil fractionation in boundary layer (slightly mixed)  a = 4.44
  val$const$a_l                           <- as.numeric(D$a_l); #  per mil fractionation during diffusion in water
  val$const$a_i                           <- val$const$a_l + val$const$b_s; #  al+bs
  # blank spreadsheet line

  ##details<<
  ## Which values to use
  # blank spreadsheet line
  sw$Licor_or_TDL_A_photosynthesis                          <- as.numeric(D$Licor_or_TDL_A_photosynthesis); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  # blank spreadsheet line
  sw$val_const_override_Licor_flow_rate                     <- as.numeric(D$override_Licor_flow_rate); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  val$const$flow_rate                                       <- as.numeric(D$Licor_flow_rate); #  these values will normally come from the LI6400, but occaisonally I may want to specify different constants,
  sw$val_const_override_Licor_leaf_area                     <- as.numeric(D$override_Licor_leaf_area); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  val$const$leaf_area                                       <- as.numeric(D$Licor_leaf_area); #  so when they are used in equations below,
  sw$val_const_override_Licor_boundary_layer_cond_to_water  <- as.numeric(D$override_Licor_boundary_layer_cond_to_water); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  val$const$boundary_layer_cond_to_water                    <- as.numeric(D$Licor_boundary_layer_cond_to_water); #  I would like to have the option to choose these constants instead of the values in the LI64000 file  (gbw)
  sw$val_const_override_Licor_H2OS                          <- as.numeric(D$override_Licor_H2OS); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  val$const$H2OS                                            <- as.numeric(D$Licor_H2OS); #  Not in calcs below yet
  sw$val_const_override_Licor_StmRat                        <- as.numeric(D$override_Licor_StmRat); #  1=use Licor values, 0=use specified values for (val$const$flow_rate, val$const$leaf_area, val$const$boundary_layer_cond_to_water, val$const$H2OS, val$const$StmRat)
  val$const$StmRat                                          <- as.numeric(D$Licor_StmRat); #  Not in calcs below yet

  ##details<<
  ## constants in formulas not given names
  # blank spreadsheet line
  val$const$gbc_1.37                      <- as.numeric(D$gbc_1_37); #  gbc  BLcond/1.37 boudary layer conductance for CO2
  val$const$gsc_1.6                       <- as.numeric(D$gsc_1_6); #  gsc  gsw/1.6 stomatal conductance for CO2

  # blank spreadsheet line


  ##details<<
  ## Calculation information
  # blank spreadsheet line
  sw$calc_pc_to_use <- as.numeric(D$calc_pc_to_use);
                          # 1 = pc_using_gm
                          # 2 = pc_using_simple_Delta_for_gm
                          # 3 = pc_using_simple_Delta_for_modeling
                          # 4 = pc_using_complex_Delta_no_decarboxylation
                          # 5 = pc_using_complex_Delta_full_model

  # blank spreadsheet line
  # blank spreadsheet line
  # blank spreadsheet line
  # blank spreadsheet line

  # NOT USED
  # blank spreadsheet line
  sw$calc_Delta_i_to_use <- as.numeric(D$calc_Delta_i_to_use);
                          # 1 = Delta_i_simple_for_gm
                          # 2 = Delta_i_simple_for_modeling
                          # 3 = Delta_i_complex_for_gm

  # blank spreadsheet line
  # blank spreadsheet line
  # blank spreadsheet line

  sw$calc_gm_to_use <- as.numeric(D$calc_gm_to_use);
                          # 1 = gm_point_simple
                          # 2 = gm_point_complex

  # blank spreadsheet line
  # blank spreadsheet line

  ##############################################################################
  ##details<<
  ## create a list to all returned parameters
  PARAM <- list();

  PARAM$val                             <- val                          ;
  PARAM$sw                              <- sw                           ;
  PARAM$TDL_fn                          <- TDL_fn                       ;
  PARAM$Licor_fn                        <- Licor_fn                     ;
  PARAM$plot_format_list                <- plot_format_list             ;
  PARAM$output_fn_prefix                <- output_fn_prefix             ;
  PARAM$output_summary_TDL_fn           <- output_summary_TDL_fn        ;
  PARAM$output_summary_Licor_fn         <- output_summary_Licor_fn      ;
  PARAM$output_summary_Calc_fn          <- output_summary_Calc_fn       ;
  PARAM$output_summary_Calc_last_fn     <- output_summary_Calc_last_fn  ;
  PARAM$output_all_Calc_fn              <- output_all_Calc_fn           ; # "0.1-16" "2012-07-11"
  PARAM$output_CI_TDL_fn                <- output_CI_TDL_fn             ;
  PARAM$output_CI_Licor_fn              <- output_CI_Licor_fn           ;
  PARAM$output_CI_Calc_fn               <- output_CI_Calc_fn            ;
  PARAM$output_CI_Calc_last_fn          <- output_CI_Calc_last_fn       ;
  PARAM$R_bootstrap                     <- R_bootstrap                  ;
  PARAM$sig_CI                          <- sig_CI                       ;
  PARAM$seed                            <- seed                         ;
  PARAM$TDL_cycle                       <- TDL_cycle                    ;
  PARAM$Licor_TDL_time_offset_seconds   <- Licor_TDL_time_offset_seconds;

  return( PARAM );
  ### PARAM list of all inputs from template
} # assign_param_variables()

