#' Assign variables from xls template input file to yaml variable names
#'
#' @param D Parameter input table
#'
#' @return PARAM_RAW list of all inputs from Excel template
#'
read_template_param_table_to_raw <-
  function(D)
{
  ## DEBUG
  # D <- PARAM_TABLE

  PARAM_RAW <- list()

  # column 2 of the spreadsheet has the values
  c_v <- 2;

  # first row is header, then count from there
  r_v <- 0;

  ##details<<
  ## Template version
  r_v<-r_v+1; PARAM_RAW$Template_version <- as.numeric(D[r_v,c_v]); # Version of template must match isogasex
  r_v<-r_v+1; # blank spreadsheet line

  # Parameter
  ##details<<
  ## switches to use and filenames for TDL and Licor files
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$sw_use_TDL    <- as.numeric(D[r_v,c_v]); # 1 = process TDL   data, 0 = do not use TDL   data
  r_v<-r_v+1; PARAM_RAW$TDL_fn        <- as.character(D[r_v,c_v])
  r_v<-r_v+1; PARAM_RAW$sw_use_Licor  <- as.numeric(D[r_v,c_v]); # 1 = process Licor data, 0 = do not use Licor data
  r_v<-r_v+1; PARAM_RAW$Licor_fn      <- as.character(D[r_v,c_v])
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  ##details<<
  ## Prefix for output filenames
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$output_fn_prefix  <- as.character(D[r_v,c_v]); # this prefix will appear on all output files



  ##details<<
  ## TIME WINDOW SECTION
  r_v<-r_v+1; # blank spreadsheet line
                                                            # two rows at a time
  r_v<-r_v+1; PARAM_RAW$Start_date    <- format(strptime(D[r_v,c_v], "%m/%d/%y"), format="%m/%d/%y"); attr(PARAM_RAW$Start_date, "names") <- NULL
  r_v<-r_v+1; PARAM_RAW$Start_time    <- format(strptime(D[r_v,c_v], "%H:%M:%S"), format="%H:%M:%S"); attr(PARAM_RAW$Start_time, "names") <- NULL
  r_v<-r_v+1; PARAM_RAW$End_date      <- format(strptime(D[r_v,c_v], "%m/%d/%y"), format="%m/%d/%y"); attr(PARAM_RAW$End_date  , "names") <- NULL
  r_v<-r_v+1; PARAM_RAW$End_time      <- format(strptime(D[r_v,c_v], "%H:%M:%S"), format="%H:%M:%S"); attr(PARAM_RAW$End_time  , "names") <- NULL


  ##details<<
  ## OUTPUT SECTION
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$print_TDL_missing       <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; PARAM_RAW$print_TDL_cycle_timing  <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; PARAM_RAW$write_all_obs_file      <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; PARAM_RAW$save_RData              <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line


  ##details<<
  ## plot formats
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$png  <- as.numeric(D[r_v,c_v])
  r_v<-r_v+1; PARAM_RAW$eps  <- as.numeric(D[r_v,c_v])
  r_v<-r_v+1; PARAM_RAW$pdf  <- as.numeric(D[r_v,c_v])
  r_v<-r_v+1; PARAM_RAW$bmp  <- as.numeric(D[r_v,c_v])
  r_v<-r_v+1; PARAM_RAW$jpeg <- as.numeric(D[r_v,c_v])
  r_v<-r_v+1; PARAM_RAW$tiff <- as.numeric(D[r_v,c_v])

  ##details<<
  ## interpolation:  1 = smooth.spline() like tanks
  ##                 2 = use last reference mean until next measurement (horizontal interp) - for abrupt ref gas changes
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$interp_tank_hi    <- as.numeric(D[r_v,c_v]); # high tank
  r_v<-r_v+1; PARAM_RAW$interp_tank_low   <- as.numeric(D[r_v,c_v]); # low tank
  r_v<-r_v+1; PARAM_RAW$interp_reference  <- as.numeric(D[r_v,c_v]); # leaf reference

  ##details<<
  ## Bootstrap
  r_v<-r_v+1; # blank spreadsheet line

  r_v<-r_v+1; PARAM_RAW$R_bootstrap <- as.numeric(D[r_v,c_v]); # Number of bootstrap samples to assess uncertainty of estimates
  r_v<-r_v+1; PARAM_RAW$sig_CI      <- as.numeric(D[r_v,c_v]); # Confidence level for central confidence interval
  r_v<-r_v+1; PARAM_RAW$seed        <- as.numeric(D[r_v,c_v]); # pseudorandom number seed, 0 is random based on clock, set to integer for results to be repeatable

  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  ##details<<
  ## TDL sampling information
  r_v<-r_v+1; PARAM_RAW$number_tank_hi    <- as.numeric(D[r_v,c_v]);   # 9; # high tank
  r_v<-r_v+1; PARAM_RAW$number_tank_low   <- as.numeric(D[r_v,c_v]);   #10; # low tank
  r_v<-r_v+1; PARAM_RAW$number_reference  <- as.numeric(D[r_v,c_v]);   # 1; # leaf reference
  r_v<-r_v+1; PARAM_RAW$number_chamber    <- as.numeric(D[r_v,c_v]);   #21; # leaf chamber
  r_v<-r_v+1; PARAM_RAW$name_tank_hi      <- as.character(D[r_v,c_v]); #"tank_hi";   # high tank
  r_v<-r_v+1; PARAM_RAW$name_tank_low     <- as.character(D[r_v,c_v]); #"tank_low";  # low tank
  r_v<-r_v+1; PARAM_RAW$name_reference    <- as.character(D[r_v,c_v]); #"reference"; # leaf reference
  r_v<-r_v+1; PARAM_RAW$name_chamber      <- as.character(D[r_v,c_v]); #"chamber";   # leaf chamber

  r_v<-r_v+1; # blank spreadsheet line

          # seconds is time length of measurements to use as data
  r_v<-r_v+1; PARAM_RAW$seconds_tank_hi   <- as.numeric(D[r_v,c_v]); # high tank
  r_v<-r_v+1; PARAM_RAW$seconds_tank_low  <- as.numeric(D[r_v,c_v]); # low tank
  r_v<-r_v+1; PARAM_RAW$seconds_reference <- as.numeric(D[r_v,c_v]); # leaf reference
  r_v<-r_v+1; PARAM_RAW$seconds_chamber   <- as.numeric(D[r_v,c_v]); # leaf chamber
  r_v<-r_v+1; PARAM_RAW$Hz                <- as.numeric(D[r_v,c_v]); # sampling rate, output of TDL
  r_v<-r_v+1; PARAM_RAW$seconds_exclude_first_chamber <- as.numeric(D[r_v,c_v]); # time it takes to equilbrate from reference to leaf chamber

  r_v<-r_v+1; # blank spreadsheet line

  # If using Licor file
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$Licor_TDL_time_offset_seconds <- as.numeric(D[r_v,c_v]);
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
  r_v<-r_v+1; PARAM_RAW$true_value_hi_12C             <- as.numeric(D[r_v,c_v]); # example tank values
  r_v<-r_v+1; PARAM_RAW$true_value_hi_13C             <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; PARAM_RAW$true_value_lo_12C             <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; PARAM_RAW$true_value_lo_13C             <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$fo_13C                        <- as.numeric(D[r_v,c_v]); # 0.00474;  # if only 12 and 13CO2 are measured, fo means fraction of other isotopologues not measured
  r_v<-r_v+1; PARAM_RAW$fo_18O                        <- as.numeric(D[r_v,c_v]); # if 12 and 13CO2, and 12C18O16O are measured (not ususal for Dave's machine)
  r_v<-r_v+1; PARAM_RAW$Rstd_13C                      <- as.numeric(D[r_v,c_v]); # (Rvpdb)
  r_v<-r_v+1; PARAM_RAW$Rstd_18O                      <- as.numeric(D[r_v,c_v]); # (Rvsmow) may occaisionally be used with Dave's machine, method not tested yet
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$Gamma_star                    <- as.numeric(D[r_v,c_v]); # CO2 compensation point in absence of day respiration.  I will need to specify experiment specific values for these constants in the equations below
  r_v<-r_v+1; PARAM_RAW$f_photo_respiration           <- as.numeric(D[r_v,c_v]); # fractionation associated with photorespiration
  r_v<-r_v+1; PARAM_RAW$e                             <- as.numeric(D[r_v,c_v]); # fractionation associated with day respiration
  r_v<-r_v+1; PARAM_RAW$Rd                            <- as.numeric(D[r_v,c_v]); # day respiration
  r_v<-r_v+1; PARAM_RAW$k                             <- as.numeric(D[r_v,c_v]); # carboxylation efficiency, often initial slope of A/Ci, or better A/Cc
  r_v<-r_v+1; PARAM_RAW$b_gm                          <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; PARAM_RAW$b_modeling                    <- as.numeric(D[r_v,c_v]);
  r_v<-r_v+1; PARAM_RAW$b_s                           <- as.numeric(D[r_v,c_v]); #  per mil fractionation as CO2 enters solution at 25C bs =  1.1
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$a                             <- as.numeric(D[r_v,c_v]); #  per mil fractionation in still air (stomatal pore and substomata) a1 =  0.7
  r_v<-r_v+1; PARAM_RAW$a_b                           <- as.numeric(D[r_v,c_v]); #  per mil fractionation in boundary layer (slightly mixed)  a = 4.44
  r_v<-r_v+1; PARAM_RAW$a_l                           <- as.numeric(D[r_v,c_v]); #  per mil fractionation during diffusion in water
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  ##details<<
  ## Which values to use
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$Licor_or_TDL_A_photosynthesis       <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (flow_rate, leaf_area, boundary_layer_cond_to_water, H2OS, StmRat)
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$override_Licor_flow_rate            <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (flow_rate, leaf_area, boundary_layer_cond_to_water, H2OS, StmRat)
  r_v<-r_v+1; PARAM_RAW$Licor_flow_rate                     <- as.numeric(D[r_v,c_v]); #  these values will normally come from the LI6400, but occaisonally I may want to specify different constants,
  r_v<-r_v+1; PARAM_RAW$override_Licor_leaf_area            <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (flow_rate, leaf_area, boundary_layer_cond_to_water, H2OS, StmRat)
  r_v<-r_v+1; PARAM_RAW$Licor_leaf_area                     <- as.numeric(D[r_v,c_v]); #  so when they are used in equations below,
  r_v<-r_v+1; PARAM_RAW$override_Licor_boundary_layer_cond_to_water   <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (flow_rate, leaf_area, boundary_layer_cond_to_water, H2OS, StmRat)
  r_v<-r_v+1; PARAM_RAW$Licor_boundary_layer_cond_to_water  <- as.numeric(D[r_v,c_v]); #  I would like to have the option to choose these constants instead of the values in the LI64000 file  (gbw)
  r_v<-r_v+1; PARAM_RAW$override_Licor_H2OS                 <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (flow_rate, leaf_area, boundary_layer_cond_to_water, H2OS, StmRat)
  r_v<-r_v+1; PARAM_RAW$Licor_H2OS                          <- as.numeric(D[r_v,c_v]); #  Not in calcs below yet
  r_v<-r_v+1; PARAM_RAW$override_Licor_StmRat               <- as.numeric(D[r_v,c_v]); #  1=use Licor values, 0=use specified values for (flow_rate, leaf_area, boundary_layer_cond_to_water, H2OS, StmRat)
  r_v<-r_v+1; PARAM_RAW$Licor_StmRat                        <- as.numeric(D[r_v,c_v]); #  Not in calcs below yet

  ##details<<
  ## constants in formulas not given names
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$gbc_1_37                      <- as.numeric(D[r_v,c_v]); #  gbc  BLcond/1.37 boudary layer conductance for CO2
  r_v<-r_v+1; PARAM_RAW$gsc_1_6                       <- as.numeric(D[r_v,c_v]); #  gsc  gsw/1.6 stomatal conductance for CO2

  r_v<-r_v+1; # blank spreadsheet line


  ##details<<
  ## Calculation information
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; PARAM_RAW$calc_pc_to_use <- as.numeric(D[r_v,c_v]);
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
  r_v<-r_v+1; PARAM_RAW$calc_Delta_i_to_use <- as.numeric(D[r_v,c_v]);
                          # 1 = Delta_i_simple_for_gm
                          # 2 = Delta_i_simple_for_modeling
                          # 3 = Delta_i_complex_for_gm

  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line

  r_v<-r_v+1; PARAM_RAW$calc_gm_to_use <- as.numeric(D[r_v,c_v]);
                          # 1 = gm_point_simple
                          # 2 = gm_point_complex

  r_v<-r_v+1; # blank spreadsheet line
  r_v<-r_v+1; # blank spreadsheet line


  return( PARAM_RAW);
}

