#' Title
#'
#' @param D
#' @param path
#'
#' @return
#' @export
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
  c.v <- 2;

  # first row is header, then count from there
  r.v <- 1;

  ##details<<
  ## Template version
  r.v<-r.v+1; sw$template.version <- as.numeric(D[r.v,c.v]); # Version of template must match isogasex
  r.v<-r.v+1; # blank spreadsheet line

  # Parameter
  ##details<<
  ## switches to use and filenames for TDL and Licor files
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; sw$use.TDL   <- as.numeric(D[r.v,c.v]); # 1 = process TDL   data, 0 = do not use TDL   data
  r.v<-r.v+1; TDL.fn   <- paste(path, "/", as.character(D[r.v,c.v]), sep="");
  r.v<-r.v+1; sw$use.Licor <- as.numeric(D[r.v,c.v]); # 1 = process Licor data, 0 = do not use Licor data
  r.v<-r.v+1; Licor.fn <- paste(path, "/", as.character(D[r.v,c.v]), sep="");
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line

  ##details<<
  ## Prefix for output filenames
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; output.fn.prefix <- as.character(D[r.v,c.v]); # this prefix will appear on all output files



  ##details<<
  ## TIME WINDOW SECTION
  r.v<-r.v+1; # blank spreadsheet line
                                                            # two rows at a time
  r.v<-r.v+1; val$timewindow$start.time   <- strptime(paste(D[r.v,c.v],D[r.v+1,c.v]), "%m/%d/%y %H:%M:%S");
  r.v<-r.v+1; # row in start.time
  r.v<-r.v+1; val$timewindow$end.time     <- strptime(paste(D[r.v,c.v],D[r.v+1,c.v]), "%m/%d/%y %H:%M:%S");
  r.v<-r.v+1; # row in end.time


  ##details<<
  ## OUTPUT SECTION
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; sw$print.TDL.missing       <- as.numeric(D[r.v,c.v]); # high tank
  r.v<-r.v+1; sw$print.TDL.cycle.timing  <- as.numeric(D[r.v,c.v]); # low tank
  r.v<-r.v+1; sw$write.all.obs.file      <- as.numeric(D[r.v,c.v]); # leaf reference
  r.v<-r.v+1; sw$save.RData              <- as.numeric(D[r.v,c.v]); # leaf reference
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line


  ##details<<
  ## plot formats
  r.v<-r.v+1; # blank spreadsheet line
  plot.format.list <- NULL;  # 1=png, 2=eps, 3=pdf, 4=bmp, 5=jpeg, 6=tiff
  r.v<-r.v+1; if ( as.numeric(D[r.v,c.v]) ) { plot.format.list <- c(plot.format.list, 1); };  # png
  r.v<-r.v+1; if ( as.numeric(D[r.v,c.v]) ) { plot.format.list <- c(plot.format.list, 2); };  # eps
  r.v<-r.v+1; if ( as.numeric(D[r.v,c.v]) ) { plot.format.list <- c(plot.format.list, 3); };  # pdf
  r.v<-r.v+1; if ( as.numeric(D[r.v,c.v]) ) { plot.format.list <- c(plot.format.list, 4); };  # bmp
  r.v<-r.v+1; if ( as.numeric(D[r.v,c.v]) ) { plot.format.list <- c(plot.format.list, 5); };  # jpeg
  r.v<-r.v+1; if ( as.numeric(D[r.v,c.v]) ) { plot.format.list <- c(plot.format.list, 6); };  # tiff

  #if (sw$use.TDL  ) { output.summary.TDL.fn       <- paste(output.fn.prefix, "_summary_TDL.csv",        sep=""); };
  #if (sw$use.Licor) { output.summary.Licor.fn     <- paste(output.fn.prefix, "_summary_Licor.csv",      sep=""); };
                      output.summary.TDL.fn       <- paste(output.fn.prefix, "_summary_TDL.csv",        sep="");
                      output.summary.Licor.fn     <- paste(output.fn.prefix, "_summary_Licor.csv",      sep="");
                      output.summary.Calc.fn      <- paste(output.fn.prefix, "_summary_Calc.csv",       sep="");
                      output.summary.Calc.last.fn <- paste(output.fn.prefix, "_summary_Calc_last.csv",  sep="");
                      output.all.Calc.fn          <- paste(output.fn.prefix, "_all_Calc.csv",           sep=""); # "0.1-16" "2012-07-11"
                      output.CI.TDL.fn            <- paste(output.fn.prefix, "_CI_TDL.csv",             sep="");
                      output.CI.Licor.fn          <- paste(output.fn.prefix, "_CI_Licor.csv",           sep="");
                      output.CI.Calc.fn           <- paste(output.fn.prefix, "_CI_Calc.csv",            sep="");
                      output.CI.Calc.last.fn      <- paste(output.fn.prefix, "_CI_Calc_last.csv",       sep="");

  ##details<<
  ## interpolation:  1 = smooth.spline() like tanks
  ##                 2 = use last reference mean until next measurement (horizontal interp) - for abrupt ref gas changes
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; sw$interp.tank.hi    <- as.numeric(D[r.v,c.v]); # high tank
  r.v<-r.v+1; sw$interp.tank.low   <- as.numeric(D[r.v,c.v]); # low tank
  r.v<-r.v+1; sw$interp.reference  <- as.numeric(D[r.v,c.v]); # leaf reference


  ##details<<
  ## Bootstrap
  r.v<-r.v+1; # blank spreadsheet line

  r.v<-r.v+1; R.bootstrap <- as.numeric(D[r.v,c.v]); # Number of bootstrap samples to assess uncertainty of estimates
  r.v<-r.v+1; sig.CI      <- as.numeric(D[r.v,c.v]); # Confidence level for central confidence interval
  r.v<-r.v+1; seed        <- as.numeric(D[r.v,c.v]); # pseudorandom number seed, 0 is random based on clock, set to integer for results to be repeatable

  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line

  ##details<<
  ## TDL sampling information
  TDL.cycle <- as.list(new.env());  # create a list to return with data
          # number.* is TDL field "PrevSite"
  r.v<-r.v+1; TDL.cycle$number.tank.hi    <- as.numeric(D[r.v,c.v]);   # 9; # high tank
  r.v<-r.v+1; TDL.cycle$number.tank.low   <- as.numeric(D[r.v,c.v]);   #10; # low tank
  r.v<-r.v+1; TDL.cycle$number.reference  <- as.numeric(D[r.v,c.v]);   # 1; # leaf reference
  r.v<-r.v+1; TDL.cycle$number.chamber    <- as.numeric(D[r.v,c.v]);   #21; # leaf chamber
  r.v<-r.v+1; TDL.cycle$name.tank.hi      <- as.character(D[r.v,c.v]); #"tank.hi";   # high tank
  r.v<-r.v+1; TDL.cycle$name.tank.low     <- as.character(D[r.v,c.v]); #"tank.low";  # low tank
  r.v<-r.v+1; TDL.cycle$name.reference    <- as.character(D[r.v,c.v]); #"reference"; # leaf reference
  r.v<-r.v+1; TDL.cycle$name.chamber      <- as.character(D[r.v,c.v]); #"chamber";   # leaf chamber

  r.v<-r.v+1; # blank spreadsheet line

          # seconds is time length of measurements to use as data
  r.v<-r.v+1; TDL.cycle$seconds.tank.hi   <- as.numeric(D[r.v,c.v]); # high tank
  r.v<-r.v+1; TDL.cycle$seconds.tank.low  <- as.numeric(D[r.v,c.v]); # low tank
  r.v<-r.v+1; TDL.cycle$seconds.reference <- as.numeric(D[r.v,c.v]); # leaf reference
  r.v<-r.v+1; TDL.cycle$seconds.chamber   <- as.numeric(D[r.v,c.v]); # leaf chamber
  r.v<-r.v+1; TDL.cycle$Hz                <- as.numeric(D[r.v,c.v]); # sampling rate, output of TDL
  r.v<-r.v+1; TDL.cycle$seconds.exclude.first.chamber <- as.numeric(D[r.v,c.v]); # time it takes to equilbrate from reference to leaf chamber
  TDL.cycle <- set_TDL_cycle(TDL.cycle); # TDL.cycle variables
 #if (sw$use.TDL  ) {
 #  # TDL sampling information
 #  TDL.cycle <- as.list(new.env());  # create a list to return with data
 #          # number.* is TDL field "PrevSite"
 #  TDL.cycle$number.tank.hi    <-  9; # high tank
 #  TDL.cycle$number.tank.low   <- 10; # low tank
 #  TDL.cycle$number.reference  <-  1; # leaf reference
 #  TDL.cycle$number.chamber    <- 21; # leaf chamber
 #  TDL.cycle$name.tank.hi      <- "tank.hi"; # high tank
 #  TDL.cycle$name.tank.low     <- "tank.low"; # low tank
 #  TDL.cycle$name.reference    <- "reference"; # leaf reference
 #  TDL.cycle$name.chamber      <- "chamber"; # leaf chamber
 #          # seconds is time length of measurements to use as data
 #  TDL.cycle$seconds.tank.hi   <- 5; # high tank
 #  TDL.cycle$seconds.tank.low  <- 5; # low tank
 #  TDL.cycle$seconds.reference <- 5; # leaf reference
 #  TDL.cycle$seconds.chamber   <- 5; # leaf chamber
 #  TDL.cycle$Hz                <- 10; # sampling rate, output of TDL
 #  TDL.cycle$seconds.exclude.first.chamber <- 20; # time it takes to equilbrate from reference to leaf chamber
 #  TDL.cycle <- set_TDL_cycle(TDL.cycle); # TDL.cycle variables
 #};

  r.v<-r.v+1; # blank spreadsheet line

  # If using Licor file
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; Licor.TDL.time.offset.seconds <- as.numeric(D[r.v,c.v]);
                                        # positive indicates Licor is ahead of TDL
                                        # (e.g., 16 has Licor = 12:00:00 matches TDL = 12:00:16)
                                        # (that is, gas takes 16 seconds to get from Licor to TDL)
 #if (sw$use.Licor) {
 #  Licor.TDL.time.offset.seconds <- 0; # positive indicates Licor is ahead of TDL
 #                                      # (e.g., 16 has Licor = 12:00:00 matches TDL = 12:00:16)
 #                                      # (that is, gas takes 16 seconds to get from Licor to TDL)
 #};

  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line

  ##details<<
  ## CONSTANTS SECTION
  ##details<<
  ## True calibration tank values
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; val$const$true.value.hi.12C             <- as.numeric(D[r.v,c.v]); # example tank values
  r.v<-r.v+1; val$const$true.value.hi.13C             <- as.numeric(D[r.v,c.v]);
  r.v<-r.v+1; val$const$true.value.lo.12C             <- as.numeric(D[r.v,c.v]);
  r.v<-r.v+1; val$const$true.value.lo.13C             <- as.numeric(D[r.v,c.v]);
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; val$const$fo.13C                        <- as.numeric(D[r.v,c.v]); # 0.00474;  # if only 12 and 13CO2 are measured, fo means fraction of other isotopologues not measured
  r.v<-r.v+1; val$const$fo.18O                        <- as.numeric(D[r.v,c.v]); # if 12 and 13CO2, and 12C18O16O are measured (not ususal for Dave's machine)
  r.v<-r.v+1; val$const$Rstd.13C                      <- as.numeric(D[r.v,c.v]); # (Rvpdb)
  r.v<-r.v+1; val$const$Rstd.18O                      <- as.numeric(D[r.v,c.v]); # (Rvsmow) may occaisionally be used with Dave's machine, method not tested yet
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; val$const$Gamma.star                    <- as.numeric(D[r.v,c.v]); # CO2 compensation point in absence of day respiration.  I will need to specify experiment specific values for these constants in the equations below
  r.v<-r.v+1; val$const$f.photo.respiration           <- as.numeric(D[r.v,c.v]); # fractionation associated with photorespiration
  r.v<-r.v+1; val$const$e                             <- as.numeric(D[r.v,c.v]); # fractionation associated with day respiration
  r.v<-r.v+1; val$const$Rd                            <- as.numeric(D[r.v,c.v]); # day respiration
  r.v<-r.v+1; val$const$k                             <- as.numeric(D[r.v,c.v]); # carboxylation efficiency, often initial slope of A/Ci, or better A/Cc
  r.v<-r.v+1; val$const$b.gm                          <- as.numeric(D[r.v,c.v]);
  r.v<-r.v+1; val$const$b.modeling                    <- as.numeric(D[r.v,c.v]);
  r.v<-r.v+1; val$const$b.s                           <- as.numeric(D[r.v,c.v]); #  per mil fractionation as CO2 enters solution at 25C bs =  1.1
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; val$const$a                             <- as.numeric(D[r.v,c.v]); #  per mil fractionation in still air (stomatal pore and substomata) a1 =  0.7
  r.v<-r.v+1; val$const$a.b                           <- as.numeric(D[r.v,c.v]); #  per mil fractionation in boundary layer (slightly mixed)  a = 4.44
  r.v<-r.v+1; val$const$a.l                           <- as.numeric(D[r.v,c.v]); #  per mil fractionation during diffusion in water
  r.v<-r.v+1; val$const$a.i                           <- val$const$a.l + val$const$b.s; #  al+bs
  r.v<-r.v+1; # blank spreadsheet line

  ##details<<
  ## Which values to use
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; sw$Licor.or.TDL.A.photosynthesis                          <- as.numeric(D[r.v,c.v]); #  1=use Licor values, 0=use specified values for (val$const$flow.rate, val$const$leaf.area, val$const$boundary.layer.cond.to.water, val$const$H2OS, val$const$StmRat)
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; sw$val.const.override.Licor.flow.rate                     <- as.numeric(D[r.v,c.v]); #  1=use Licor values, 0=use specified values for (val$const$flow.rate, val$const$leaf.area, val$const$boundary.layer.cond.to.water, val$const$H2OS, val$const$StmRat)
  r.v<-r.v+1; val$const$flow.rate                                       <- as.numeric(D[r.v,c.v]); #  these values will normally come from the LI6400, but occaisonally I may want to specify different constants,
  r.v<-r.v+1; sw$val.const.override.Licor.leaf.area                     <- as.numeric(D[r.v,c.v]); #  1=use Licor values, 0=use specified values for (val$const$flow.rate, val$const$leaf.area, val$const$boundary.layer.cond.to.water, val$const$H2OS, val$const$StmRat)
  r.v<-r.v+1; val$const$leaf.area                                       <- as.numeric(D[r.v,c.v]); #  so when they are used in equations below,
  r.v<-r.v+1; sw$val.const.override.Licor.boundary.layer.cond.to.water  <- as.numeric(D[r.v,c.v]); #  1=use Licor values, 0=use specified values for (val$const$flow.rate, val$const$leaf.area, val$const$boundary.layer.cond.to.water, val$const$H2OS, val$const$StmRat)
  r.v<-r.v+1; val$const$boundary.layer.cond.to.water                    <- as.numeric(D[r.v,c.v]); #  I would like to have the option to choose these constants instead of the values in the LI64000 file  (gbw)
  r.v<-r.v+1; sw$val.const.override.Licor.H2OS                          <- as.numeric(D[r.v,c.v]); #  1=use Licor values, 0=use specified values for (val$const$flow.rate, val$const$leaf.area, val$const$boundary.layer.cond.to.water, val$const$H2OS, val$const$StmRat)
  r.v<-r.v+1; val$const$H2OS                                            <- as.numeric(D[r.v,c.v]); #  Not in calcs below yet
  r.v<-r.v+1; sw$val.const.override.Licor.StmRat                        <- as.numeric(D[r.v,c.v]); #  1=use Licor values, 0=use specified values for (val$const$flow.rate, val$const$leaf.area, val$const$boundary.layer.cond.to.water, val$const$H2OS, val$const$StmRat)
  r.v<-r.v+1; val$const$StmRat                                          <- as.numeric(D[r.v,c.v]); #  Not in calcs below yet

  ##details<<
  ## constants in formulas not given names
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; val$const$gbc.1.37                      <- as.numeric(D[r.v,c.v]); #  gbc  BLcond/1.37 boudary layer conductance for CO2
  r.v<-r.v+1; val$const$gsc.1.6                       <- as.numeric(D[r.v,c.v]); #  gsc  gsw/1.6 stomatal conductance for CO2

  r.v<-r.v+1; # blank spreadsheet line


  ##details<<
  ## Calculation information
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; sw$calc.pc.to.use <- as.numeric(D[r.v,c.v]);
                          # 1 = pc.using.gm
                          # 2 = pc.using.simple.Delta.for.gm
                          # 3 = pc.using.simple.Delta.for.modeling
                          # 4 = pc.using.complex.Delta.no.decarboxylation
                          # 5 = pc.using.complex.Delta.full.model

  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line

  # NOT USED
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; sw$calc.Delta.i.to.use <- as.numeric(D[r.v,c.v]);
                          # 1 = Delta.i.simple.for.gm
                          # 2 = Delta.i.simple.for.modeling
                          # 3 = Delta.i.complex.for.gm

  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line

  r.v<-r.v+1; sw$calc.gm.to.use <- as.numeric(D[r.v,c.v]);
                          # 1 = gm.point.simple
                          # 2 = gm.point.complex

  r.v<-r.v+1; # blank spreadsheet line
  r.v<-r.v+1; # blank spreadsheet line

  ##############################################################################
  ##details<<
  ## create a list to all returned parameters
  VARIABLES <- new.env();

  VARIABLES$val                             <- val                          ;
  VARIABLES$sw                              <- sw                           ;
  VARIABLES$TDL.fn                          <- TDL.fn                       ;
  VARIABLES$Licor.fn                        <- Licor.fn                     ;
  VARIABLES$plot.format.list                <- plot.format.list             ;
  VARIABLES$output.fn.prefix                <- output.fn.prefix             ;
  VARIABLES$output.summary.TDL.fn           <- output.summary.TDL.fn        ;
  VARIABLES$output.summary.Licor.fn         <- output.summary.Licor.fn      ;
  VARIABLES$output.summary.Calc.fn          <- output.summary.Calc.fn       ;
  VARIABLES$output.summary.Calc.last.fn     <- output.summary.Calc.last.fn  ;
  VARIABLES$output.all.Calc.fn              <- output.all.Calc.fn           ; # "0.1-16" "2012-07-11"
  VARIABLES$output.CI.TDL.fn                <- output.CI.TDL.fn             ;
  VARIABLES$output.CI.Licor.fn              <- output.CI.Licor.fn           ;
  VARIABLES$output.CI.Calc.fn               <- output.CI.Calc.fn            ;
  VARIABLES$output.CI.Calc.last.fn          <- output.CI.Calc.last.fn       ;
  VARIABLES$R.bootstrap                     <- R.bootstrap                  ;
  VARIABLES$sig.CI                          <- sig.CI                       ;
  VARIABLES$seed                            <- seed                         ;
  VARIABLES$TDL.cycle                       <- TDL.cycle                    ;
  VARIABLES$Licor.TDL.time.offset.seconds   <- Licor.TDL.time.offset.seconds;

  return( as.list(VARIABLES) );
  ### VARIABLES list of all inputs from Excel template
} # assign_variables()

