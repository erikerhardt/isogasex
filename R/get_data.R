#' Get data from Excel spreadsheet
#'
#' Read Excel template with \code{gdata} \code{read.xls} using perl.
#'
#' @param filename
#'
#' @return DATA
#' @importFrom readxl read_xls
#'
#' @examples
get_data <-
function# Get data from Excel spreadsheet
###
(filename
###
)
{

  ##details<<
  ## Read Excel template with \code{gdata} \code{read.xls} using perl.

  # determine if running in windows or unix environment
  #OS = .Platform$OS.type;

  n_sheets = 1;  # number of worksheets in Excel workbook
  #    p_o = paste("   reading ", n_sheets, "sheets: "); wWw <- write_out(p_o);

  #DATA = as.list(new.env());  # create a list to return with all data
  # below wasDATA$D1

  ## 3/23/2012 now all data read with gdata's read.xls using perl
  #if (OS == "windows") {
  #  ## package to read Excel workbooks
  #  library("xlsReadWrite"); # to read Excel files
  #
  #  # assign each worksheet to a variable within the DATA environment
  #  i_sheet <- 1; DATA = as.matrix( read.xls(filename, colNames=FALSE, sheet=i_sheet, type="character") ); wWw <- write_out(paste(" ", i_sheet));
  #    wWw <- write_out(paste("\n"));
  #} # windows

  #if (OS == "unix") {
    ## package to read Excel workbooks
    #library("gdata"); # to read Excel files


    # assign each worksheet to a variable within the DATA environment
    #perl_command <- system("which perl", intern=TRUE);
    #i_sheet <- 1; DATA = as.matrix( read.xls(filename, sheet=i_sheet, verbose=FALSE, header=FALSE, perl=perl_command) ); wWw <- write_out(paste(" ", i_sheet));
    #i_sheet <- 1; DATA = as.matrix( read.xls(filename, sheet=i_sheet, verbose=FALSE, header=FALSE, blank.lines.skip=FALSE, perl="perl") ); # wWw <- write_out(paste(" ", i_sheet));
    i_sheet <- 1; DATA = as.matrix( readxl::read_xls(filename, sheet=i_sheet)); #, verbose=FALSE, header=FALSE, blank.lines.skip=FALSE, perl="perl") ); # wWw <- write_out(paste(" ", i_sheet));
      #wWw <- write_out(paste("\n"));
  #} # unix

  return( DATA );
  ### DATA
}

