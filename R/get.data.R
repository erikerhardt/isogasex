get.data <-
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

  n.sheets = 1;  # number of worksheets in Excel workbook
  #    p.o = paste("   reading ", n.sheets, "sheets: "); wWw <- write.out(p.o);

  #DATA = as.list(new.env());  # create a list to return with all data
  # below wasDATA$D1

  ## 3/23/2012 now all data read with gdata's read.xls using perl
  #if (OS == "windows") {
  #  ## package to read Excel workbooks
  #  library("xlsReadWrite"); # to read Excel files
  #
  #  # assign each worksheet to a variable within the DATA environment
  #  i.sheet <- 1; DATA = as.matrix( read.xls(filename, colNames=FALSE, sheet=i.sheet, type="character") ); wWw <- write.out(paste(" ", i.sheet));
  #    wWw <- write.out(paste("\n"));
  #} # windows

  #if (OS == "unix") {
    ## package to read Excel workbooks
    #library("gdata"); # to read Excel files


    # assign each worksheet to a variable within the DATA environment
    #perl.command <- system("which perl", intern=TRUE);
    #i.sheet <- 1; DATA = as.matrix( read.xls(filename, sheet=i.sheet, verbose=FALSE, header=FALSE, perl=perl.command) ); wWw <- write.out(paste(" ", i.sheet));
    i.sheet <- 1; DATA = as.matrix( read.xls(filename, sheet=i.sheet, verbose=FALSE, header=FALSE, blank.lines.skip=FALSE, perl="perl") ); # wWw <- write.out(paste(" ", i.sheet));
      #wWw <- write.out(paste("\n"));
  #} # unix

  return( DATA );
  ### DATA
}

