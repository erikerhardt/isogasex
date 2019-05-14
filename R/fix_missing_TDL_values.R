#' Title
#'
#' @param TDL
#' @param bad.ind
#'
#' @return
#' @export
#'
#' @examples
fix_missing_TDL_values <-
function# Replace NA and NAN values in TDL$data by previous value for that variable
###
(TDL
###
, bad.ind
###
)
{



  ii.fix.count <- 0;
  rc.data <- dim(TDL$data);
  for (i.bad.ind in bad.ind) {
      # counting
      ii.fix.count <- ii.fix.count + 1;
      p.o <- paste(ii.fix.count," "); wWw <- write_out(p.o);
      if ((ii.fix.count %% 20) == 0) {p.o <- paste("\n"); wWw <- write_out(p.o);};

    ##details<<
    ## Look at each field.
    for (i.field in 1:rc.data[2]) {
      ##details<<
      ## If not finite, then it's a bad value to replace.
      if (!is.finite(TDL$data[i.bad.ind,i.field])) {
        ##details<<
        ## Replace bad value with previous value in the file.
        TDL$data[i.bad.ind,i.field] <- TDL$data[i.bad.ind-1,i.field];
      }
    }
  }
  p.o <- paste("\n"); wWw <- write_out(p.o);

  ## 3/23/2012 access TDL$data directly
  #for (i.bad.ind in bad.ind) {
  #  data.bad  <- TDL$data[i.bad.ind,];            # current bad line
  #  data.prev <- TDL$data[i.bad.ind-1,];          # previous line (that was not bad)
  #  for (i.field in 1:length(data.bad)) {         # look at each field
  #    if (!is.finite(data.bad[i.field])) {      # if not finite, then a bad value to replace
  #      data.bad[i.field] <- data.prev[i.field];  # replace with previous value in the file
  #    }
  #  }
  #  TDL$data[i.bad.ind,] <- data.bad;             # replace data with fixed data
  #}

  return( TDL );
  ### TDL
}

