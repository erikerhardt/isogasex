#' Title
#'
#' @param TDL
#' @param bad_ind
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
, bad_ind
###
)
{



  ii_fix_count <- 0;
  rc_data <- dim(TDL$data);
  for (i_bad_ind in bad_ind) {
      # counting
      ii_fix_count <- ii_fix_count + 1;
      p_o <- paste(ii_fix_count," "); wWw <- write_out(p_o);
      if ((ii_fix_count %% 20) == 0) {p_o <- paste("\n"); wWw <- write_out(p_o);};

    ##details<<
    ## Look at each field.
    for (i_field in 1:rc_data[2]) {
      ##details<<
      ## If not finite, then it's a bad value to replace.
      if (!is.finite(TDL$data[i_bad_ind,i_field])) {
        ##details<<
        ## Replace bad value with previous value in the file.
        TDL$data[i_bad_ind,i_field] <- TDL$data[i_bad_ind-1,i_field];
      }
    }
  }
  p_o <- paste("\n"); wWw <- write_out(p_o);

  ## 3/23/2012 access TDL$data directly
  #for (i_bad_ind in bad_ind) {
  #  data_bad  <- TDL$data[i_bad_ind,];            # current bad line
  #  data_prev <- TDL$data[i_bad_ind-1,];          # previous line (that was not bad)
  #  for (i_field in 1:length(data_bad)) {         # look at each field
  #    if (!is.finite(data_bad[i_field])) {      # if not finite, then a bad value to replace
  #      data_bad[i_field] <- data_prev[i_field];  # replace with previous value in the file
  #    }
  #  }
  #  TDL$data[i_bad_ind,] <- data_bad;             # replace data with fixed data
  #}

  return( TDL );
  ### TDL
}

