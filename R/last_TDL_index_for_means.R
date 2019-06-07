#' Determine the last index for admissible values to calculate means
#'
#' Rind indices just before where site change occurs.
#'
#' Error checking which sites are defined in template/TDL file but not the other.
#'
#' Determine sites in TDL file not in template file.
#'
#' Remove indices in TDL file not defined in the template (do not calculate for these).
#'
#' Determine sites in template file not in TDL file.
#'
#' Make sure the last TDL_cycle$table[,"last_n_obs"] are the same site as temp_last_ind.
#'
#' If chamber, then go back and keep calculating index blocks.
#'
#' For chamber, skip the first TDL_cycle$first_n_last_skip_chamber number of blocks.
#' Note that these are in reverse order, so the last rows in TDL$last_list are the first cycles.
#' Check that the last TDL_cycle$first_n_last_skip_chamber number of blocks are all chambers
#' and if so, then remove these cycles.
#'
#' Sort by index.
#'
#' @param TDL xxxPARAMxxx
#' @param TDL_cycle xxxPARAMxxx
#'
#' @return TDL xxxRETURNxxx
#'
last_TDL_index_for_means <-
function# Determine the last index for admissible values to calculate means
###
(TDL
###
, TDL_cycle
###
)
{

  ##details<<
  # Creates TDL$last_list to hold the index and site of the last measurements.
  TDL$last_list <- matrix(NA, nrow=0, ncol=3); # to hold the index and site of the last measurements
  colnames(TDL$last_list) <- c("first_ind","ind","site"); # name columns

  ##details<<
  ## Rind indices just before where site change occurs.
  temp_last_ind     <- (1:TDL$n)[!(TDL$data[1:TDL$n,"PrevSite"] == c(TDL$data[2:TDL$n,"PrevSite"],-1))];
  sites_in_TDL_file <- TDL$data[temp_last_ind,"PrevSite"];
  ##details<<
  ##   Error checking which sites are defined in template/TDL file but not the other.
  ##details<<
  ##     Determine sites in TDL file not in template file.
  sites_in_TDL_file_NOT_in_template <- unique(sites_in_TDL_file)[!(unique(sites_in_TDL_file) %in% TDL_cycle$table[,1])];
  if (length(sites_in_TDL_file_NOT_in_template)) {
    for (i_site in sites_in_TDL_file_NOT_in_template) {
      p_o <- paste("WARNING: TDL site number", i_site, "is in TDL file but not defined in the isogasex_template.xls file.\n"); write_out(p_o);
    }
    ##details<<
    ## Remove indices in TDL file not defined in the template (do not calculate for these).
    del_ind <- !(sites_in_TDL_file %in% sites_in_TDL_file_NOT_in_template);
    sites_in_TDL_file <- sites_in_TDL_file[del_ind];
    temp_last_ind <- temp_last_ind[del_ind];
  }
  ##details<<
  ##     Determine sites in template file not in TDL file.
  sites_in_template_NOT_in_TDL_file <- TDL_cycle$table[!(TDL_cycle$table[,1] %in% unique(sites_in_TDL_file)),1];
  if (length(sites_in_template_NOT_in_TDL_file)) {
    for (i_site in sites_in_template_NOT_in_TDL_file) {
      p_o <- paste("WARNING: TDL site number", i_site, "is in TDL file but not defined in the isogasex_template.xls file.\n"); write_out(p_o);
    }
  }

  ##details<<
  ## Make sure the last TDL_cycle$table[,"last_n_obs"] are the same site as temp_last_ind.
  for (i_site in 1:length(temp_last_ind)) {
    temp_site_num       <- sites_in_TDL_file[i_site]; # site number
    temp_site_last_ind  <- temp_last_ind[i_site];     # last index
    temp_site_num_calc  <- TDL_cycle$table[TDL_cycle$table[,"site"]==temp_site_num,"last_n_obs"];
    temp_site_first_ind <- temp_site_last_ind-temp_site_num_calc+1; # first index
    if (temp_site_first_ind < 0) {
      temp_site_match <- 0;  # if first measurement of TDL is shorter than number of last_n_obs  2/12/2012 11:37PM
    } else {
      temp_site_list      <- TDL$data[temp_site_first_ind:temp_site_last_ind,"PrevSite"]; # site list from first to last index
      temp_site_match     <- (sum(temp_site_list == temp_site_num) == temp_site_num_calc); # are all the measurements from the same site?
    }
    if (temp_site_match) {
      TDL$last_list <- rbind(TDL$last_list, c(temp_site_first_ind, temp_site_last_ind, temp_site_num));
    }
    ##details<<
    ## If chamber, then go back and keep calculating index blocks.
    if (temp_site_num == TDL_cycle$number_chamber) {
      temp_loop_sw <- 1;
      while((temp_loop_sw == 1) & (temp_site_first_ind > temp_site_num_calc)) {
        temp_site_last_ind  <- temp_site_last_ind  - temp_site_num_calc; # last index
        temp_site_first_ind <- temp_site_first_ind - temp_site_num_calc; # first index
        temp_site_list      <- TDL$data[temp_site_first_ind:temp_site_last_ind,"PrevSite"]; # site list from first to last index
        temp_site_match     <- (sum(temp_site_list == temp_site_num) == temp_site_num_calc); # are all the measurements from the same site?
        if (temp_site_match) {
          TDL$last_list <- rbind(TDL$last_list, c(temp_site_first_ind, temp_site_last_ind, temp_site_num));
        }
        temp_loop_sw <- temp_site_match; # continue to loop if last block was all chambers
      }
      ##details<<
      ## For chamber, skip the first TDL_cycle$first_n_last_skip_chamber number of blocks.
      ##   Note that these are in reverse order, so the last rows in TDL$last_list are the first cycles.
      ##   Check that the last TDL_cycle$first_n_last_skip_chamber number of blocks are all chambers
      ##   and if so, then remove these cycles.
      temp_del_last  <- length(TDL$last_list[,3]);
      temp_del_first <- temp_del_last - TDL_cycle$first_n_last_skip_chamber + 1;
      if ((temp_del_first > 0) & (i_site > 1)) {
        temp_keep_rows <- !(1:temp_del_last %in% temp_del_first:temp_del_last);
        TDL$last_list <- TDL$last_list[temp_keep_rows,];
      }
    }
  }

  ##details<<
  ## Sort by index.
  temp_order <- order(TDL$last_list[,1]);
  TDL$last_list <- TDL$last_list[temp_order,];

  ### Below replaced by above 10/4/2011 to improve speed
  # #
  # #   start at end of file, constructing means going back to front.
  # #   start at last value, see which site it is
  # #     determine last_n_obs.* for that site
  # #   count back last_n_obs.*
  # #     if same site, calculate mean and variance and store with last_time and site
  # #     else, walk backwards and find site change time and continue.
  # #   when hit beginning of file, end.
  # #   Sort in time.
  #
  # # list of indices for the last measurement time to take means
  # #   may be a different number of measurements for each site, depending on last_n_obs.* at top
  # TDL$last_list <- NULL; # to hold the index and site of the last measurements
  # current_ind <- TDL$n;
  # test_ind <- TDL$n;
  #
  # # list of sites in TDL file that are not in the xls file
  # sites_in_TDL_not_in_template <- NULL;
  #
  # while ((current_ind > 0) && (test_ind > 0)) {
  #   current_site <- TDL$data[current_ind,"PrevSite"];                               # site associated with current index
  #   if (sum(TDL_cycle$table[,1] == current_site) == 0) { # error checking
  #     if (!(current_site %in% sites_in_TDL_not_in_template)) {
  #       p_o <- paste("WARNING: TDL site number", current_site, "is in TDL file but not defined in the isogasex_template.xls file.\n"); write_out(p_o);
  #       sites_in_TDL_not_in_template <- c(sites_in_TDL_not_in_template, current_site);
  #     }
  #     #error.message <- paste("TDL site number", current_site, "is in TDL file but not defined in the isogasex_template.xls file. Check xls file site numbers.");
  #     #stop(error.message);
  #   }
  #
  #   current_last_n_obs <- TDL_cycle$table[(TDL_cycle$table[,1] == current_site),2]; # number of observations to look back for this site
  #   if (!length(current_last_n_obs)) { # if site in TDL file is not defined in isogasex_template.xls file, look back one site
  #     current_ind <- current_ind - 1;                                             # search back one index at a time
  #   } else {
  #     test_ind <- current_ind-current_last_n_obs+1;                                   # index to test whether same site
  #     test_site <- TDL$data[test_ind,"PrevSite"];                                     # site back this previous number of observations
  #     if ((current_site == test_site) && (test_ind > 0)) {
  #       TDL$last_list <- rbind(c(test_ind,current_ind,current_site),TDL$last_list);   # add index to index list
  #       current_ind <- current_ind - current_last_n_obs;                              # update current index
  #     }
  #     if ((current_site != test_site) && (test_ind > 0)) {
  #       test_site <- current_site;
  #       while ((current_site == test_site) && (current_ind > 0) && (test_ind > 0)) {
  #         current_ind <- current_ind - 1;                                             # search back one index at a time
  #         current_site <- TDL$data[current_ind,"PrevSite"];                           # do until this site is different
  #       };
  #     };
  #   }
  # }; # while
  # colnames(TDL$last_list) <- c("first_ind","ind","site"); # name columns
  #
  # # keep only the last tank_hi, tank_low, and reference measurement
  # # skip the first couple chamber measurements after reference (in TDL_cycle$seconds_exclude_first_chamber)
  # updated_last_list <- NULL;
  # for (i_list in 1:(dim(TDL$last_list)[1] - 1)) {
  #   # When to do nothing (any of these conditions):
  #   if (
  #     # if two tank_hi values, then do nothing
  #     ((TDL$last_list[i_list,"site"] == TDL_cycle$number_tank_hi) && (TDL$last_list[i_list+1,"site"] == TDL_cycle$number_tank_hi))
  #     ||
  #     # if two tank_low values, then do nothing
  #     ((TDL$last_list[i_list,"site"] == TDL_cycle$number_tank_low) && (TDL$last_list[i_list+1,"site"] == TDL_cycle$number_tank_low))
  #     ||
  #     # if two tank_reference values, then do nothing
  #     ((TDL$last_list[i_list,"site"] == TDL_cycle$number_reference) && (TDL$last_list[i_list+1,"site"] == TDL_cycle$number_reference))
  #     ||
  #     # if current is a chamber value and the value TDL_cycle$first_n_last_skip_chamber previous is not a chamber, then do nothing
  #     ((TDL$last_list[i_list,"site"] == TDL_cycle$number_chamber) && (TDL$last_list[max((i_list-TDL_cycle$first_n_last_skip_chamber),1),"site"] != TDL_cycle$number_chamber))
  #     )
  #   { 0; } # do nothing
  #   else # otherwise save the value
  #   { updated_last_list <- rbind(updated_last_list, TDL$last_list[i_list,]); };
  # }
  # TDL$last_list <- updated_last_list;
  # colnames(TDL$last_list) <- c("first_ind","ind","site"); # name columns
  #
  # unique_sites <- unique(TDL$last_list[,"site"])
  # not_in_TDL_sites <- (TDL_cycle$table[!(TDL_cycle$table[,1] %in% unique_sites),1])
  # if (length(not_in_TDL_sites)) {
  #   p_o <- paste("WARNING: template site number", not_in_TDL_sites, "is not in the TDL file.\n"); write_out(p_o);
  #   p_o <- paste("         ERRORS MAY OCCUR, CAUSING THE PROGRAM TO STOP RUNNING.\n"); write_out(p_o);
  # }

  return(TDL);
  ### TDL
}

