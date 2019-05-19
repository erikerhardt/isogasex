#' Title
#'
#' @param TDL
#' @param TDL.cycle
#'
#' @return
#' @export
#'
#' @examples
last_TDL_index_for_means <-
function# Determine the last index for admissible values to calculate means
###
(TDL
###
, TDL.cycle
###
)
{

  ##details<<
  # Creates TDL$last.list to hold the index and site of the last measurements.
  TDL$last.list <- matrix(NA, nrow=0, ncol=3); # to hold the index and site of the last measurements
  colnames(TDL$last.list) <- c("first.ind","ind","site"); # name columns

  ##details<<
  ## Rind indices just before where site change occurs.
  temp.last.ind     <- (1:TDL$n)[!(TDL$data[1:TDL$n,"PrevSite"] == c(TDL$data[2:TDL$n,"PrevSite"],-1))];
  sites.in.TDL.file <- TDL$data[temp.last.ind,"PrevSite"];
  ##details<<
  ##   Error checking which sites are defined in template/TDL file but not the other.
  ##details<<
  ##     Determine sites in TDL file not in template file.
  sites.in.TDL.file.NOT.in.template <- unique(sites.in.TDL.file)[!(unique(sites.in.TDL.file) %in% TDL.cycle$table[,1])];
  if (length(sites.in.TDL.file.NOT.in.template)) {
    for (i.site in sites.in.TDL.file.NOT.in.template) {
      p.o <- paste("WARNING: TDL site number", i.site, "is in TDL file but not defined in the isogasex_template.xls file.\n"); wWw <- write_out(p.o);
    }
    ##details<<
    ## Remove indices in TDL file not defined in the template (do not calculate for these).
    del.ind <- !(sites.in.TDL.file %in% sites.in.TDL.file.NOT.in.template);
    sites.in.TDL.file <- sites.in.TDL.file[del.ind];
    temp.last.ind <- temp.last.ind[del.ind];
  }
  ##details<<
  ##     Determine sites in template file not in TDL file.
  sites.in.template.NOT.in.TDL.file <- TDL.cycle$table[!(TDL.cycle$table[,1] %in% unique(sites.in.TDL.file)),1];
  if (length(sites.in.template.NOT.in.TDL.file)) {
    for (i.site in sites.in.template.NOT.in.TDL.file) {
      p.o <- paste("WARNING: TDL site number", i.site, "is in TDL file but not defined in the isogasex_template.xls file.\n"); wWw <- write_out(p.o);
    }
  }

  ##details<<
  ## Make sure the last TDL.cycle$table[,"last.n.obs"] are the same site as temp.last.ind.
  for (i.site in 1:length(temp.last.ind)) {
    temp.site.num       <- sites.in.TDL.file[i.site]; # site number
    temp.site.last.ind  <- temp.last.ind[i.site];     # last index
    temp.site.num.calc  <- TDL.cycle$table[TDL.cycle$table[,"site"]==temp.site.num,"last.n.obs"];
    temp.site.first.ind <- temp.site.last.ind-temp.site.num.calc+1; # first index
    if (temp.site.first.ind < 0) {
      temp.site.match <- 0;  # if first measurement of TDL is shorter than number of last.n.obs  2/12/2012 11:37PM
    } else {
      temp.site.list      <- TDL$data[temp.site.first.ind:temp.site.last.ind,"PrevSite"]; # site list from first to last index
      temp.site.match     <- (sum(temp.site.list == temp.site.num) == temp.site.num.calc); # are all the measurements from the same site?
    }
    if (temp.site.match) {
      TDL$last.list <- rbind(TDL$last.list, c(temp.site.first.ind, temp.site.last.ind, temp.site.num));
    }
    ##details<<
    ## If chamber, then go back and keep calculating index blocks.
    if (temp.site.num == TDL.cycle$number.chamber) {
      temp.loop.sw <- 1;
      while((temp.loop.sw == 1) & (temp.site.first.ind > temp.site.num.calc)) {
        temp.site.last.ind  <- temp.site.last.ind  - temp.site.num.calc; # last index
        temp.site.first.ind <- temp.site.first.ind - temp.site.num.calc; # first index
        temp.site.list      <- TDL$data[temp.site.first.ind:temp.site.last.ind,"PrevSite"]; # site list from first to last index
        temp.site.match     <- (sum(temp.site.list == temp.site.num) == temp.site.num.calc); # are all the measurements from the same site?
        if (temp.site.match) {
          TDL$last.list <- rbind(TDL$last.list, c(temp.site.first.ind, temp.site.last.ind, temp.site.num));
        }
        temp.loop.sw <- temp.site.match; # continue to loop if last block was all chambers
      }
      ##details<<
      ## For chamber, skip the first TDL.cycle$first.n.last.skip.chamber number of blocks.
      ##   Note that these are in reverse order, so the last rows in TDL$last.list are the first cycles.
      ##   Check that the last TDL.cycle$first.n.last.skip.chamber number of blocks are all chambers
      ##   and if so, then remove these cycles.
      temp.del.last  <- length(TDL$last.list[,3]);
      temp.del.first <- temp.del.last - TDL.cycle$first.n.last.skip.chamber + 1;
      if ((temp.del.first > 0) & (i.site > 1)) {
        temp.keep.rows <- !(1:temp.del.last %in% temp.del.first:temp.del.last);
        TDL$last.list <- TDL$last.list[temp.keep.rows,];
      }
    }
  }

  ##details<<
  ## Sort by index.
  temp.order <- order(TDL$last.list[,1]);
  TDL$last.list <- TDL$last.list[temp.order,];

  ### Below replaced by above 10/4/2011 to improve speed
  # #
  # #   start at end of file, constructing means going back to front.
  # #   start at last value, see which site it is
  # #     determine last.n.obs.* for that site
  # #   count back last.n.obs.*
  # #     if same site, calculate mean and variance and store with last.time and site
  # #     else, walk backwards and find site change time and continue.
  # #   when hit beginning of file, end.
  # #   Sort in time.
  #
  # # list of indices for the last measurement time to take means
  # #   may be a different number of measurements for each site, depending on last.n.obs.* at top
  # TDL$last.list <- NULL; # to hold the index and site of the last measurements
  # current.ind <- TDL$n;
  # test.ind <- TDL$n;
  #
  # # list of sites in TDL file that are not in the xls file
  # sites.in.TDL.not.in.template <- NULL;
  #
  # while ((current.ind > 0) && (test.ind > 0)) {
  #   current.site <- TDL$data[current.ind,"PrevSite"];                               # site associated with current index
  #   if (sum(TDL.cycle$table[,1] == current.site) == 0) { # error checking
  #     if (!(current.site %in% sites.in.TDL.not.in.template)) {
  #       p.o <- paste("WARNING: TDL site number", current.site, "is in TDL file but not defined in the isogasex_template.xls file.\n"); wWw <- write_out(p.o);
  #       sites.in.TDL.not.in.template <- c(sites.in.TDL.not.in.template, current.site);
  #     }
  #     #error.message <- paste("TDL site number", current.site, "is in TDL file but not defined in the isogasex_template.xls file. Check xls file site numbers.");
  #     #stop(error.message);
  #   }
  #
  #   current.last.n.obs <- TDL.cycle$table[(TDL.cycle$table[,1] == current.site),2]; # number of observations to look back for this site
  #   if (!length(current.last.n.obs)) { # if site in TDL file is not defined in isogasex_template.xls file, look back one site
  #     current.ind <- current.ind - 1;                                             # search back one index at a time
  #   } else {
  #     test.ind <- current.ind-current.last.n.obs+1;                                   # index to test whether same site
  #     test.site <- TDL$data[test.ind,"PrevSite"];                                     # site back this previous number of observations
  #     if ((current.site == test.site) && (test.ind > 0)) {
  #       TDL$last.list <- rbind(c(test.ind,current.ind,current.site),TDL$last.list);   # add index to index list
  #       current.ind <- current.ind - current.last.n.obs;                              # update current index
  #     }
  #     if ((current.site != test.site) && (test.ind > 0)) {
  #       test.site <- current.site;
  #       while ((current.site == test.site) && (current.ind > 0) && (test.ind > 0)) {
  #         current.ind <- current.ind - 1;                                             # search back one index at a time
  #         current.site <- TDL$data[current.ind,"PrevSite"];                           # do until this site is different
  #       };
  #     };
  #   }
  # }; # while
  # colnames(TDL$last.list) <- c("first.ind","ind","site"); # name columns
  #
  # # keep only the last tank.hi, tank.low, and reference measurement
  # # skip the first couple chamber measurements after reference (in TDL.cycle$seconds.exclude.first.chamber)
  # updated.last.list <- NULL;
  # for (i.list in 1:(dim(TDL$last.list)[1] - 1)) {
  #   # When to do nothing (any of these conditions):
  #   if (
  #     # if two tank.hi values, then do nothing
  #     ((TDL$last.list[i.list,"site"] == TDL.cycle$number.tank.hi) && (TDL$last.list[i.list+1,"site"] == TDL.cycle$number.tank.hi))
  #     ||
  #     # if two tank.low values, then do nothing
  #     ((TDL$last.list[i.list,"site"] == TDL.cycle$number.tank.low) && (TDL$last.list[i.list+1,"site"] == TDL.cycle$number.tank.low))
  #     ||
  #     # if two tank.reference values, then do nothing
  #     ((TDL$last.list[i.list,"site"] == TDL.cycle$number.reference) && (TDL$last.list[i.list+1,"site"] == TDL.cycle$number.reference))
  #     ||
  #     # if current is a chamber value and the value TDL.cycle$first.n.last.skip.chamber previous is not a chamber, then do nothing
  #     ((TDL$last.list[i.list,"site"] == TDL.cycle$number.chamber) && (TDL$last.list[max((i.list-TDL.cycle$first.n.last.skip.chamber),1),"site"] != TDL.cycle$number.chamber))
  #     )
  #   { 0; } # do nothing
  #   else # otherwise save the value
  #   { updated.last.list <- rbind(updated.last.list, TDL$last.list[i.list,]); };
  # }
  # TDL$last.list <- updated.last.list;
  # colnames(TDL$last.list) <- c("first.ind","ind","site"); # name columns
  #
  # unique.sites <- unique(TDL$last.list[,"site"])
  # not.in.TDL.sites <- (TDL.cycle$table[!(TDL.cycle$table[,1] %in% unique.sites),1])
  # if (length(not.in.TDL.sites)) {
  #   p.o <- paste("WARNING: template site number", not.in.TDL.sites, "is not in the TDL file.\n"); wWw <- write_out(p.o);
  #   p.o <- paste("         ERRORS MAY OCCUR, CAUSING THE PROGRAM TO STOP RUNNING.\n"); wWw <- write_out(p.o);
  # }

  return(TDL);
  ### TDL
}

