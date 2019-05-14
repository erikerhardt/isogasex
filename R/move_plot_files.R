#' Title
#'
#' @param plot.format
#'
#' @return
#' @export
#'
#' @examples
move_plot_files <-
function# move selected plot files to subdirectories
###
(plot.format
###
)
{
  ##details<<
  ## Create \file{BS} and \file{var} directories.
  dir.create("BS");
  dir.create("var");

  for (ii in plot.format) {
    if (ii == 1) { type <- ".png" ; };
    if (ii == 2) { type <- ".eps" ; };
    if (ii == 3) { type <- ".pdf" ; };
    if (ii == 4) { type <- ".bmp" ; };
    if (ii == 5) { type <- ".jpeg"; };
    if (ii == 6) { type <- ".tiff"; };

    ##details<<
    ## Move \file{BS_CHECK*} files into \file{BS} directory.
    plot.list <- list.files(pattern=type);               # jpeg files in out dir
    BS.list.all <- list.files(pattern="BS_CHECK");
    BS.list <- intersect(plot.list, BS.list.all);
    if (length(BS.list)) {
      for (i in 1:length(BS.list)) {
        file.rename(BS.list[i], paste("BS/",BS.list[i],sep=""));
      }
    }

    ##details<<
    ## Move files that are not (TDL, Licor, offset) into \file{var} directory.
    remaining.list <- list.files(pattern=type)               # jpeg files in out dir
    ind.remaining <- 1:length(remaining.list);               # assign indices
    ind.remaining.TDL    <- grep("TDL",    remaining.list);  # TDL indices
    ind.remaining.Licor  <- grep("Licor",  remaining.list);  # Licor indices
    ind.remaining.offset <- grep("offset", remaining.list);  # offset indices
    ind.nomove <- unique(c(ind.remaining.TDL, ind.remaining.Licor, ind.remaining.offset));  # files not to move
    ind.move <- setdiff(ind.remaining, ind.nomove);  # files to move
    for (i in ind.move) {
      file.rename(remaining.list[i], paste("var/",remaining.list[i],sep=""));
    }

  } # ii

  return( NULL );
  ### NULL
}

