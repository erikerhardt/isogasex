#' move selected plot files to subdirectories
#'
#' Create \file{BS} and \file{var} directories.
#'
#' Move \file{BS_CHECK*} files into \file{BS} directory.
#'
#' Move files that are not (TDL, Licor, offset) into \file{var} directory.
#'
#' @param plot_format xxxPARAMxxx
#'
#' @return NULL xxxRETURNxxx
#'
move_plot_files <-
function# move selected plot files to subdirectories
###
(plot_format
###
)
{
  ##details<<
  ## Create \file{BS} and \file{var} directories.
  dir.create("BS");
  dir.create("var");

  for (ii in plot_format) {
    if (ii == 1) { type <- ".png" ; };
    if (ii == 2) { type <- ".eps" ; };
    if (ii == 3) { type <- ".pdf" ; };
    if (ii == 4) { type <- ".bmp" ; };
    if (ii == 5) { type <- ".jpeg"; };
    if (ii == 6) { type <- ".tiff"; };

    ##details<<
    ## Move \file{BS_CHECK*} files into \file{BS} directory.
    plot_list <- list.files(pattern=type);               # jpeg files in out dir
    BS_list_all <- list.files(pattern="BS_CHECK");
    BS_list <- intersect(plot_list, BS_list_all);
    if (length(BS_list)) {
      for (i in 1:length(BS_list)) {
        file.rename(BS_list[i], paste("BS/",BS_list[i],sep=""));
      }
    }

    ##details<<
    ## Move files that are not (TDL, Licor, offset) into \file{var} directory.
    remaining_list <- list.files(pattern=type)               # jpeg files in out dir
    ind_remaining <- 1:length(remaining_list);               # assign indices
    ind_remaining_TDL    <- grep("TDL",    remaining_list);  # TDL indices
    ind_remaining_Licor  <- grep("Licor",  remaining_list);  # Licor indices
    ind_remaining_offset <- grep("offset", remaining_list);  # offset indices
    ind_nomove <- unique(c(ind_remaining_TDL, ind_remaining_Licor, ind_remaining_offset));  # files not to move
    ind_move <- setdiff(ind_remaining, ind_nomove);  # files to move
    for (i in ind_move) {
      file.rename(remaining_list[i], paste("var/",remaining_list[i],sep=""));
    }

  } # ii

  invisible(NULL);
  ### NULL
}

