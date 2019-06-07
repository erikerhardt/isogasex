#' write progress to the screen with time elapsed
#'
#' @param text_to_cat xxxPARAMxxx
#' @param time_start xxxPARAMxxx
#' @param type_print xxxPARAMxxx
#'
#' @return NULL xxxRETURNxxx
#' @importFrom utils capture.output
#'
write_progress <-
function# write_progress to the screen with time elapsed
###
(text_to_cat
###
, time_start
###
, type_print= c("cat", "matrix")
###
)
{
  time_sofar <- progress_time(time_start);
  if (type_print == "cat") {
    cat("Progress:", time_sofar, "s ", text_to_cat);
    utils::capture.output(expr = cat(sprintf(">%8.2f",time_sofar), text_to_cat), append=TRUE, file="process_info.txt");
  }
  if (type_print == "matrix") {
    print(text_to_cat);
    utils::capture.output(expr = text_to_cat, append=TRUE, file="process_info.txt");
  }
  flush.console();

  invisible(NULL);
  ### NULL
} # write_progress()

