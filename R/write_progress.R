#' write progress to the screen with time elapsed
#'
#' @param text_to_cat
#' @param time_start
#' @param type_print
#'
#' @return NULL
#'
#' @examples
write_progress <-
function# write_progress to the screen with time elapsed
###
(text_to_cat
###
, time_start
###
, type_print="cat"
###
)
{
  time_sofar <- progress_time(time_start);
  if (type_print == "cat") {
    cat("Progress:", time_sofar, "s ", text_to_cat);
    capture.output(expr = cat(sprintf(">%8.2f",time_sofar), text_to_cat), append=TRUE, file="process_info.txt");
  }
  if (type_print == "matrix") {
    print(text_to_cat);
    capture.output(expr = text_to_cat, append=TRUE, file="process_info.txt");
  }
  flush.console();

  invisible(NULL);
  ### NULL
} # write_progress()

