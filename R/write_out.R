#' write progress to the screen
#'
#' @param text_to_cat xxxPARAMxxx
#'
#' @return NULL xxxRETURNxxx
#' @importFrom utils capture.output flush.console
#'
write_out <-
function# write_progress to the screen
###
(text_to_cat
###
)
{
  cat(text_to_cat);
  utils::capture.output(expr = cat(text_to_cat), append=TRUE, file="process_info.txt");
  utils::flush.console();

  invisible(NULL);
  ### NULL
} # write_out()

