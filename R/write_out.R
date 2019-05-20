#' write progress to the screen
#'
#' @param text_to_cat
#'
#' @return NULL
#' @export
#'
#' @examples
write_out <-
function# write_progress to the screen
###
(text_to_cat
###
)
{
  cat(text_to_cat);
  capture.output(expr = cat(text_to_cat), append=TRUE, file="process_info.txt");
  flush.console();

  invisible(NULL);
  ### NULL
} # write_out()

