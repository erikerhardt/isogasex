#' Title
#'
#' @param text.to.cat
#' @param time.start
#' @param type.print
#'
#' @return
#' @export
#'
#' @examples
write_progress <-
function# write_progress to the screen with time elapsed
###
(text.to.cat
###
, time.start
###
, type.print="cat"
###
)
{
  time.sofar <- progress_time(time.start);
  if (type.print == "cat") {
    cat("Progress:", time.sofar, "s ", text.to.cat);
    capture.output(expr = cat(sprintf(">%8.2f",time.sofar), text.to.cat), append=TRUE, file="process_info.txt");
  }
  if (type.print == "matrix") {
    cat("Progress:", time.sofar,"s \n");
    print(text.to.cat);
    capture.output(expr = text.to.cat, append=TRUE, file="process_info.txt");
  }
  flush.console();

  return( NULL );
  ### NULL
} # write_progress()

