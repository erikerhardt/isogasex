write.progress <-
function# write progress to the screen with time elapsed
###
(text.to.cat
###
, time.start
###
, type.print="cat"
###
)
{
  time.sofar <- progress.time(time.start);
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
} # write.progress()

