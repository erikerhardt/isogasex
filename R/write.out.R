write.out <-
function# write progress to the screen
###
(text.to.cat
###
)
{
  cat(text.to.cat);
  capture.output(expr = cat(text.to.cat), append=TRUE, file="process_info.txt");
  flush.console();

  return( NULL );
  ### NULL
} # write.out()

