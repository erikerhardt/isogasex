#' Title
#'
#' @param TDL.cycle
#'
#' @return
#' @export
#'
#' @examples
set_TDL_cycle <-
function# TDL.cycle variables
###
(TDL.cycle
###
)
{
  ##details<<
  ## \code{last.n.obs.*} is the number of observations to take to be \code{seconds.*} long.

          # last.n.obs is the number of observations to take to be seconds.* long
  TDL.cycle$last.n.obs.tank.hi      <- TDL.cycle$Hz * TDL.cycle$seconds.tank.hi  ;
  TDL.cycle$last.n.obs.tank.low     <- TDL.cycle$Hz * TDL.cycle$seconds.tank.low ;
  TDL.cycle$last.n.obs.reference    <- TDL.cycle$Hz * TDL.cycle$seconds.reference;
  TDL.cycle$last.n.obs.chamber      <- TDL.cycle$Hz * TDL.cycle$seconds.chamber  ;
  TDL.cycle$first.n.last.skip.chamber<- ceiling(TDL.cycle$Hz * TDL.cycle$seconds.exclude.first.chamber / TDL.cycle$last.n.obs.chamber); # number of last.list groups to skip as chamber equilbrates from reference
  TDL.cycle$table <-
    matrix(c(
       TDL.cycle$number.tank.hi     ,TDL.cycle$number.tank.low     ,TDL.cycle$number.reference     ,TDL.cycle$number.chamber
      ,TDL.cycle$last.n.obs.tank.hi ,TDL.cycle$last.n.obs.tank.low ,TDL.cycle$last.n.obs.reference ,TDL.cycle$last.n.obs.chamber
      ),ncol=2)
  TDL.cycle$table.name <- c(TDL.cycle$name.tank.hi       ,TDL.cycle$name.tank.low       ,TDL.cycle$name.reference       ,TDL.cycle$name.chamber);
  colnames(TDL.cycle$table) <- c("site","last.n.obs"); # name columns

  return( TDL.cycle );
  ### TDL.cycle
}

