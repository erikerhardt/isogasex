#' Title
#'
#' @param TDL_cycle
#'
#' @return
#' @export
#'
#' @examples
set_TDL_cycle <-
function# TDL_cycle variables
###
(TDL_cycle
###
)
{
  ##details<<
  ## \code{last_n_obs.*} is the number of observations to take to be \code{seconds.*} long.

          # last_n_obs is the number of observations to take to be seconds.* long
  TDL_cycle$last_n_obs_tank_hi      <- TDL_cycle$Hz * TDL_cycle$seconds_tank_hi  ;
  TDL_cycle$last_n_obs_tank_low     <- TDL_cycle$Hz * TDL_cycle$seconds_tank_low ;
  TDL_cycle$last_n_obs_reference    <- TDL_cycle$Hz * TDL_cycle$seconds_reference;
  TDL_cycle$last_n_obs_chamber      <- TDL_cycle$Hz * TDL_cycle$seconds_chamber  ;
  TDL_cycle$first_n_last_skip_chamber<- ceiling(TDL_cycle$Hz * TDL_cycle$seconds_exclude_first_chamber / TDL_cycle$last_n_obs_chamber); # number of last_list groups to skip as chamber equilbrates from reference
  TDL_cycle$table <-
    matrix(c(
       TDL_cycle$number_tank_hi     ,TDL_cycle$number_tank_low     ,TDL_cycle$number_reference     ,TDL_cycle$number_chamber
      ,TDL_cycle$last_n_obs_tank_hi ,TDL_cycle$last_n_obs_tank_low ,TDL_cycle$last_n_obs_reference ,TDL_cycle$last_n_obs_chamber
      ),ncol=2)
  TDL_cycle$table_name <- c(TDL_cycle$name_tank_hi       ,TDL_cycle$name_tank_low       ,TDL_cycle$name_reference       ,TDL_cycle$name_chamber);
  colnames(TDL_cycle$table) <- c("site","last_n_obs"); # name columns

  return( TDL_cycle );
  ### TDL_cycle
}

