#' A, TDL photosynthesis
#'
#' \deqn{A = (flow / 10^6) * (Ce - Co) / (leaf_area / 10^4)}
#'
#' @param flow xxxPARAMxxx
#' @param Ce xxxPARAMxxx
#' @param Co xxxPARAMxxx
#' @param leaf_area xxxPARAMxxx
#'
#' @return A xxxRETURNxxx
#'
f_val_calc_TDL_A_photosynthesis <-
function# A, TDL photosynthesis
###
(flow
###
, Ce
###
, Co
###
, leaf_area
###
)
{
  ##details<<
  ##\deqn{A = (flow / 10^6) * (Ce - Co) / (leaf_area / 10^4)}
  A <- (flow / 10^6) * (Ce - Co) / (leaf_area / 10^4);

  return( A );
  ### A
}

