#' Adjusted flow
#'
#' Deprecated (0.1-20, 9/5/2012
#'
#' This is no longer used since the correction is not required.
#'
#' LI6400 flow includes water in the entering air but the TDL removes this water before measuring and this effectively reduces the flow (use H2OR to correct it).}
#'
#' \deqn{flow_adjusted <- flow * (1 - (H2OR / 1000))}
#'
#' @param flow
#' @param H2OR
#'
#' @return flow_adjusted
#'
#' @examples
f_val_calc_flow_adjusted <-
function# Adjusted flow
### Deprecated (0.1-20, 9/5/2012)
###
### This is no longer used since the correction is not required.
###
### LI6400 flow includes water in the entering air but the TDL removes this water before measuring and this effectively reduces the flow (use H2OR to correct it).
(flow
###
, H2OR
###
)
{
  ##details<<
  ##\deqn{flow_adjusted <- flow * (1 - (H2OR / 1000))}
  flow_adjusted <- flow * (1 - (H2OR / 1000));

  return( flow_adjusted );
  ### flow_adjusted
}

