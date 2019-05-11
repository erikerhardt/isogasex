f.val.calc.flow.adjusted <-
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
  ##\deqn{flow.adjusted <- flow * (1 - (H2OR / 1000))}
  flow.adjusted <- flow * (1 - (H2OR / 1000));

  return( flow.adjusted );
  ### flow.adjusted
}

