## line.init() ------------------------------------------------------------
## Function to initialize the line that will be written into the management file
## as empty data.frame.

line.init <- function() {
  data.frame(MON = NA, DAY = NA, HUSC = NA, OP = NA, OP_TYPE = NA,
             PAR1 = NA, PAR2 = NA, PAR3 = NA, PAR4 = NA, PAR5 = NA,
             PAR6 = NA, PAR7 = NA, PAR8 = NA )
}
