## initialize_line() ------------------------------------------------------------
## Function to initialize the line that will be written into the management file
## as empty data.frame.

initialize_line <- function() {
  data.frame(MON = NA, DAY = NA, HUSC = NA, OP = NA, MGT_1 = NA,
             MGT_2 = NA, MGT_3 = NA, MGT_4 = NA, MGT_5 = NA,
             MGT_6 = NA, MGT_7 = NA, MGT_8 = NA, MGT_9 = NA )
}
