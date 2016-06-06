# format_mgtstringout(mgt_line) -------------------------------------------
# Function to set the format of the management operations right for writing
# them to the text files. As input the vector of one line of mgt operations
# is given. Output is the formatted string.
format_mgtstringout <- function(mgt_line){
  options(warn=-1)
  empty_spc <- c("   ", "  ", "        ", "  ", "    ", "   ", "  ", "            ",
                 "      ", "           ", "    ", "      ", "     ")
  var_format <- c("%3.0f", "%2.0f", "%8.3f", "%2.0f", "%4.0f", "%3.0f", "%2.0f",
                  "%12.5f", "%6.2f", "%11.5f", "%4.2f", "%6.2f", "%5.2f")
  mgt_line <- sprintf(var_fmt, mgt_line)
  mgt_line[grep("NA", mgt_line)] <- empty_spc[grep("NA", mgt_line)]

  string_out <- paste(mgt_line[1:13], collapse = " ")
  return(string_out)
}
