## mgmt.format(v_out) -----------------------------------------------------
## Function to set the format of the management operations right for writing
## them to the text files. As input the vector of one line of mgt operations is
## given. Output is the formatted string.
mgmt.format <- function(v_out){
  options(warn=-1)
  empty_spc <- c("   ", "  ", "        ", "  ", "    ", "   ", "  ", "            ",
                 "      ", "           ", "    ", "      ", "     ")
  var_fmt <- c("%3.0f", "%2.0f", "%8.3f", "%2.0f", "%4.0f", "%3.0f", "%2.0f", "%12.5f",
               "%6.2f", "%11.5f", "%4.2f", "%6.2f", "%5.2f")
  v_out <- sprintf(var_fmt, v_out)
  v_out[grep("NA", v_out)] <- empty_spc[grep("NA", v_out)]

  str_out <- paste(v_out[1:13], collapse = " ")
  return(str_out)
}
