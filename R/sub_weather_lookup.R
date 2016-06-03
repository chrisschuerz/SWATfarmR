## sub.weather.lookup() ---------------------------------------------------
## Function to create the station lookup table for the subbasins found in the
## TxtIO directory. For running the function TxtIO must be set as working
## directory. The output is a data.frame holding the corresponding pcp and tmp
## stations to each subbasin.
sub.weater.lookup <- function(sub_path) {
  sub_list <- file.names(file_path = sub_path, pat_str = "\\.sub$")
  stat_lookup <- data.frame(SUB   = character(),
                            I_PCP = numeric(),
                            I_TMP = numeric(),
                            I_LAT = numeric())
  for (i in sub_list){
    temp  <- readLines(sub_path%/%i, warn = FALSE)
    i_sub <- scan(text = temp[1], what = "", quiet = TRUE)[4]
    i_pcp <- scan(text = temp[7], what = "", quiet = TRUE)[1]
    i_tmp <- scan(text = temp[8], what = "", quiet = TRUE)[1]
    i_lat <- scan(text = temp[5], what = "", quiet = TRUE)[1]

    stat_lookup <- rbind(stat_lookup,
                         data.frame(SUB   = as.numeric(i_sub),
                                    I_PCP = as.numeric(i_pcp),
                                    I_TMP = as.numeric(i_tmp),
                                    I_LAT = as.numeric(i_lat)))
  }
  return(stat_lookup)
}
