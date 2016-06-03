## read.weather_input(file_str, format_int) -------------------------------
## Function reads the wheater data given in ascii format. The delimiter
## positions are given by the format integers: (length date, length values,
## variables per station).
read.weather_input <- function(file_name, val_format) {

  sub_num <- function(df, g1, g2){
    substr(df, g1 , g2) %>%
      as.numeric
  }

  dec_pos <- gregexpr("\\.", val_format)
  shift   <- c(dec_pos[[1]]-1, nchar(val_format) - dec_pos[[1]])
  date_strt <- 1
  date_end  <- 7
  df <- fread(file_name, skip = 4, sep = "\n", header = FALSE)
  g <- gregexpr("\\.", df[1])[[1]]
  g1 <- c(date_strt, g - shift[1])
  g2 <- c(date_end, g + shift[2])
  df %<>%
    mapply(sub_num,., g1, g2) %>%
    as.data.frame
  return(df)
}
