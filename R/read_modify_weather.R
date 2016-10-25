# read_weatherinput(file_name, format_int) --------------------------------
# Function reads the wheater data given in ascii format. The delimiter
# positions are given by the format integers: (length date, length values,
# variables per station).

read_weather <- function(file_name, value_format) {
  library(magrittr)
  #Position of the decimal point according to the given data format
  dec_pos <- gregexpr("\\.", value_format)[[1]]
  # positions before and after the decimal point for each value
  shift   <- c(dec_pos[[1]]-1, nchar(value_format) - dec_pos)
  # start position of the date in the data table
  date_strt <- 1
  # end position of the date in the data table
  date_end  <- 7

  # read the weather data into a data table
  weather_df <- fread(file_name, skip = 4, sep = "\n", header = FALSE)

  # Positions of decimal point, start and end of a value in each line of the
  # data table
  df_decpos   <- gregexpr("\\.", weather_df[,1])[[1]]
  df_valstart <- c(date_strt, df_decpos - shift[1])
  df_valend   <- c(date_end,  df_decpos + shift[2])

  subset_values <- function(df, value_startpos, value_endpos){
    substr(df, value_startpos , value_endpos) %>%
    as.numeric
  }

  weather_df %<>%
    mapply(subset_values,., df_valstart, df_valend) %>%
    as.data.frame()

  return(weather_df)

}

# modify_weatherinput(weather_df, lookup_lst, lbl_string) -----------------
modify_weather <- function(weather_df, lookup_lst, col_label){
  weather_df %>%
    assign_weatherstation(., lookup_lst, col_label) %>%
      mutate(., YEAR = as.numeric(substr(DATE,1,4)),
                JDN  = as.numeric(substr(DATE,5,7))) %>%
    select(., -DATE) %>%
    group_by(.,YEAR, JDN) %>%
    summarize_each(.,funs(sum_na.rm)) %>%
    ungroup(.) %>%
    mutate(., DATE = as.Date(JDN%//%YEAR, "%j/%Y"),
              MON = as.numeric(substr(DATE, 6, 7)),
              DAY  = as.numeric(substr(DATE, 9, 10))) %>%
    select(., YEAR, MON, DAY, JDN, starts_with("SUB"))
}

# limit_timespan(weather_df, lookup) ------------------------------------------
# Limit the weather data to the time span given in file.cio that is stored in
# the lookup list
limit_timespan <- function(weather_df, lookup_lst){
  weather_df %>% filter(., YEAR >= lookup_lst$bound_yrs[1],
                           YEAR <= lookup_lst$bound_yrs[2])
}

## Subfunctions -----------------------------------------------------------
# Subset numbers in read data table according to start and end positions of
# the values in each line of the data table
subset_values <- function(df, value_startpos, value_endpos){
  substr(df, value_startpos , value_endpos) %>%
    as.numeric
}

## sum_na.rm(val)
## calculates the mean excluding NA values. Required in this form for the
## summarize_each() command above
sum_na.rm  <- function(value) sum (value, na.rm = TRUE)

## assign_weatherstation(weather_df, station_lookup, col_label) -----------
assign_weatherstation <- function(weather_df, lookup_lst, col_label){
  station_index <- lookup_lst$station[["I"%_%col_label]]
  weather_df <- cbind(weather_df[,1],
                  weather_df[,(1 + station_index)]) %>%
    rename_weatherheader("SUB")
  return(weather_df)
}

## rename_weatherheader(df, lbl) ------------------------------------------
## Function takes data.frame with the structure: DATE, SUB_i and labels the
## columns.
rename_weatherheader <- function(weather_df, col_label){
  n <- dim(weather_df)[2] - 1
  col_header <- c("DATE", paste(col_label, sprintf("%03d",seq(1:n)),
                                sep = "_"))
  colnames(weather_df) <- col_header
  return(weather_df)
}
