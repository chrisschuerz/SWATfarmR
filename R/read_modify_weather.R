## read_weatherinput(file_name, format_int) -------------------------------
## Function reads the wheater data given in ascii format. The delimiter
## positions are given by the format integers: (length date, length values,
## variables per station).
read_weather <- function(file_name, value_format) {

  sub_num <- function(df, g1, g2){
    substr(df, g1 , g2) %>%
      as.numeric
  }

  dec_pos <- gregexpr("\\.", value_format)
  shift   <- c(dec_pos[[1]]-1, nchar(value_format) - dec_pos[[1]])
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

# modify_weatherinput(in_df, lookup, lbl) ---------------------------------
modify_weather <- function(weather_df, lookup, lbl){
  weather_df %>%
    assign.stat_to_sub(., unlist(lookup$station["I"%_%lbl]), "SUB") %>%
    mutate(., YEAR = as.numeric(substr(DATE,1,4)),
           JDN  = as.numeric(substr(DATE,5,7))) %>%
    select(., -DATE) %>%
    group_by(.,YEAR, JDN) %>%
    summarize_each(.,funs(sum_na.rm)) %>%
    ungroup(.) %>%
    mutate(., DATE = as.Date(JDN%/%YEAR, "%j/%Y"),
           MON  = as.numeric(substr(DATE, 6, 7)),
           DAY  = as.numeric(substr(DATE, 9, 10))) %>%
    select(., YEAR, MON, DAY, JDN, starts_with("SUB"))
}

trim.timeseries <- function(in_df, lookup){
  in_df %<>% filter(., YEAR >= lookup$bound_yrs[1],
                    YEAR <= lookup$bound_yrs[2])
  return(in_df)
}

# trim_timeseries(in_df, lookup) ------------------------------------------
trim_timeseries <- function(in_df, lookup){
  in_df %<>% filter(., YEAR >= lookup$bound_yrs[1],
                    YEAR <= lookup$bound_yrs[2])
  return(in_df)
}

## Subfunctions -----------------------------------------------------------
## sum_na.rm(val)
## calculates the mean excluding NA values. Required in this form for the
## summarize_each() command above
sum_na.rm  <- function(value) sum (value, na.rm = TRUE)

## assign_weatherstation(weather_df, station_lookup, col_label) -----------
assign_weatherstation <- function(weather_df, station_lookup, col_label){
  weather_df <- cbind(weather_df[,1],
                  weather_df[,(1 + stat_lookup)]) %>%
    rename_colheader(weather_df, col_label)
  return(weather_df)
}

## set.col.head(df, lbl) --------------------------------------------------
## Function takes data.frame with the structure: DATE, SUB_i and labels the
## columns.
rename_weatherheader <- function(df, lbl){
  n <- dim(df)[2] - 1
  df_head <- c("DATE", paste(rep(lbl, n),
                             sprintf("%03d",seq(1:n)),
                             sep = "_"))
  colnames(df) <- df_head
  return(df)
}
