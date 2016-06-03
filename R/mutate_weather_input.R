## mutate.weather_input(in_df, lookup, lbl) -------------------------------
mutate.weather_input <- function(in_df, lookup, lbl){
  in_df %>%
    assign.stat_to_sub(., unlist(lookup$station["I"%_%lbl]), "SUB") %>%
    mutate(., YEAR = as.numeric(substr(DATE,1,4)),
           JDN  = as.numeric(substr(DATE,5,7))) %>%
    select(., -DATE) %>%
    group_by(.,YEAR, JDN) %>%
    summarize_each(.,funs(sum.na_rm)) %>%
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
