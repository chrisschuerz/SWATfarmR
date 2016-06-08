# select_OPdate(sdl_df, i_op, op_year, prv_date, meta_data, -----------
#                   tmp_df, pcp_df, mon_thrs)
select_date <- function(sdl_df, i_op, op_year, prv_date, meta_data, input_lst,
                        pcp_thrs, api_thrs, ant_d, sub_d) {

  pcp_df  <- input_lst$precipitation
  tmp_df  <- input_lst$temperature$index
  api_lst <- input_lst$antecedent_precip

  if(sdl_df$DATE_RULE == "p"){
    op_date <- convert_jdn2monday(prv_date, op_year, 0)
  } else {
    switch (sdl_df$DATE_RULE,
            "n" = {
              jdn_start <- sdl_df$JDN1[i_op]
              jdn_end   <- sdl_df$JDN2[i_op]
              ant_days  <- 0
              sub_days  <- 0
            }
            "o" = {
              jdn_init <- select_jdninit(tmp_df, sdl_df, op_year, i_op, 0, 0,
                                         meta_data$SUB)
              jdn_start <- jdn_init
              jdn_end   <- jdn_init
              ant_days <- ant_d
              sub_days <- sub_d
            }
            "<" = {
              jdn_init <- select_jdninit(tmp_df, sdl_df, op_year, i_op, 1, 0,
                                         meta_data$SUB)
              jdn_start <- jdn_init
              jdn_end   <- jdn_init
              ant_days <- ant_d
              sub_days <- sub_d
            }
            ">" = {
              jdn_init <- select_jdninit(tmp_df, sdl_df, op_year, i_op, 0, 1,
                                         meta_data$SUB)
              jdn_start <- jdn_init
              jdn_end   <- jdn_init
              ant_days <- ant_d
              sub_days <- sub_d
            }
    )

    pcp_date <- select_timespan(pcp_df, sdl_df, op_year, jdn_start, jdn_end,
                                ant_days, sub_days, meta_data$SUB, "PCP")
    api_date <-select_timespan(api_lst[["SUB"%_%meta_data$SUB]],
                               sdl_df, op_year, jdn_start, jdn_end,
                               ant_days, sub_days, meta_data$SUB, "API")

    op_date <- select_opdate(pcp_date, api_date, pcp_thrs, api_thrs) %>%
      convert_jdn2monday(., op_year, prv_date)
  }

  return(op_date)
}


## Subfunctions -----------------------------------------------------------
select_timespan <- function(weather_df, sdl_df, op_year, jdn_1, jdn_2,
                            ant_d, sub_d, sel_label, col_label){
  sel_timespan <- weather_df %>%
    filter(., YEAR == op_year) %>%
    filter(., JDN >= (jdn_1 - ant_d) &
              JDN <= (jdn_2 + sub_d)) %>%
    select(., JDN, ends_with(comp_label)) %>%
    rename.col(.,c("JDN", col_label))
}

select_jdninit <- function(temp_df, sdl_df, op_year, i_op, prev_mon, next_mon,
                           sel_label){
  tmp_op <- temp_df %>%
    filter(., YEAR == op_year) %>%
    filter(., MON >= (sdl_df$MON_1[i_op] - prev_mon) &
              MON <= (sdl_df$MON_2[i_op] + next_mon)) %>%
    select(., contains(sel_label)) %>%
    mean(.[,1], na.rm = TRUE)

  jdn_dates <- c(sdl_df$JDN2[i_op],sdl_df$JDN1[i_op])
  jdn_init  <- (mean(jdn_dates) +
                  diff(jdn_dates)*tmp_op*(prev_mon + next_mon)) %>%
    round(., digits = 0)
  return(jdn_init)
}
## select_opdate <- function(pcp_date, api_date, pcp_thrs, amc_thrs)
##
select_opdate <- function(pcp_date, api_date, pcp_thrs, amc_thrs){
  pcp_sel <- pcp_date %>% filter(., PCP < pcp_thrs)
  amc_sel <- api_date %>% filter(., AMC < amc_thrs)
  op_date <- inner_join(pcp_sel, amc_sel, by = "JDN")

  if(dim(op_date)[1] > 0){
    op_date %<>%
      select(., JDN) %>%
      sample_n(., 1)
  } else {
    op_date <- inner_join(pcp_date, api_date, by = "JDN") %>%
      mutate(., WGT = 10*PCP + AMC) %>%
      filter(., .$WGT == min(.$WGT)) %>%
      select(., JDN)
  }
  return(op_date)
}


## convert_jdn2monday(jdn_num, year_num, prv_date)
## Function takes julian day and year and converts them to integer values of
## month and day. Additionally the date is compared to previous date. If the
## date is lower than the previous date then it is set to prev_date + 1 before
## conversion.
convert_jdn2monday <- function(jdn_num, year_num, prv_date) {
  if(jdn_num <= prv_date) jdn_num <- prv_date + 1

  paste(year_num,jdn_num, sep = "") %>%
    as.Date(., "%Y%j") %>%
    as.character(.) %>%
    strsplit(., "-") %>%
    unlist(.) %>%
    as.numeric(.) %>%
    .[2:3]
}

