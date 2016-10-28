# select_date(sdl_df, i_op, op_year, prv_date, meta_data, -----------
#                   tmp_df, pcp_df, mon_thrs)
select_date <- function(sdl_df, i_op, op_year, prv_date, meta_data, input_lst,
                        thrs, day_rnd, day_ssp, sel_type) {

  pcp_df  <- input_lst$precipitation
  tmp_df  <- input_lst$temperature_index
  api_lst <- input_lst$antecedent_precip

  if(sdl_df[7] == "p"){
    op_date <- convert_jdn2monday(prv_date, op_year, 0)
  } else {
    switch (sdl_df[7],
            "n" = {
              jdn_start <- sdl_df[3]
              jdn_end   <- sdl_df[4]
              day_rnd <- c(0,0)
              sel_wgt <- rep(1,(jdn_end-jdn_start+1))
            },
            # "o" = {
            #   jdn_init <- select_jdninit(tmp_df, sdl_df, op_year, i_op, 0, 0,
            #                              meta_data$SUB)
            #   jdn_start <- jdn_init
            #   jdn_end   <- jdn_init
            # },
            "<" = {
              jdn_init <- select_jdninit(tmp_df, sdl_df, op_year, i_op, -1, 0,
                                         meta_data$SUB)
              jdn_start <- jdn_init
              jdn_end   <- jdn_init
              sel_wgt <- compute_sampwghts(day_rnd, sel_type)
            },
            ">" = {
              jdn_init <- select_jdninit(tmp_df, sdl_df, op_year, i_op, 0, 1,
                                         meta_data$SUB)
              jdn_start <- jdn_init
              jdn_end   <- jdn_init
              sel_wgt <- compute_sampwghts(day_rnd, sel_type)
            }
    )

    pcp_date <- select_timespan(pcp_df, op_year, jdn_start, jdn_end,
                                day_rnd, meta_data$SUB, "PCP")
    pcp_sseq <- select_timespan(pcp_df, op_year, jdn_start, jdn_end,
                                c(day_rnd[1], (day_rnd[2] + day_ssp)),
                                meta_data$SUB, "PCP") %>%
      mutate(SSP = compute_subseqprecip(PCP, day_ssp)) %>%
      filter(JDN <= pcp_date$JDN) %>%
      select(-PCP)
    api_date <- select_timespan(api_lst[["SUB"%_%meta_data$SUB]], op_year,
                               jdn_start, jdn_end, day_rnd, meta_data$SOIL,
                               "API")

    op_date <- select_opdate(pcp_date, pcp_sseq, api_date, thrs, sel_wgt) %>%
      convert_jdn2monday(., op_year, prv_date)
  }

  return(op_date)
}


## Subfunctions -----------------------------------------------------------
select_timespan <- function(weather_df, op_year, jdn_1, jdn_2,
                            day_rnd, sel_label, col_label){
  sel_timespan <- weather_df %>%
    filter(., YEAR == op_year) %>%
    filter(., JDN >= (jdn_1 - day_rnd[1]) &
              JDN <= (jdn_2 + day_rnd[2])) %>%
    select(., JDN, ends_with(sel_label)) %>%
    rename_col(.,c("JDN", col_label))
}

select_jdninit <- function(temp_df, sdl_df, op_year, i_op, prev_mon, next_mon,
                           sel_label){
  tmp_op <- temp_df %>%
    filter(., YEAR == op_year) %>%
    filter(., MON >= (sdl_df[5] + prev_mon) &
              MON <= (sdl_df[6] + next_mon)) %>%
    select(., contains(sel_label)) %>%
    colMeans(na.rm = TRUE) %>%
    unname

  jdn_dates <- c(sdl_df[3],sdl_df[4])
  jdn_init  <- (mean(jdn_dates) +
                  diff(jdn_dates)*tmp_op*(prev_mon + next_mon)) %>%
    round(., digits = 0)
  return(jdn_init)
}
## select_opdate <- function(pcp_date, api_date, pcp_thrs, api_thrs)
##
select_opdate <- function(pcp_date, pcp_sseq, api_date, thrs, sel_wgt){

  pcp_sel <- pcp_date %>% filter(., PCP < thrs[1])
  ssp_sel <- pcp_sseq %>% filter(., SSP < thrs[2])
  api_sel <- api_date %>% cbind(., WGT = sel_wgt) %>%
                          filter(., API < thrs[3])
  op_date <- inner_join(pcp_sel, ssp_sel, by = "JDN") %>%
    inner_join(., api_sel, by = "JDN")

  if(dim(op_date)[1] > 0){
    op_date %<>%
      sample_n(., 1, weight = WGT) %>%
      select(., JDN)
  } else {
    wgt <- sum(thrs)/thrs*c(2,1,1) #PCP should have twice the priority to others
    op_date <- inner_join(pcp_date, pcp_sseq, by = "JDN") %>%
      inner_join(., api_date, by = "JDN")
    op_date %<>%
      mutate(., WGT = (wgt[1]*PCP + wgt[2]*SSP + wgt[3]*API)) %>%
      filter(., .$WGT == min(.$WGT, na.rm = TRUE)) %>%
      sample_n(., 1) %>%
      select(., JDN)
  }
  return(op_date)
}

compute_subseqprecip <- function(pcp_vec, n_day){
  n <- 1:n_day
  pcp_sub <- c()
  for (i in 1:length(pcp_vec)){
    pcp_sub <- c(pcp_sub, sum(pcp_vec[(i+1):(i+n_day)]/n, na.rm = TRUE))
  }
  return(pcp_sub)
}

compute_sampwghts <- function(day_rnd, sel_type){
  type <- substr(sel_type,1,4)
  sigma <- as.numeric(substr(sel_type,5,nchar(sel_type)))
  switch (type,
          "unif" = {rep(1,(sum(day_rnd)+1))},
          "norm" = {c((-day_rnd[1]:-1)/sigma,
                      (0:day_rnd[2])/sigma) %>%
                    dnorm}
  )
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



