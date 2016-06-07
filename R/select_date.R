# select_OPdate(sdl_df, i_op, op_year, prv_date, meta_data, -----------
#                   tmp_df, pcp_df, mon_thrs)
select_date <- function(sdl_df, i_op, op_year, prv_date, meta_data, input_lst,
                        mon_thrs, thrs_rule, pcp_thrs, api_thrs) {

  pcp_df  <- input_lst$precipitation
  tmp_df  <- input_lst$temperature$index
  api_lst <- input_lst$antecedent_precip

  # Routine to avoid errors if previous line is empty or NA
  same_date = FALSE
  if(length(sdl_df$JDN1[i_op - 1]) != 0 ) {
    if(!is.na(sdl_df$JDN1[i_op - 1])) {
      if((sdl_df$JDN1[i_op] == sdl_df$JDN1[i_op-1]) &
         (sdl_df$JDN2[i_op] == sdl_df$JDN2[i_op-1])) {
        same_date = TRUE
      }
    }
  }

  if(same_date) {
    op_date <- convert_jdn2monday(prv_date, op_year, 0)
  } else if (do.call(thrs_rule, list(sdl_df$MON_1[i_op], mon_thrs))) {

    if(thrs_rule == "<") {
      prev_mon <- -1
      next_mon <-  0
    } else {
      prev_mon <-  0
      next_mon <-  1
    }
    tmp_op <- tmp_df %>%
      filter(., YEAR == op_year) %>%
      filter(., MON >= (sdl_df$MON_1[i_op] + prev_mon) &
               MON <= (sdl_df$MON_2[i_op] + next_mon)) %>%
      select(., contains(meta_data$SUB)) %>%
      .[,1] %>%
      mean(., na.rm = TRUE)

    jdn_dates <- c(sdl_df$JDN2[i_op],sdl_df$JDN1[i_op])
    jdn_init  <- (mean(jdn_dates) +
                    diff(jdn_dates)*tmp_op*(prev_mon + next_mon)) %>%
      round(., digits = 0)

    pcp_date <- pcp_df %>%
      filter(., YEAR == op_year) %>%
      filter(., JDN >= (jdn_init - 5) &
               JDN <= (jdn_init + 5)) %>%
      select(., JDN, contains(meta_data$SUB)) %>%
      rename.col(.,c("JDN", "PCP"))

    api_date <- api_lst %>%
      .[["SUB"%_%meta_data$SUB]] %>%
      filter(., YEAR == op_year) %>%
      filter(., JDN >= (jdn_init - 5) &
               JDN <= (jdn_init + 5)) %>%
      select(., JDN, ends_with(meta_data$SOIL)) %>%
      rename.col(.,c("JDN", "AMC"))

    op_date <- select_opdate(pcp_date, api_date, 3, 25) %>%
      jdn.to.monday(., op_year, prv_date)
  } else {
    pcp_date <- pcp_df %>%
      filter(., YEAR == op_year) %>%
      filter(., JDN >= (sdl_df$JDN1[i_op] - 5) &
               JDN <= (sdl_df$JDN2[i_op] + 5)) %>%
      select(., JDN, contains(meta_data$SUB)) %>%
      rename.col(.,c("JDN", "PCP"))

    api_date <- api_lst %>%
      .[["SUB"%_%meta_data$SUB]] %>%
      filter(., YEAR == op_year) %>%
      filter(., JDN >= (sdl_df$JDN1[i_op] - 5) &
               JDN <= (sdl_df$JDN2[i_op] + 5)) %>%
      select(., JDN, ends_with(meta_data$SOIL)) %>%
      rename.col(.,c("JDN", "AMC"))

    op_date <- select_opdate(pcp_date, api_date, 5, 25) %>%
      jdn.to.monday(., op_year, prv_date)
  }
  return(op_date)
}



## Subfunctions -----------------------------------------------------------
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

## jdn.to.monday(jdn_num, year_num, prv_date)
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

