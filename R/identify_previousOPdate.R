## previous.date(mgm_file, year_num) ----------------------------------
## Function extracts the date of the previous operation in the management file
## in JDN format.
previous.date <- function(mgmt_file, year_num) {
  prv_date <- NULL
  if(length(mgmt_file) > 30){
    prv_date <- mgmt_file %>%
      last(.) %>%
      substr(., 1, 6) %>%
      scan(text = ., what = " ", quiet = TRUE) %>%
      as.numeric(.) %>%
      sprintf("%02d", .)
    if (length(prv_date) > 0){
      prv_date <- paste(year_num,
                        prv_date[1],
                        prv_date[2],
                        sep = "-") %>%
        as.Date(.) %>%
        yday(.)
    }else prv_date = 0
  }else prv_date = 0
  return(prv_date)
}
