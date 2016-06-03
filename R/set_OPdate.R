## select.op.date <- function(pcp_date, amc_date, pcp_thrs, amc_thrs) -----
##
select.op.date <- function(pcp_date, amc_date, pcp_thrs, amc_thrs){
  pcp_sel <- pcp_date %>% filter(., PCP < pcp_thrs)
  amc_sel <- amc_date %>% filter(., AMC < amc_thrs)
  op_date <- inner_join(pcp_sel, amc_sel, by = "JDN")

  if(dim(op_date)[1] > 0){
    op_date %<>%
      select(., JDN) %>%
      sample_n(., 1)
  } else {
    op_date <- inner_join(pcp_date, amc_date, by = "JDN") %>%
      mutate(., WGT = 10*PCP + AMC) %>%
      filter(., .$WGT == min(.$WGT)) %>%
      select(., JDN)
  }
  return(op_date)
}
