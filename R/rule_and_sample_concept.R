library(tidyverse)
library(pasta)
library(rlang)
tbls <- raab$.data$variables

year_i <- 2000
sub_i <- 2
var_names <- map_chr(tbls, ~colnames(.x)[5]) %>%
  str_remove(., "_[:digit:]+$")

var_col_sel <- var_names%_%sub_i

rule_str <- "(month <= 2) * (pcp == 0)"

tbls %>%
  map(., ~filter(.x, year %in% c(year_i, year_i + 1))) %>%
  map(., ~select(.x, year, month, day, jdn, any_of(var_col_sel))) %>%
  reduce(., left_join, by = c("year","month", "day", "jdn")) %>%
  set_names(., str_remove(colnames(.), "_[:digit:]+$")) %>%
  mutate(mmdd = 100*month + day) %>%
  mutate(new_col = !!parse_quosure(rule_str)) %>%
  sample_n(., 1, weight = new_col)
