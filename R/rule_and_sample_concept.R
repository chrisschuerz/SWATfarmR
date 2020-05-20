library(tidyverse)
library(pasta)
library(rlang)
tbls <- raab$.data$variables

year_i <- 2000
sub_i <- 1
var_names <- map_chr(tbls, ~colnames(.x)[5]) %>%
  str_remove(., "_[:digit:]+$")

var_col_sel <- var_names%_%sub_i

rule_str <- "(jdn >= 39) * (jdn <= 41) * (1- lin_wgt(pcp, 1, 10))"

fun <- function(tbls_lst, eval_str, yr, sb) {
  var_names <- map_chr(tbls_lst, ~colnames(.x)[5]) %>%
    str_remove(., "_[:digit:]+$")

  var_col_sel <- var_names%_%sb

  tbls_lst %>%
    map(., ~filter(.x, year %in% c(yr))) %>%
    map(., ~select(.x, year, month, day, jdn, any_of(var_col_sel))) %>%
    reduce(., left_join, by = c("year","month", "day", "jdn")) %>%
    set_names(., str_remove(colnames(.), "_[:digit:]+$")) %>%
    mutate(mmdd = 100*month + day) %>%
    mutate(new_col = !!parse_quosure(rule_str)) %>%
    sample_n(., 1, weight = new_col)
}

lin_wgt <- function(x, lw_bd, up_bd) {
  (x >= up_bd) + (x < up_bd & x > lw_bd) * (x - lw_bd) / (up_bd - lw_bd)
}

logit_wgt <- function(x, lwr, upr) {
  k   <- 9.19024 / (upr - lwr)
  x_0 <- upr/2 + lwr/2
  1 / (1 + exp(-k*(x - x_0)))
}

x <- seq(0,12,0.01)
lg <- 1 - logit_rng(x, 2,10)
lin <- 1 - lin_wgt(x, 2, 10)


a <- map_dbl(1:100, ~fun(tbls, rule_str, year_i, 1)[[1,5]])
