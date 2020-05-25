library(tidyverse)
library(rlang)
library(pasta)

load_farmr("/home/christoph/Documents/projects/SWATfarmR/data/TxtInOut/raab.farm")
hru_attribute <- raab$.data$meta$hru_attributes

hru_attribute
i_hru <- 246
hru_attribute_i <- hru_attribute[i_hru,]

mgt_tbl_i <- mgt_codes %>%
  filter(land_use == hru_attribute$luse[i_hru]) %>%
  sample_management(.) %>%
  filter_static_rules(., hru_attribute_i) %>%
  select(-land_use, -management, -weight, -rules_static)

sample_management <- function(mgt_tbl) {
  mgt_sel <- mgt_tbl %>%
    group_by(management) %>%
    summarise(weight = max(weight, na.rm = TRUE)) %>%
    sample_n(., 1, weight = weight) %>%
    .$management

  mgt_tbl %>%
    filter(management == mgt_sel)
}

filter_static_rules <- function(mgt_tbl, hru_attribute_i) {
  sel_rule <- map_df(mgt_tbl$rules_static,
         ~transmute(hru_attribute_i, sel = !!parse_expr(.x))) %>%
    unlist()
  mgt_tbl[sel_rule,]
}

tbls <- raab$.data$variables

i <- 1
year_i <- 2000
sub_i <- hru_attribute_i$subbasin
prev_op <- NA
var_names <- map_chr(tbls, ~colnames(.x)[5]) %>%
  str_remove(., "_[:digit:]+$")

var_col_sel <- var_names%_%hru_attribute_i$subbasin

var_tbl <- tbls %>%
  map(., ~select(.x, year, month, day, jdn, any_of(var_col_sel))) %>%
  reduce(., left_join, by = c("year","month", "day", "jdn")) %>%
  set_names(., str_remove(colnames(.), "_[:digit:]+$")) %>%
  mutate(md = 100*month + day,
         ymd = year*1e4 + md,
         date = ymd(ymd),
         hu = NA,
         hu_fract = NA)

mgt_i <- mgt_tbl_i[i,]

schedule_op_i <- function(var_tbl, eval_str, yr, prev_op) {
  var_tbl %>%
    mutate(wgt = !!parse_expr(eval_str)) %>%
    sample_n(., 1, weight = wgt) %>%
    select(date, year, month, day)
}

compute_hu <- function(var_tbl, mgt_i, plant_dat, date_i) {
  op_i <- mgt_i$operation
  if(op_i == 1) {
    plant_i <- mgt_i$mgt1
    t_base <- filter(plant_dat, value == plant_i) %>% .$t_base
    phu <- mgt_i$mgt4
    var_tbl <- var_tbl %>%
      mutate(hu = ifelse(date >= date_i$date, tav - t_base, 0),
             hu = ifelse(hu > 0, hu, 0),
             hu = cumsum(hu),
             hu_fract = hu / phu)
  }

  if(op_i %in% c(5, 8)) {
    var_tbl <- var_tbl %>%
      mutate(hu = NA,
             hu_fract = NA)
  }

  return(var_tbl)
}

if(!is.na(prev_op)) {
  var_tbl <- filter(var_tbl, date >= prev_op)
}

date_i <- schedule_op_i(var_tbl, mgt_i$rules_dynamic, 2000, prev_op)
prev_op <- date_i$date
var_tbl <- compute_hu(var_tbl, mgt_i, lookup$plant, date_i)
