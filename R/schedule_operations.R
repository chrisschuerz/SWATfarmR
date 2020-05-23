hru_attribute
i_hru <- 246
hru_attribute_i <- hru_attribute[i_hru,]

mgt_tbl_i <- a
mgt_codes %>%
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
         ~transmute(hru_attribute_i, sel = !!parse_quosure(.x))) %>%
    unlist()
  mgt_tbl[sel_rule,]
}
