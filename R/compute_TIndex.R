# compute_TIndex(tmp_df, lookup) ------------------------------------------
compute_TIndex <- function (temp_df, lookup_lst) {
  # Aggregate data to monthly daily mean temperature values
  temp_df %<>%
    group_by(., YEAR, MON) %>%
    summarize_each(.,funs(mean(., na.rm = TRUE))) %>%
    select(., YEAR, MON, starts_with("SUB"))

  # Calculate normalized deviations to the monthly daily mean tempeartures
  # for each subbasin.
  temp_stat <- temp_df %>%
    ungroup %>%
    select(-YEAR) %>%
    melt(id.vars = "MON") %>%
    group_by(MON) %>%
    summarise(temp_mean = mean(value, na.rm = TRUE),
              temp_min  = min(value, na.rm = TRUE),
              temp_max  = max(value, na.rm = TRUE),
              temp_max_range = max(c((temp_mean - temp_min),
                                     (temp_max  - temp_mean))))

  temp_ind <- temp_df %>%
    left_join(temp_stat, by = "MON") %>%
    mutate_each(funs((. - temp_mean)/temp_max_range),
                     starts_with("SUB")) %>%
    select(-starts_with("temp_"))

  # temp_index <- list(index = temp_ind,
  #                    stat = temp_stat)
  return(temp_ind)
}
