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
              temp_max  = max(value, na.rm = TRUE))

  temp_index <- temp_df %>%
    left_join(temp_df_stat, by = "MON") %>%
    mutate_each(funs(. - temp_mean), starts_with("SUB")) %>%
    mutate_each(funs())
    select(-mean)

  temp_min_max <- temp_index %>%
    select(-YEAR) %>%
    melt(id.vars = "MON") %>%
    group_by(MON) %>%

  norm_min_max <- matrix(data = as.matrix(temp_index), nrow = 12)
  norm_min_max <- data.frame(MON = 1:12,
                             MIN = apply(norm_min_max, 1, min),
                             MAX = apply(norm_min_max, 1, max))
  norm_min_max <- norm_min_max[rep(1:12, lookup$n_years),]

  tmp_norm_pos <- tmp_norm
  tmp_norm_pos[tmp_norm_pos < 0 ] <- 0

  tmp_norm_neg <- tmp_norm
  tmp_norm_neg[tmp_norm_neg > 0 ] <- 0

  tmp_norm <- tmp_norm_pos / (2*abs(norm_min_max$MAX)) +
    tmp_norm_neg / (2*abs(norm_min_max$MIN))
  tmp_norm <- cbind(tmp_df[,1:2], tmp_norm)

  tmp_norm <- rename.col(tmp_norm,
                         c("YEAR", "MON",
                           "SUB"%_%sprintf("%03d",seq(1:lookup$n_subbasin))))
  return(tmp_norm)
}
