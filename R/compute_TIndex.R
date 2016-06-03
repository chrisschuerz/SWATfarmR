# compute_TIndex(tmp_df, lookup) ------------------------------------------
compute_TIndex <- function (tmp_df, lookup) {
  # Aggregate data to monthly daily mean temperature values
  tmp_df %<>%
    group_by(., YEAR, MON) %>%
    summarize_each(.,funs(mean_na.rm)) %>%
    select(., YEAR, MON, starts_with("SUB"))

  # Calculate normalized deviations to the monthly daily mean tempeartures
  # for each subbasin.
  tmp_df_stat <- aggregate(x = tmp_df,
                           by = list(tmp_df$MON),
                           FUN = mean.na_rm)
  tmp_df_stat <- rowMeans(tmp_df_stat[,4:dim(tmp_df_stat)[2]])

  tmp_norm <- tmp_df[, 3:dim(tmp_df)[2]] - tmp_df_stat[rep(1:12, lookup$n_years)]

  norm_min_max <- matrix(data = as.matrix(tmp_norm), nrow = 12)
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

## Subfunctions -----------------------------------------------------------
## mean_na.rm(val)
## calculates the mean excluding NA values. Required in this form for the
## summarize_each() command above
mean_na.rm <- function(value) mean(value, na.rm = TRUE)
