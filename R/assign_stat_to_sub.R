## assign.stat_to_sub(var_df, stat_lookup, df_lbl) ------------------------
assign.stat_to_sub <- function(var_df, stat_lookup, df_lbl){
  var_df <- cbind(var_df[,1],
                  var_df[,(1 + stat_lookup)])
  var_df <- set.col.head(var_df, df_lbl)
  return(var_df)
}
