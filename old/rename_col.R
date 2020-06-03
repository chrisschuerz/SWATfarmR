## rename_col(df, lbl) ----------------------------------------------------
## Function takes data.frame and labels the columns.
rename_col <- function(df, col_names){
  colnames(df) <- col_names
  return(df)
}
