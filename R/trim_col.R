# trim_col(df, col_nr) ----------------------------------------------------
# Function returns columns holding character strings w/o leading or
# trailing whitespace
trim_col <- function (df, col_nr){
  trim <- function(chr) gsub("^\\s+|\\s+$", "", chr)
  if(length(col_nr) == 0) stop("No columns selected!")
  if(length(col_nr) == 1){
    df[,col_nr] <- trim(df[,col_nr])
  } else {
    df[,col_nr] <-  apply(df[,col_nr], 2, trim)
  }
  return(df)
}
