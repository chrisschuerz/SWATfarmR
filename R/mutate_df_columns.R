# Functions to mutate values or headers of data frame columns

## trim.col(df, col_nr) ---------------------------------------------------
## Function returns columns holding character strings w/o leading or
## trailing whitespace
trim.col <- function (df, col_nr){
  trim <- function(chr) gsub("^\\s+|\\s+$", "", chr)
  if(length(col_nr) == 0) stop("No columns selected!")
  if(length(col_nr) == 1){
    df[,col_nr] <- trim(df[,col_nr])
  } else {
    df[,col_nr] <-  apply(df[,col_nr], 2, trim)
  }
  return(df)
}

## set.col.head(df, lbl) --------------------------------------------------
## Function takes data.frame with the structure: DATE, SUB_i and labels the
## columns.
set.col.head <- function(df, lbl){
  n <- dim(df)[2] - 1
  df_head <- c("DATE", paste(rep(lbl, n),
                             sprintf("%03d",seq(1:n)),
                             sep = "_"))
  colnames(df) <- df_head
  return(df)
}

## rename.col(df, lbl) ----------------------------------------------------
## Function takes data.frame and labels the columns.
rename.col <- function(df, lbl){
  colnames(df) <- lbl
  return(df)
}
