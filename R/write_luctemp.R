#' Write a template for the lup table of update_landuse()
#'
#' @param lup_path Either path string to write a .csv or NULL. Then the
#'   template structure is returned as a data.frame
#'
#' @return
#' @export
#'
#' @examples
#' # To get a template for the lup_tbl
#'
#' write_luptemp(choose.dir()) # only windows
#' write_luptemp() #Returns tibble to .Globalenv
write_luptemp <- function(lup_path = NULL) {
  lup_tmp <- data.frame(luse_from = c("FESC", "FESC", "FRST"),
                        luse_to   = c("WWHT", "CORN", "CORN"),
                        fraction  = c(0.1, 0.25, 0.05),
                        year_from = c(2010, 2015, 2010),
                        year_to   = c(2030, 2030, 2020),
                        sub       = c('', '1:8', 'c(1,3,7)'),
                        soil      = c('', '', 'c("soilA", "soilB")'),
                        slope     = c('c("1-3", "3-5")', '"5-9999"', ''),
                        stringsAsFactors = FALSE)
  if(is.null(lup_path)){
    return(lup_tmp)
  } else {
    write.csv(x = lup_tmp, file = paste(lup_path, "lup_tmp.csv", sep = "/"),
              row.names = FALSE)
  }
}
