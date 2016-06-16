#' Limit the simulation period and write management files for this period
#'
#' @param txtIO_pth
#' @param start_year
#' @param end_year
#' @param nyskip
#'
#' @return
#' @export
#'
#' @examples
limit_simperiod <- function(txtIO_pth, start_year, end_year, nyskip){

  # Libraries ---------------------------------------------------------------
  require(lubridate)

  # General check of inputs -----------------------------------------------
  if(!dir.exists(txtIO_pth%/%"mgt_backup")){
    stop("No management backup done yet. Please perform write_mgtbackup() before!")
  }
  if(start_year > end_year){
    stop("end_year must be greater than start_year")
  }

  # Get information of file.cio ---------------------------------------------
  file_cio <- readLines(con = txtIO_pth%/%"mgt_backup"%/%"file.cio")
  n_yr <- end_year - start_year + 1

  ny_old <- as.numeric(scan(text = file_cio[8], what = "", quiet = TRUE)[1])
  start_old <- as.numeric(scan(text = file_cio[9], what = "", quiet = TRUE)[1])
  end_old <- start_old + ny_old - 1


  # Check inputs against data from file.cio ---------------------------------
  if(start_year < start_old){
    stop("start_year must be greater than Beginning year in file.cio")
  }
  if(n_yr > ny_old){
    stop("end_year greater than available years in file.cio")
  }

  # Rewrite file.cio --------------------------------------------------------
  file_cio[8] <- paste(sprintf("%16i", n_yr),
                       "   | NBYR : Number of years simulated")
  file_cio[9] <- paste(sprintf("%16i", start_year),
                       "   | IYR : Beginning year of simulation")
  if(leap_year(start_year)){
    file_cio[11] <- "             366    | IDAL : Ending julian day of simulation"
  } else {
    file_cio[11] <- "             365    | IDAL : Ending julian day of simulation"
  }
  file_cio[60] <- paste(sprintf("%16i", nyskip),
                       "   | NYSKIP: number of years to skip output printing/summarization")

  file.remove(txtIO_pth%/%"file.cio")
  writeLines(file_cio, con = txtIO_pth%/%"file.cio")


  # Modyfy and rewrite mgt files --------------------------------------------
  print("Write management operations for shorter time period to txtIO:")
  prgr_bar <- txtProgressBar(min = 0, max = 100, initial = 0, style = 3)
  count <- 0

  del_init <- start_year - start_old
  del_end  <- end_old - end_year

  mgt_files <- inquire_filenames(file_path = txtIO_pth%/%"mgt_backup",
                                 file_pattern = "\\.mgt$")

  for (i in mgt_files){
    mgt_i <- readLines(con = txtIO_pth%/%"mgt_backup"%/%i)

    pos_endyr <- which(trim(mgt_i) == "0")
    pos_skip  <- which(trim(mgt_i) == "17")
    if(length(pos_endyr) > 0){
      mgt_i[29] <- paste(sprintf("%16i", n_yr),
                           "   | NROT: number of years of rotation")

      pos_strtop <- c(31,(pos_endyr[-length(pos_endyr)] + 1))
    } else {
      pos_strtop <- pos_skip
    }

    op_pos <- pos_strtop[c((del_init + 1),
                           length(pos_strtop) - del_end + 1)]
    op_pos[2] <- op_pos[2] - 1

    mgt_i <- c(mgt_i[1:30], mgt_i[op_pos[1]:op_pos[2]])
    file.remove(txtIO_pth%/%i)
    writeLines(mgt_i, con = txtIO_pth%/%i)
    setTxtProgressBar(prgr_bar, count*100/length(mgt_files))
    count <- count + 1
  }
}

## Subfunctions -----------------------------------------------------------
trim <- function(chr) gsub("^\\s+|\\s+$", "", chr)






