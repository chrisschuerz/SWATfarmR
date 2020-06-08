#' Write management backup
#'
#' @param txtIO_pth Path to the SWAT project to apply SWATfarmR
#' @param overwrite Logical, TRUE for overwrite of old managament files
#'
#' @return Writes a backup folder for the .mgt Files of a project. This is
#'   required when subsections of simulation periods are needed for e.g.
#'   calibration and validation. The entire simulation period is saved in
#'   backup. The subsetted .mgt files can be written to txtInOut applying
#'   limit_simperiod().
#' @export
#'

write_mgtbackup <- function(txtIO_pth, overwrite = FALSE){
  if(dir.exists(txtIO_pth%//%"mgt_backup") & !overwrite){
    stop("Backup already exists! Set overwrite = TRUE to overwrite old backup")
  }
  mgt_files <- inquire_filenames(file_path = txtIO_pth,
                                 file_pattern = "\\.mgt$")

  if(dir.exists(txtIO_pth%//%"mgt_backup")){
    mgt_old <- inquire_filenames(file_path = txtIO_pth%//%"mgt_backup",
                                 file_pattern = "\\.")
    file.remove(txtIO_pth%//%"mgt_backup"%//%mgt_old)
  }else {dir.create(txtIO_pth%//%"mgt_backup")}

  print("Write management backup files:")
  prgr_bar <- txtProgressBar(min = 0, max = 100, initial = 0, style = 3)
  file.copy(txtIO_pth%//%"file.cio", txtIO_pth%//%"mgt_backup",
            overwrite = TRUE)
  for (i in 1:length(mgt_files)){
    file.copy(txtIO_pth%//%mgt_files[i], txtIO_pth%//%"mgt_backup",
              overwrite = TRUE)
  setTxtProgressBar(prgr_bar, i*100/length(mgt_files))
  }

}
