#' Read the raw management data for backup
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#' @param version String that indicates what SWAT version the project is.
#'
#' @keywords internal
#'
#'
read_mgt <- function(project_path, version) {
  if(version == 'plus') {
    mgt_dat <- read_lines(project_path%//%'management.sch')
  } else if(version == '2012') {
    mgt_dat <- read_mgt_2012(project_path)
  }
  return(mgt_dat)
}

#' Read SWAT mgt input files for SWAT2012
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom purrr map set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_remove
#'
#' @keywords internal
#'
read_mgt_2012 <- function(project_path) {
  mgt_list <- list.files(path = project_path, pattern = "[:0-9:].mgt")
  mgt_files <- map(project_path%//%mgt_list, read_lines) %>%
    set_names(str_remove(mgt_list, ".mgt"))
  return(mgt_files)
}