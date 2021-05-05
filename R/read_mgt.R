#' Read the raw management data for backup
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#' @param version String that indicates what SWAT version the project is.
#'
#' @keywords internal
#'
#'
read_mgt_init <- function(project_path, version) {
  if(version == 'plus') {
    mgt_dat <- read_mgt_plus(project_path)
  } else if(version == '2012') {
    mgt_dat <- read_mgt_2012(project_path)
  }
  return(mgt_dat)
}

#' Read SWAT mgt input files for SWAT+
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom readr read_lines
#'
#' @keywords internal
#'
read_mgt_plus <- function(project_path) {
  mgt_sch  <- read_lines(project_path%//%'management.sch')
  luse_lum <- read_table(file = project_path%//%"landuse.lum",
                         col_names = TRUE, col_types = cols(), skip = 1)
  hru_data <- read_table(file = project_path%//%"hru-data.hru",
                         col_names = TRUE, col_types = cols(), skip = 1)
  luse_header <- read_lines(project_path%//%'landuse.lum', n_max = 1)
  hru_header  <- read_lines(project_path%//%'hru-data.hru', n_max = 1)
  time_sim    <- read_lines(project_path%//%'time.sim')

  return(list(management_sch = mgt_sch,
              landuse_lum    = luse_lum,
              hru_data       = hru_data,
              luse_header    = luse_header,
              hru_header     = hru_header))
}

#' Read SWAT mgt input files for SWAT2012
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom dplyr %>%
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
