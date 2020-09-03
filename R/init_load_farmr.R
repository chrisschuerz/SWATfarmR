#' Initiate a new SWATfarmR project
#'
#' @param project_name the name of the farmr project.
#' @param project_path Path to the SWAT TxtInOut folder to which the farmr
#'   should be applied.
#'
#' @return Generates a new farmr_project in the working environment (as an R6
#'   object) and saves the project the TxtInOut folder.
#' @export

new_farmr <- function(project_name, project_path) {
  farmr_obj <- farmr_project$new(project_name = project_name,
                                 project_path = project_path)

  assign(project_name, farmr_obj, envir = sys.frame(-1))
  farmr_obj$save()
}

#' Load an existing SWATfarmR project
#'
#' @param file File path of a swatfarmr project located in a TxtInOut folder.
#'
#' @return Generates a new farmr_project in the working environment (as an R6
#'   object) and saves the project the TxtInOut folder.
#' @export

load_farmr <- function(file) {
  project_path <- dirname(file)
  project_name <- strsplit(basename(file), "\\.")[[1]][1]
  file_ext     <- strsplit(basename(file), "\\.")[[1]][2]

  if(tolower(file_ext) != "farm") stop("farmr object must be a *.farm file!")

  farmr_obj <- readRDS(file)
  farmr_obj$.data$meta$project_path <- project_path
  farmr_obj$.data$meta$project_name <- project_name

  assign(project_name, farmr_obj, envir = sys.frame(-1))
  farmr_obj$save()
}
