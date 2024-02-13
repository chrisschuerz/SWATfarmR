#' Initiate a new SWATfarmR project
#'
#' @param project_name the name of the farmr project.
#' @param project_path Path to the SWAT TxtInOut folder to which the farmr
#'   should be applied.
#' @param project_type Define the mode how scheduled operations are saved. With
#'   `project_type = 'database'` (default) a `*.mgts` object is generated aside
#'   of the `*.farm` object in the project folder, where scheduled operations
#'   are saved incrementally. This allows to continue with scheduling in case
#'   an error occurs during the scheduling. With `project_type = 'environment'`
#'   the scheduled operations are only saved in the R environment. In case of
#'   an error then all already scheduled operations will be lost.
#'
#' @return Generates a new farmr_project in the working environment (as an R6
#'   object) and saves the project the TxtInOut folder.
#' @export

new_farmr <- function(project_name, project_path, project_type = 'database') {
  farmr_obj <- farmr_project$new(project_name = project_name,
                                 project_path = project_path)

  assign(project_name, farmr_obj, envir = sys.frame(-1))
  farmr_obj$save()
}

#' Load an existing SWATfarmR project
#'
#' @param file File path of a swatfarmr project located in a TxtInOut folder.
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbReadTable
#' @importFrom dplyr mutate %>%
#' @importFrom lubridate ymd
#' @importFrom RSQLite SQLite
#' @importFrom tibble tibble
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

  mgts_path <- paste0(project_path, '/', project_name, '.mgts')
  if(file.exists(mgts_path)) {
    mgt_db <- dbConnect(SQLite(), mgts_path)
    mgt_tbls <- dbListTables(mgt_db)
    farmr_obj$.data$scheduled_operations$scheduled_years <-
      dbReadTable(mgt_db, 'scheduled_years') %>%
      tibble(.)
    if('skipped_operations' %in% mgt_tbls) {
      farmr_obj$.data$scheduled_operations$skipped_operations <-
        dbReadTable(mgt_db, 'skipped_operations') %>%
        tibble(.) %>%
        mutate(date_prev_op = ymd(19700101) + date_prev_op)
    }
    dbDisconnect(mgt_db)
  }

  assign(project_name, farmr_obj, envir = sys.frame(-1))
  farmr_obj$save()
}
