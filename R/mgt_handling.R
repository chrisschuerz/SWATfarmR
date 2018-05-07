#' Add a farm management schedule to a farmr object
#'
#' @param mgt_name Name of the management to address in the farmr object.
#' @param mgt_schedule Path to a mgt schedule file in .csv format, or data.frame
#'   (tibble) object in the working environment.
#' @param cnop_table Optional table with Curve number values for the operations
#'   plant, harvest, and tillage for the respective crops in the mgt_schedule
#'   and the present hydrologic soil groups, in .csv or data.frame format.

#' @importFrom pasta %//%
#'
#' @return Generates a new farmr_project in the working environment (as an R6
#'   object) and saves the project the TxtInOut folder.
#' @export

add_management <- function(mgt_name, mgt_schedule, cnop_table = NULL) {


  farmr_obj <- farmr_project$new(project_name = project_name,
                                 project_path = project_path)

  assign(project_name, farmr_obj, envir = sys.frame(-1))
  farmr_obj$save()
}
