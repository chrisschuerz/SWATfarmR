#' farmr_project
#'
#' @rdname farmr_project
#'
#' @import R6
#' @importFrom dplyr as_tibble select
#' @importFrom lubridate now
#' @importFrom purrr set_names
#' @importFrom stringr str_replace
#' @importFrom tidyselect starts_with
#'
#' @export
farmr_project <- R6::R6Class(
  "farmr_project",
  cloneable = FALSE,
  lock_objects = FALSE,
  public = list(
    .data = list(),

    initialize = function(project_name, project_path, project_type) {
      if(file.exists(project_path%//%project_name%.%"farm")){
        stop("FarmR project allready exists in"%&&%project_path)
      }

      t0 <- now()
      self$.data$meta$project_name <- project_name
      self$.data$meta$project_path <- project_path
      self$.data$meta$project_type <- project_type
      self$.data$meta$swat_version <- check_version(project_path)

      self$.data$meta$hru_attributes <- read_hru_attributes(project_path,
                                                            self$.data$meta$swat_version,
                                                            t0)
      self$.data$variables <- read_weather(project_path,
                                           self$.data$meta$swat_version)

      self$.data$meta$hru_var_connect <- connect_unit_weather(project_path,
                                                              self$.data$meta$hru_attributes,
                                                              self$.data$variables,
                                                              self$.data$meta$swat_version)

      self$.data$meta$mgt_raw <- read_mgt_init(project_path, self$.data$meta$swat_version)

      finish_progress(NULL, t0, "", "Finished")
    },

    add_variable = function(data, name, assign_unit = NULL, overwrite = FALSE) {
      var_add <- add_variable(data, name, assign_unit, overwrite,
                              con = self$.data$meta$hru_var_connect,
                              variables = self$.data$variables)

      self$.data$variables <- var_add$variables
      self$.data$meta$hru_var_connect <- var_add$con

      self$save()
    },

    read_management = function(file, discard_schedule = 'no') {
      if(discard_schedule == 'no' & !is.null(self$.data$scheduled_operations)) {
        stop("Management operations were already scheduled based on existing management table!\n\n",
             "If you only want to discard scheduled operations for HRUs where the new management\n",
             "is updated set  discard_schedule = 'new'.\n\n",
             "If you want to discard all already scheduled operations set discard_schedule = 'all'.\n",
             "!CAUTION! This will delete all scheduled operations and you have to start from scratch!")
      }

      mgt_lkp <- read_mgt_lkp(file,
                              self$.data$meta$swat_version,
                              self$.data$meta$project_path,
                              self$.data$meta$hru_attributes)

      if(discard_schedule == 'new' & !is.null(self$.data$scheduled_operations)) {
        self$.data$scheduled_operations <-
          compare_reset_mgt(mgt_lkp,
                            self$.data$management$schedule,
                            self$.data$scheduled_operations,
                            self$.data$meta$project_path,
                            self$.data$meta$project_name,
                            self$.data$meta$project_type
                           )
      }

      self$.data$management$schedule <- mgt_lkp$mgt_code
      self$.data$management$schedule_text <- mgt_lkp$mgt_text
      self$.data$meta$parameter_lookup <- mgt_lkp$lookup


      # self$.data$management$mgt_full <- read_mgt_table(file)
      # check_mgt_table(self$.data$management$mgt_full,
      #                 self$.data$meta$parameter_lookup,
      #                 self$.data$meta$hru_attributes)
      # self$.data$management$mgt_codes <-
      #   translate_mgt_table(self$.data$management$mgt_full,
      #                       self$.data$meta$parameter_lookup)

      # self$check_rules <- check_rules()
      self$schedule_operations <- function(start_year = NULL,
                                           end_year = NULL,
                                           n_schedule = NULL,
                                           replace = 'missing') {

        self$.data$scheduled_operations <-
          schedule_operation(data = self$.data,
                             start_year = start_year,
                             end_year = end_year,
                             n_schedule = n_schedule,
                             replace = replace)

        self$write_operations <- function(start_year = NULL, end_year = NULL) {
          write_operation(data = self$.data,
                          start_year = start_year,
                          end_year = end_year)
          self$save()
        }
        self$reset_files <- function() {
          reset_mgt(path = self$.data$meta$project_path,
                    mgt_raw = self$.data$meta$mgt_raw,
                    version = self$.data$meta$swat_version)
          self$save()
        }
        self$save()
      }
      self$save()
    },

    save = function(){
      saveRDS(object = self,
              file = self$.data$meta$project_path%//%
                     self$.data$meta$project_name%.%
                     "farm")
    }
 )
)
