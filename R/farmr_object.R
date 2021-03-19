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

    initialize = function(project_name, project_path) {
      if(file.exists(project_path%//%project_name%.%"farm")){
        stop("FarmR project allready exists in"%&&%project_path)
      }

      t0 <- now()
      self$.data$meta$project_name <- project_name
      self$.data$meta$project_path <- project_path
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
    },

    read_management = function(file, discard_schedule = FALSE) {
      if(!discard_schedule & !is.null(self$.data$scheduled_operations)) {
        stop("Management operations were already scheduled based on an other ",
             "management table! If you want to discard the scheduled operations",
             "and start from scratch, please set 'discard_schedule = TRUE.")
      }

      mgt_lkp <- read_mgt_lkp(file,
                              self$.data$meta$swat_version,
                              self$.data$meta$project_path,
                              self$.data$meta$hru_attributes)
      self$.data$management$schedules <- mgt_lkp$mgt_codes
      self$.data$management$schedules_text <- mgt_lkp$mgt_text
      self$.data$meta$parameter_lookup <- mgt_lkp$lookup


      # self$.data$management$mgt_full <- read_mgt_table(file)
      # check_mgt_table(self$.data$management$mgt_full,
      #                 self$.data$meta$parameter_lookup,
      #                 self$.data$meta$hru_attributes)
      # self$.data$management$mgt_codes <-
      #   translate_mgt_table(self$.data$management$mgt_full,
      #                       self$.data$meta$parameter_lookup)

      # self$check_rules <- check_rules()
      self$schedule_operations <- function() {
        self$.data$scheduled_operations <-
          schedule_operation(mgt_schedule = self$.data$management$mgt_codes,
                             hru_attribute = self$.data$meta$hru_attributes,
                             variables = self$.data$variables,
                             lookup = self$.data$meta$parameter_lookup)

        self$write_mgt_files <- function(start_year = NULL, end_year = NULL, write_all = TRUE) {
          write_operation(path = self$.data$meta$project_path,
                          mgt_raw = self$.data$meta$mgt_raw,
                          schedule = self$.data$scheduled_operations$scheduled_operations,
                          variable = self$.data$variables[[1]],
                          write_all = write_all,
                          start_year = start_year,
                          end_year = end_year)
          self$save()
        }
        self$reset_mgt_files <- function() {
          reset_mgt(path = self$.data$meta$project_path,
                    mgt_raw = self$.data$meta$mgt_raw)
          self$save()
        }
        self$save()
      }
      self$save()
    },

    save = function(){
      obj_save <- get(x = self$.data$meta$project_name,
                      envir = sys.frame(-1))
      saveRDS(object = obj_save,
              file = self$.data$meta$project_path%//%
                     self$.data$meta$project_name%.%
                     "farm")
    }



  #   load_soilgrids = function(soilgrids_server = self$.data$soilgrids$meta$server_path,
  #                             layer_names = c("BDRICM_M_250m",
  #                                             "BLDFIE_M_sl"%&%1:7%_%"250m",
  #                                             "CLYPPT_M_sl"%&%1:7%_%"250m",
  #                                             "CRFVOL_M_sl"%&%1:7%_%"250m",
  #                                             "SLTPPT_M_sl"%&%1:7%_%"250m",
  #                                             "SNDPPT_M_sl"%&%1:7%_%"250m",
  #                                             "CECSOL_M_sl"%&%1:7%_%"250m",
  #                                             "ORCDRC_M_sl"%&%1:7%_%"250m",
  #                                             "PHIHOX_M_sl"%&%1:7%_%"250m")){
  #     cat("Downloading soilgrids layer:\n\n")
  #     layer_meta <- get_layermeta(project_path = self$.data$meta$project_path,
  #                                 wcs = soilgrids_server)
  #
  #     self$.data$soilgrids$meta$pixel_size <- layer_meta$pixel_size
  #     self$.data$soilgrids$meta$extent <- layer_meta$extent
  #
  #     self$.data$soilgrids$meta$layer_names <-
  #       obtain_soilgrids(project_path = self$.data$meta$project_path,
  #                        shp_file = self$.data$shape_file,
  #                        wcs = soilgrids_server,
  #                        layer_meta = layer_meta,
  #                        layer_names = layer_names)
  #
  #     cat("\nLoading soilgrids layer into R:\n\n")
  #     soil_data <- load_soilgrids(project_path = self$.data$meta$project_path,
  #                                 shape_file   = self$.data$shape_file,
  #                                 layer_names  = self$.data$soilgrids$meta$layer_names)
  #
  #
  #     self$.data$soilgrids$raster     <- soil_data$soil_raster
  #     self$.data$soilgrids$data       <- soil_data$soil_list
  #     self$.data$soilgrids$meta$layer <- soil_data$layer_meta
  #     self$.data$data_processed       <- soil_data$soil_list
  #
  #     self$from_scratch <- function(){
  #       self$.data$data_processed <- self$.data$soilgrids$data
  #       self$.data$soil_cluster <- NULL
  #     }
  #
  #     self$aggregate_depth <- function(lower_bound) {
  #       if(!is.null(self$.data$soil_cluster) &
  #          is.null(self$.data$soil_cluster$final_n_class)){
  #         stop("Set final number of soil classes before aggregating!")
  #       }
  #
  #       self$.data$data_processed <-
  #         aggregate_layer(soil_list = self$.data$data_processed,
  #                         lower_bound)
  #
  #       self$.data$soilgrids$meta$layer$depths <- lower_bound
  #     }
  #
  #     self$cluster_soil <- function(n_class){
  #       if(!is.null(self$.data$soil_cluster$final_n_class)){
  #         stop("Clustering allready performed and final number of classes set!\n"%&%
  #              "Start from scratch with $from_scratch() if you want to redo clustering.")
  #       }
  #
  #       self$.data$data_processed$soil_class <- NULL
  #
  #       self$.data$soil_cluster <-
  #         cluster_soil(soil_list = self$.data$data_processed,
  #                      n_class = n_class)
  #
  #       self$evaluate_cluster <- function(){
  #         evaluate_cluster(cluster_result = self$.data$soil_cluster)
  #       }
  #
  #       self$select_n_class <-  function(final_n_class){
  #         if(!("n"%_%final_n_class %in% names(self$.data$soil_cluster))){
  #           stop("Selected number of classes not available!")
  #         }
  #
  #         self$.data$soil_cluster$final_n_class <- final_n_class
  #         self$.data$data_processed <-
  #           set_cluster_data(soil_data = self$.data, n_class = final_n_class)
  #       }
  #
  #       self$plot_cluster <- function(n_class = self$.data$soil_cluster$final_n_class) {
  #         plot_soilmap(soil_data = self$.data, n_class = n_class)
  #       }
  #     }
  #
  #     self$write_output <- function(format){
  #       if(!is.null(self$.data$soil_cluster) &
  #          is.null(self$.data$soil_cluster$final_n_class)){
  #         stop("Set final number of soil classes before writing outputs!")
  #       }
  #
  #       write_output(soil_data = self$.data, format = format)
  #     }
  #
  #     self$save()
  #
  #   }
  #
 )
)
