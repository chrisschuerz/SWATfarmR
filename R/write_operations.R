#' Write the mgt files in the TxtInOut folder
#'
#' @param path Path to the TxtInOut folder
#' @param mgt_raw List of original mgt files
#' @param schedule List of tibbles with the shceduled operations.
#' @param write_all Logical. If TRUE mgt files are written for all HRUs. If FALSE
#'   only mgt files are written where operations were scheduled, or an initial
#'   crop was defined.
#' @param start_year Numeric. Defines the start year for which to write operations.
#' @param end_year Numeric. Defines the last year for which to write operations.
#'
#' @importFrom dplyr filter  %>%
#' @importFrom lubridate now year
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
write_operation <- function(path, mgt_raw, schedule, variable, write_all, start_year, end_year) {
  year_range <- sort(unique(variable$year))
  if(is.null(start_year)) {
    start_year <- min(year_range)
  }
  if(is.null(end_year)) {
    end_year <- max(year_range)
  }
  if(start_year >= end_year) {
    stop("'end_year' must be larger than 'start_year'!")
  }
  if(!(start_year %in% year_range) | !(end_year %in% year_range)) {
    stop("'start_year' and 'end_year' must be in a range between ",
         min(year_range), " and ", max(year_range), "!")
  }
  cat("Writing management files:\n")
  t0 <- now()
  n_hru <- length(schedule)
  hru_files <- names(schedule)
  for (i_hru in 1:n_hru) {
    hru_file_i <- hru_files[i_hru]
    mgt_i <- mgt_raw[[hru_file_i]]
    schedule_i <- schedule[[hru_file_i]]

    if(!is.null(schedule_i$init_crop)) {
      mgt_i <- initialize_crop(mgt_i, schedule_i$init_crop)
    } else if(write_all) {
      mgt_i[4] <- paste(sprintf("%16i", 0),
                        "   | IGRO: Land cover status: 0-none growing; 1-growing")
    }
    if(!is.null(schedule_i$schedule)) {
      schedule_tbl <- schedule_i$schedule %>%
        filter(year(date) >= start_year) %>%
        filter(year(date) <= end_year) %>%
        mutate(date = ifelse(operation %in% c(0, 17), NA, date))

      mgt_i <- add_management_schedule(mgt_i, schedule_tbl)
    } else if(write_all) {
      mgt_i <- mgt_i[1:30]
      mgt_i[29] <- paste(sprintf("%16i", 1),
                            "   | NROT: number of years of rotation")
      mgt_i <- c(mgt_i, "                17")
    }
    write_lines(mgt_i, path%//%hru_file_i%.%"mgt")
    display_progress_pct(i_hru, n_hru, t0)
  }
  write_file_cio(path, start_year, end_year)
  finish_progress(n_hru, t0, "Finished writing", "'.mgt' file")
}

#' Write values for any initial crop
#'
#' @param mgt_file Lines of the respective mgt file
#' @param schedule_init tibble with crop_init parameters for the respective HRU
#'
#' @keywords internal
#'
initialize_crop <- function(mgt_file, schedule_init) {
  mgt_file[4] <- paste(sprintf("%16i", 1),
                       "   | IGRO: Land cover status: 0-none growing; 1-growing")
  mgt_file[5] <- paste(sprintf("%16i", schedule_init$plant_id),
                       "   | PLANT_ID: Land cover ID number (IGRO = 1)")
  mgt_file[6] <- paste(sprintf("%16.2f", schedule_init$lai_init),
                       "   | LAI_INIT: Initial leaf are index (IGRO = 1)")
  mgt_file[7] <- paste(sprintf("%16.2f", schedule_init$bio_init),
                       "   | BIO_INIT: Initial biomass (kg/ha) (IGRO = 1)")
  mgt_file[8] <- paste(sprintf("%16.2f", schedule_init$phu_plant),
                       "   | PHU_PLT: Number of heat units to bring plant to maturity (IGRO = 1)")
  return(mgt_file)
}

#' Add the scheduled management operations in thve mgt file
#'
#' @param mgt_file Lines of the respective mgt file
#' @param schedule_tbl Tibble with the scheduled operations for the respective HRU
#'
#' @importFrom dplyr select  %>%
#' @importFrom lubridate day month year
#' @importFrom tibble add_column
#' @importFrom purrr map2_df
#' @importFrom stringr str_replace_all
#'
#' @keywords internal
#'
add_management_schedule <- function(mgt_file, schedule_tbl) {
  nrot <- length(unique(year(schedule_tbl$date)))
  mgt_file[29] <- paste(sprintf("%16i", nrot),
                        "   | NROT: number of years of rotation")

  schedule_tbl <- add_column(schedule_tbl,
                             month = month(schedule_tbl$date),
                             day   = day(schedule_tbl$date),
                             hu    = "        ",
                             .before = 1) %>%
    select(-date)

  str_format <- c(month = "%3i", day = "%2i", hu = "%8s", operation = "%2i",
                  mgt1 = "%4i", mgt2 = "%3i", mgt3 = "%2i", mgt4 = "%12.5f",
                  mgt5 = "%6.2f", mgt6 = "%11.5f", mgt7 = "%4.2f", mgt8 = "%6.2f",
                  mgt9 = "%5.2f")

  schedule_txt <- map2_df(str_format, schedule_tbl, ~sprintf(.x,.y)) %>%
    apply(., 1, paste, collapse = " ") %>%
    str_replace_all(., "NA", "  ")

  mgt_file <- c(mgt_file[1:30], schedule_txt)

  return(mgt_file)

}

#' Write the start and end dates into the file.cio
#'
#' @param path Path to the TxtInOut folder
#' @param start_year Numeric. Defines the start year for which to write operations.
#' @param end_year Numeric. Defines the last year for which to write operations.
#'
#' @importFrom lubridate leap_year
#' @importFrom readr read_lines write_lines
#'
#' @keywords internal
#'
write_file_cio <- function(path, start_year, end_year) {
  file_cio <- read_lines(path%//%"file.cio")
  file_cio[8]  <- paste(sprintf("%16i", (end_year - start_year + 1)),
                        "   | NBYR : Number of years simulated")
  file_cio[9]  <- paste(sprintf("%16i", start_year),
                        "   | IYR : Beginning year of simulation")
  file_cio[10] <- paste(sprintf("%16i", 1),
                        "   | IDAF : Beginning julian day of simulation")
  file_cio[11] <- paste(sprintf("%16i", ifelse(leap_year(end_year), 366, 365)),
                        "   | IDAL : Ending julian day of simulation")
  write_lines(file_cio, path%//%"file.cio")
}

#' Reset all management files back to the original files
#'
#' @param path Tibble Path to the TxtInOut folder
#' @param mgt_raw List of original mgt files
#'
#' @importFrom lubridate now
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
reset_mgt <- function(path, mgt_raw) {
  cat("Resetting all management files:\n")
  n_hru <- length(mgt_raw)
  t0 <- now()
  for (i_hru in 1:n_hru) {
    file_i <- names(mgt_raw)[i_hru]
    write_lines(mgt_raw[[file_i]], path%//%file_i%.%"mgt")
    SWATfarmR:::display_progress_pct(i_hru, n_hru, t0)
  }
  SWATfarmR:::finish_progress(n_hru, t0, "Finished resetting", "'.mgt' file")
}
