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
#' @importFrom dplyr filter if_else %>%
#' @importFrom lubridate now year ymd
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
write_operation <- function(path, mgt_raw, schedule, assigned_hrus, write_all,
                            start_year, end_year, year_range, version) {

  if(is.null(start_year)) {
    start_year <- year_range[1]
  }
  if(is.null(end_year)) {
    end_year <- year_range[2]
  }
  if(start_year > end_year) {
    stop("'end_year' must be greater than 'start_year'!")
  }
  if(!(start_year %in% year_range) | !(end_year %in% year_range)) {
    stop("'start_year' and 'end_year' must be in a range between ",
         min(year_range), " and ", max(year_range), "!")
  }

  cat("Writing management files:\n")

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
        mutate(date = if_else(operation %in% c(0, 17), ymd(NA) , date))

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

write_op_plus <- function() {
  t0 <- now()
  cat("  - Writing 'hru-data.hru'\n")
  hru_data <- prepare_hru(mgt_raw, assigned_hrus)
  write_lines(hru_data, pth%//%'hru-data.hru')

  cat("  - Writing 'landuse.lum' and 'schedule.mgt'\n")
  lum_head <- add_edit_timestamp(mgt_raw$luse_header)
  lum_names <- lum_to_string(names(mgt_raw$landuse_lum))
  mgt_head <- c(add_edit_timestamp(mgt_raw$management_sch[1]), mgt_raw$management_sch[2])

  lum_mgt <- map2(schedule, names(schedule), ~prepare_lum_i(.x, .y, mgt_raw$landuse_lum)) %>%
    sort_mgt(.)

  landuse_lum <- map(lum_mgt, ~.x$lum) %>%
    reduce(., c) %>%
    c(lum_head, lum_names, .)

  schedule_mgt <-  map(lum_mgt, ~.x$mgt) %>%
    reduce(., c) %>%
    c(mgt_head, .)

  write_lines(landuse_lum, pth%//%'landuse.lum')
  write_lines(schedule_mgt, pth%//%'schedule.mgt')

  cat("  - Writing 'time.sim'\n")
  time_sim <- mgt_raw$time_sim
  time_sim[3] <- map2_chr(c(0, start_year, 0, end_year, 0),
                          c('%9s','%10s', '%9s',  '%9s',  '%9s'),
                          ~ sprintf(.y, .x)) %>%
    paste(., collapse = ' ')

  write_lines(time_sim, pth%//%'time.sim')

  interval(t0,now()) %>%
    round(.) %>%
    as.period(.) %>%
    as.character(.) %>%
    cat("Finished writing management schedules in", ., "\n")
  # finish_progress(n_hru, t0, "Finished writing", "'.mgt' file")
}


prepare_hru <- function(mgt_raw, assigned_hrus) {
  hru_head <- add_edit_timestamp(mgt_raw$hru_header)
  hru_names <- hru_to_string(names(mgt_raw$hru_data))

  hru_data <- mgt_raw$hru_data %>%
    left_join(., select(assigned_hrus, hru_name, schedule), by = c('name' = 'hru_name')) %>%
    mutate(lu_mgt = schedule) %>%
    select(- schedule) %>%
    apply(., 1, hru_to_string) %>%
    c(hru_head, hru_names, .)
  return(hru_data)
}

sort_mgt <- function(mgt) {
  mgt_names <- names(mgt)
  base_nm <- str_remove(mgt_names, '\\_[:digit:]+\\_[:digit:]+')
  base_rnk <- rank(base_nm, ties.method = 'min')
  unit_rnk <- str_extract_all(mgt_names, '\\_[:digit:]+') %>%
    map(., ~ str_remove(.x, '\\_')) %>%
    map(., ~ as.numeric(.x)) %>%
    map_dbl(., ~ 1000*.x[1] + .x[2]) %>%
    rank(., ties.method = 'min')
  mgt_order <- order(1000*base_rnk + unit_rnk)
  mgt[mgt_order]
}

prepare_lum_i <- function(schedule_i, name_i, lum_raw) {
  lum_init <- str_remove(name_i, '\\_[:digit:]+\\_[:digit:]+')
  lum_num <- str_extract(name_i, '\\_[:digit:]+\\_[:digit:]+')
  mgt_name <- paste0(str_remove(lum_init, '\\_lum'), '_mgt', ifelse(is.na(lum_num), '', lum_num))
  lum_i <- lum_raw %>%
    filter(name == lum_init) %>%
    mutate(name = name_i,
           mgt = mgt_name,
           plnt_com = ifelse(is.null(schedule_i$init_crop), plnt_com, schedule_i$init_crop)) %>%
    lum_to_string(.)
  if(is.null(schedule_i$schedule)) {
    schdl <- NULL
  } else {
    schdl <- apply(schedule_i$schedule, 1, schdl_to_string)
  }

  mgt_head <- paste(sprintf('%-24s', mgt_name), sprintf('%10d',length(schdl)),  sprintf('%10d',0))
  mgt_i <- c(mgt_head, schdl)
  return(list(lum = lum_i, mgt = mgt_i))
}

add_edit_timestamp <- function(str) {
  str %>%
    str_remove(., ' and edited.*') %>%
    paste(. , 'and edited with SWATfarmR on', Sys.time())
}

hru_to_string <- function(hru_line) {
  paste(sprintf('%8s', hru_line[1]),
        '',
        sprintf('%-16s', hru_line[2]),
        sprintf('%17s', hru_line[3]),
        sprintf('%17s', hru_line[4]),
        sprintf('%17s', hru_line[5]),
        sprintf('%17s', hru_line[6]),
        sprintf('%17s', hru_line[7]),
        sprintf('%17s', hru_line[8]),
        sprintf('%17s', hru_line[9]),
        sprintf('%17s', hru_line[10])
  )
}

lum_to_string <- function(lum_line) {
  map2_chr(lum_line,c('%-20s', rep('%17s',13)),   ~ sprintf(.y, .x)) %>%
    paste(., collapse = ' ')
}

schdl_to_string <- function(op_line) {
  paste(sprintf('%64s', op_line[2]),
        sprintf('%9d', ifelse(op_line[2] == 'skip', 0, month(op_line[1]))),
        sprintf('%9d', ifelse(op_line[2] == 'skip', 0, day(op_line[1]))),
        sprintf('%13.3f', 0),
        sprintf('%17s', op_line[3]),
        sprintf('%17s', op_line[4]),
        # op_line[5]) %>%
        sprintf('%13s', ifelse(is.na(op_line[5]), 0, op_line[5]))) %>%
    str_replace_all(., '  NA', 'null')
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
