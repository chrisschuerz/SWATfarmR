#' Write the mgt schedules into the respective files in the TxtInOut folder
#'
#' @param path Path to the TxtInOut folder
#' @param mgt_raw List of original files that are relevant for the mgt scheduling.
#' @param schedule List of tibbles with the scheduled operations.
#' @param assigned_hrus Tibble that links the mgt schedules to the HRUs
#' @param start_year Numeric. Defines the start year for which to write operations.
#' @param end_year Numeric. Defines the last year for which to write operations.
#' @param year_range numeric vector with start and end year of the scheduled operations.
#' @param version String that indicates the SWAT version.
#'
#'
#' @keywords internal
#'
write_operation <- function(path, mgt_raw, schedule, assigned_hrus,
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
  if(start_year < year_range[1] | end_year > year_range[2]) {
    stop("'start_year' and 'end_year' must be in a range between ",
         year_range[1], " and ", year_range[2], "!")
  }

  cat("Writing management files:\n")
  if (version == 'plus') {
    write_op_plus(path, mgt_raw, schedule, assigned_hrus, start_year, end_year)
  } else {
    write_op_2012(path, mgt_raw, schedule, assigned_hrus, start_year, end_year)
  }
}

#' Write the management schedules for a SWAT+ project
#'
#' @param path Path to the TxtInOut folder
#' @param mgt_raw List of original files that are relevant for the mgt scheduling.
#' @param schedule List of tibbles with the scheduled operations.
#' @param assigned_hrus Tibble that links the mgt schedules to the HRUs
#' @param start_year Numeric. Defines the start year for which to write operations.
#' @param end_year Numeric. Defines the last year for which to write operations.
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate now interval
#' @importFrom purrr map map2 map2_chr reduce
#' @importFrom readr write_lines
#' @importFrom stringr str_trim str_split
#'
#' @keywords internal
#'
write_op_plus <- function(path, mgt_raw, schedule, assigned_hrus, start_year, end_year) {
  t0 <- now()
  cat("  - Preparing 'hru-data.hru'\n")
  hru_data <- prepare_hru(mgt_raw, assigned_hrus)
  write_lines(hru_data, path%//%'hru-data.hru')

  cat("  - Preparing 'landuse.lum'\n")
  lum_head <- add_edit_timestamp(mgt_raw$luse_header)
  lum_names <- lum_to_string(names(mgt_raw$landuse_lum))

  lum_mgt <- map2(schedule, names(schedule), ~prepare_lum_i(.x, .y, mgt_raw$landuse_lum,
                                                            start_year, end_year)) %>%
    sort_mgt(.)

  landuse_lum <- map(lum_mgt, ~.x$lum) %>%
    reduce(., c) %>%
    c(lum_head, lum_names, .)

  cat("  - Preparing 'schedule.mgt'\n")

  mgt_col <- 'name                       numb_ops  numb_auto            op_typ       mon       day        hu_sch          op_data1          op_data2      op_data3'
  mgt_head <- c(add_edit_timestamp(mgt_raw$management_sch[1]), mgt_col)
  schedule_mgt <-  map(lum_mgt, ~.x$mgt) %>%
    reduce(., c) %>%
    c(mgt_head, .)

  cat("  - Preparing 'plant.ini'\n")
  ini_head <- c(add_edit_timestamp(mgt_raw$plant_ini[1]), mgt_raw$plant_ini[2])
  plnt_ini <- map(schedule, ~ prepare_plant_ini_i(.x, start_year, end_year)) %>%
    map2(., names(schedule), ~ build_ini_line(.x, .y)) %>%
    unlist(.) %>%
    unname(.) %>%
    c(ini_head, .)

  cat("  - Writing files \n")
  write_lines(landuse_lum, path%//%'landuse.lum')
  write_lines(schedule_mgt, path%//%'management.sch')
  write_lines(plnt_ini, path%//%'plant.ini')

  cat("  - Updating 'time.sim'\n")
  time_sim <- mgt_raw$time_sim
  time_sim[1] <- add_edit_timestamp(time_sim[1])
  time_sim[3] <- map2_chr(c(0, start_year, 0, end_year, 0),
                          c('%9s','%10s', '%9s',  '%9s',  '%9s'),
                          ~ sprintf(.y, .x)) %>%
    paste(., collapse = ' ')
  write_lines(time_sim, path%//%'time.sim')

  cat("  - Updating 'file.cio'\n")
  file_cio <- mgt_raw$file_cio
  file_cio[1] <- add_edit_timestamp(file_cio[1])
  lum_line <- file_cio[21] %>%
    str_trim(.) %>%
    str_split(., '[:space:]+', simplify = TRUE)
  lum_line[3] <- 'management.sch'
  file_cio[21] <- paste(sprintf(rep('%-17s', length(lum_line)), lum_line),
                        collapse = ' ')
  write_lines(file_cio, path%//%'file.cio')

  interval(t0,now()) %>%
    round(.) %>%
    as.period(.) %>%
    as.character(.) %>%
    cat("Finished writing management files in", ., "\n")
}

#' Write the management schedules for a SWAT2012 project
#'
#' @param path Path to the TxtInOut folder
#' @param mgt_raw List of original files that are relevant for the mgt scheduling.
#' @param schedule List of tibbles with the scheduled operations.
#' @param assigned_hrus Tibble that links the mgt schedules to the HRUs
#' @param start_year Numeric. Defines the start year for which to write operations.
#' @param end_year Numeric. Defines the last year for which to write operations.
#'
#' @importFrom dplyr filter if_else mutate %>%
#' @importFrom lubridate now year ymd
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
write_op_2012 <- function(path, mgt_raw, schedule, assigned_hrus, start_year, end_year) {
  t0 <- now()
  n_hru <- nrow(assigned_hrus)
  for (i_hru in 1:n_hru) {
    hru_file_i <- assigned_hrus$file[i_hru]
    schdl_label_i <- assigned_hrus$schedule[i_hru]
    mgt_i <- mgt_raw[[hru_file_i]]
    schedule_i <- schedule[[schdl_label_i]]

    if(!is.null(schedule_i$init_crop)) {
      mgt_i <- initialize_crop(mgt_i, schedule_i$init_crop)
    } else {
      mgt_i[4] <- paste(sprintf("%16i", 0),
                        "   | IGRO: Land cover status: 0-none growing; 1-growing")
    }
    if(!is.null(schedule_i$schedule)) {
      schedule_tbl <- schedule_i$schedule %>%
        filter(year(date) >= start_year) %>%
        filter(year(date) <= end_year) %>%
        mutate(date = if_else(operation %in% c(0, 17), ymd(NA) , date))

      mgt_i <- add_management_schedule(mgt_i, schedule_tbl)
    } else {
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

#' Prepare the hru-data for writing in SWAT+ and convert it to text lines
#'
#' @param mgt_raw List of original files that are relevant for the mgt scheduling.
#' @param assigned_hrus Tibble that links the mgt schedules to the HRUs.
#'
#' @importFrom dplyr left_join mutate select %>%
#' @importFrom lubridate now year ymd
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
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

#' Sort the management schedules by name, unit (sub or rtu), and number of realization.
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
#' @importFrom dplyr %>%
#' @importFrom purrr map map_dbl
#' @importFrom stringr str_extract_all str_remove
#'
#' @keywords internal
#'
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

#' Prepare the landuse.lum for the the generated operation schedule i
#'
#' @param schedule_i Tibble with the ith scheduled operations.
#' @param name_i Name of the ith operation schedule
#' @param lum_raw Initial landuse.lum
#' @param start_year Numeric. Defines the start year for which to write operations.
#' @param end_year Numeric. Defines the last year for which to write operations.
#'
#' @importFrom dplyr filter mutate %>%
#' @importFrom lubridate year
#' @importFrom purrr map map_dbl
#' @importFrom stringr str_extract str_remove
#'
#' @keywords internal
#'
prepare_lum_i <- function(schedule_i, name_i, lum_raw, start_year, end_year) {
  lum_init <- str_remove(name_i, '\\_[:digit:]+\\_[:digit:]+')
  lum_num <- str_extract(name_i, '\\_[:digit:]+\\_[:digit:]+')
  mgt_name <- paste0(str_remove(lum_init, '\\_lum'), '_mgt' , ifelse(is.na(lum_num), '', lum_num))
  if (is.null(schedule_i$init_crop) & is.null(schedule_i$schedule)) {
    com_name <- 'null'
  } else {
    com_name <- paste0(str_remove(lum_init, '\\_lum'), '_comm', ifelse(is.na(lum_num), '', lum_num))
  }
  lum_i <- lum_raw %>%
    filter(name == lum_init) %>%
    mutate(name = name_i,
           mgt = mgt_name,
           plnt_com = com_name) %>%
           # plnt_com = ifelse(is.null(schedule_i$init_crop), plnt_com, schedule_i$init_crop)) %>%
    lum_to_string(.)

  if(is.null(schedule_i$schedule)) {
    schdl <- NULL
  } else {
    schdl <- schedule_i$schedule %>%
      filter(year(date) >= start_year, year(date) <= end_year) %>%
      apply(., 1, schdl_to_string)
  }

  mgt_head <- paste(sprintf('%-24s', mgt_name), sprintf('%10d',length(schdl)),  sprintf('%10d',0))
  mgt_i <- c(mgt_head, schdl)
  return(list(lum = lum_i, mgt = mgt_i))
}

#' Prepare the plant.ini for the the generated operation schedule i
#'
#' @param schedule_i Tibble with the ith scheduled operations.
#' @param start_year Numeric. Defines the start year for which to write operations.
#' @param end_year Numeric. Defines the last year for which to write operations.
#'
#' @importFrom dplyr bind_rows filter mutate %>%
#' @importFrom lubridate year
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
prepare_plant_ini_i <- function(schedule_i, start_year, end_year) {
  if(!is.null(schedule_i$schedule)) {
    plnt_hrv <- schedule_i$schedule %>%
      filter(., year(date) >= start_year & year(date) <= end_year) %>%
      filter(., operation %in% c('plnt', 'harv', 'hvkl', 'kill'))

    if(nrow(plnt_hrv) == 0) {
      plnt_comm <- schedule_i$init_crop %>%
        mutate(., lc_status = 'y', .after = plt_name)
    } else if (all(!unique(plnt_hrv$operation) %in% c('plnt', 'hvkl', 'kill'))) {
      if(!is.null(schedule_i$init_crop)) {
        plnt_comm <- map_df(unique(plnt_hrv$op_data1),
                            ~ mutate(schedule_i$init_crop, plt_name = .x)) %>%
          mutate(., lc_status = 'y', .after = plt_name)
      } else {
        plnt_comm  <- tibble(plt_name  = unique(plnt_hrv$op_data1),
                             lc_status = 'y',
                             lai_init  = 1,
                             bm_init   = 1000,
                             phu_init  = 0,
                             plnt_pop  = 0,
                             yrs_init  = 1,
                             rsd_init  = 1000)
      }
    } else {
      first_plnt_pos <- which(plnt_hrv$operation == 'plnt')[1]

      plnts <- unique(plnt_hrv$op_data1)

      plnt_name_lc_stat_y <- plnt_hrv[0:(first_plnt_pos - 1),] %>%
        filter(operation %in% c('hvkl', 'kill')) %>%
        .$op_data1 %>%
        unique(.)

      if(!is.null(schedule_i$init_crop) & length(plnt_name_lc_stat_y) > 0) {
        plnt_lc_stat_y <- map_df(plnt_name_lc_stat_y,
                                 ~ mutate(schedule_i$init_crop, plt_name = .x)) %>%
          mutate(., lc_status = 'y', .after = plt_name)
      } else if(is.null(schedule_i$init_crop) & length(plnt_name_lc_stat_y) > 0) {
        plnt_lc_stat_y <- tibble(plt_name  = plnt_name_lc_stat_y,
                                 lc_status = 'y',
                                 lai_init  = 1,
                                 bm_init   = 1000,
                                 phu_init  = 0,
                                 plnt_pop  = 0,
                                 yrs_init  = 1,
                                 rsd_init  = 1000)
      } else {
        plnt_lc_stat_y <- NULL
      }

      plnt_name_lc_stat_n <-  plnts[!plnts %in% plnt_name_lc_stat_y]

      if(length(plnt_name_lc_stat_n) > 0) {
        plnt_lc_stat_n <- tibble(plt_name  = plnt_name_lc_stat_n,
                                 lc_status = 'n',
                                 lai_init  = 0,
                                 bm_init   = 0,
                                 phu_init  = 0,
                                 plnt_pop  = 0,
                                 yrs_init  = 0,
                                 rsd_init  = 0)
      } else {
        plnt_lc_stat_n <- NULL
      }
      plnt_comm <- bind_rows(plnt_lc_stat_y, plnt_lc_stat_n)
    }
  } else if (!is.null( schedule_i$init_crop)){
    plnt_comm <- schedule_i$init_crop %>%
      mutate(., lc_status = 'y', .after = plt_name)
  } else {
    plnt_comm <- NULL
  }

  return(plnt_comm)
}

#' Build the lines for the plant.ini text file from the prepared plant inis
#'
#' @param ini_i Tibble with the ith plant_ini.
#' @param name_i Name of the corresponding operation schedule.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map2_df
#' @importFrom stringr str_remove str_extract
#'
#' @keywords internal
#'
build_ini_line  <- function(ini_i, name_i) {
  if(!is.null(ini_i)) {
    lum_init <- str_remove(name_i, '\\_[:digit:]+\\_[:digit:]+')
    lum_num <- str_extract(name_i, '\\_[:digit:]+\\_[:digit:]+')
    com_name <- paste0(str_remove(lum_init, '\\_lum'), '_comm',
                       ifelse(is.na(lum_num), '', lum_num))

    ini_lines <- ini_i %>%
      map2_df(., c('%56s', rep('%13s', 7)), ~ sprintf(.y, .x)) %>%
      apply(., 1, paste, collapse = ' ')

    ini_head <- paste(sprintf('%-18s', com_name),
                      sprintf('%7s', length(ini_lines)),
                      sprintf('%11s', 1))

    ini_lines <- c(ini_head, ini_lines)
  } else {
    ini_lines <- NULL
  }

  return(ini_lines)
}

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
#' @importFrom dplyr %>%
#' @importFrom stringr str_remove
#'
#' @keywords internal
#'
add_edit_timestamp <- function(str) {
  farmr_version <- '2.0.5'
  if (is.null(str)) {
    paste('Written by SWATfarmR', farmr_version, 'on', Sys.time())
  } else {
    str %>%
      str_remove(., ' and edited.*') %>%
      paste(. , 'and edited with SWATfarmR', farmr_version, 'on', Sys.time())
  }
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
#' @importFrom dplyr %>%
#' @importFrom purrr map2_chr
#'
#' @keywords internal
#'
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
reset_mgt <- function(path, mgt_raw, version) {
  t0 <- now()
  cat("Resetting all management files:\n")
  if(version == 'plus') {
    cat("  - Resetting 'hru-data.hru'\n")
    hru_data <- reset_hru_plus(mgt_raw)
    write_lines(hru_data, path%//%'hru-data.hru')

    cat("  - Resetting 'landuse.lum'\n")
    landuse_lum <- reset_lum_plus(mgt_raw)
    write_lines(landuse_lum, path%//%'landuse.lum')

    cat("  - Resetting 'schedule.mgt'\n")
    if (!is.null(mgt_raw$management_sch)) {
      write_lines(mgt_raw$management_sch, path%//%'management.sch')
    } else {
      suppressWarnings(suppressMessages(file.remove(path%//%'management.sch')))
    }

    cat("  - Resetting 'plant.ini'\n")
    landuse_lum <- reset_lum_plus(mgt_raw)
    write_lines(mgt_raw$plant_ini, path%//%'plant.ini')

    cat("  - Resetting 'file.cio'\n")
    write_lines(mgt_raw$file_cio, path%//%'file.cio')

    cat("  - Resetting 'time.sim'\n")
    write_lines(mgt_raw$time_sim, path%//%'time.sim')

    interval(t0,now()) %>%
      round(.) %>%
      as.period(.) %>%
      as.character(.) %>%
      cat("Finished resetting management files in", ., "\n")

  } else if(version == '2012') {
    n_hru <- length(mgt_raw)
    t0 <- now()
    for (i_hru in 1:n_hru) {
      file_i <- names(mgt_raw)[i_hru]
      write_lines(mgt_raw[[file_i]], path%//%file_i%.%"mgt")
      display_progress_pct(i_hru, n_hru, t0)
    }
    finish_progress(n_hru, t0, "Finished resetting", "'.mgt' file")
  }
}

#' Reset the file 'hru-data.hru' file of a SWAT+ setup to the initial file
#'
#' @param mgt_raw List of original mgt files
#'
#' @keywords internal
#'
reset_hru_plus <- function(mgt_raw) {
  hru_names <- hru_to_string(names(mgt_raw$hru_data))
  hru_data <- mgt_raw$hru_data %>%
    apply(., 1, hru_to_string) %>%
    c(mgt_raw$hru_header, hru_names, .)
  return(hru_data)
}

#' Reset the file 'landuse.lum' file of a SWAT+ setup to the initial file
#'
#' @param mgt_raw List of original mgt files
#'
#' @keywords internal
#'
reset_lum_plus <- function(mgt_raw) {
  lum_names <- lum_to_string(names(mgt_raw$landuse_lum))
  landuse_lum <- mgt_raw$landuse_lum %>%
    apply(., 1, lum_to_string) %>%
    c(mgt_raw$luse_header, lum_names, .)
  return(landuse_lum)
}
