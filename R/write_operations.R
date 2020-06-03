
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
        filter(year(date) <= end_year)

      mgt_i <- add_management_schedule(mgt_i, schedule_tbl)
    } else if(write_all) {
      mgt_i <- mgt_i[1:30]
      mgt_file[29] <- paste(sprintf("%16i", 1),
                            "   | NROT: number of years of rotation")
      mgt_i <- c(mgt_i, "                17")
    }
    write_lines(mgt_i, path%//%hru_file_i%.%"mgt")
    SWATfarmR:::display_progress_pct(i_hru, n_hru, t0)
  }
  SWATfarmR:::finish_progress(nrow(hru_attribute), t0, "Finished writing", "'.mgt' file")
}

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
