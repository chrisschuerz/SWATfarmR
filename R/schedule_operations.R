#' Schedule management operations for all HRUs based on static and dynamic rules
#'
#' @param mgt_schedule Tibble that provides mgt parameters and rules
#' @param hru_attribute Tibble that provides static HRU attributes
#' @param variables List of tibbles that provides time series of variables for
#'   all subbasins that can be implemented in dynamic rules.
#' @param lookup list of tibbles with lookup tables for plant, tillage,
#'   fertilizer codes from SWAT data base.
#'
#' @importFrom DBI dbClearResult dbConnect dbDisconnect dbReadTable dbSendStatement dbWriteTable
#' @importFrom dplyr filter select %>%
#' @importFrom lubridate now year years
#' @importFrom purrr map set_names
#' @importFrom rlang sym
#' @importFrom RSQLite SQLite
#' @importFrom stringr str_detect
#' @importFrom tidyselect all_of
#'
#' @keywords internal
#'
schedule_operation <- function(mgt_schedule, variables, lookup, hru_attribute,
                               var_con, start_year, end_year, n_schedule,
                               project_path, project_name, version) {
#Add n_schedule option to define how many repetitions per sub/rtu and crop should be done

  mgts_path <- paste0(project_path, '/', project_name, '.mgts')

  schedule <- list()
  schedule_con <- prepare_schedule_con(hru_attribute, version)
  op_skip <- NULL

  if(is.null(n_schedule)) n_schedule <- Inf

  unit_lbl <- ifelse(version == 'plus', 'rtu', 'sub')
  luse_lbl <- ifelse(version == 'plus', 'lu_mgt', 'luse')
  hru_lbl  <- ifelse(version == 'plus', 'hru_name', 'file') #Check for SWAT2012!
  init_lbl <- lookup$management$value[lookup$management$label == 'initial_plant']


  if(!is.null(start_year)) {
    stopifnot(is.numeric(start_year))
    variables <- map(variables, ~ filter(.x, year(date) >= start_year))
  }

  if(!is.null(end_year)) {
    stopifnot(is.numeric(end_year))
    variables <- map(variables, ~ filter(.x, year(date) <= end_year))
  }

  if(nrow(variables[[1]]) == 0) {
    stop("The selection of 'start_year' and 'end_year' resulted in empty",
         " time series for the 'variables'.")
  }

  var_years <- map(variables, ~ tibble(start_jdn = yday(.x$date[1]),
                                       start_yr  = year(.x$date[1]),
                                       end_jdn   = yday(.x$date[nrow(.x)]),
                                       end_yr    = year(.x$date[nrow(.x)]),
                                          )) %>%
    map2_df(., names(.), ~ mutate(.x, variable = .y, .before = 1)) %>%
    check_var_times(., start_year, end_year)

  if (file.exists(mgts_path)) {
    mgt_db <- dbConnect(SQLite(), mgts_path)
    assigned_hru <- dbReadTable(conn = mgt_db, name = 'assigned_hru') %>%
      as_tibble(.)
    schdl_yrs <- dbReadTable(conn = mgt_db, name = 'scheduled_years')
  } else {
    mgt_db <- dbConnect(SQLite(), mgts_path)
    assigned_hru <- hru_attribute %>%
      select(all_of(unit_lbl), hru, all_of(hru_lbl) ,all_of(luse_lbl)) %>%
      mutate(schedule = NA_character_, n = 0)

    dbWriteTable(conn = mgt_db, name = 'assigned_hru', value = assigned_hru)
    dbWriteTable(conn = mgt_db, name = 'scheduled_years', value = schdl_yrs)
  }
  dbDisconnect(mgt_db)

  t0 <- now()
  i_prg <- 1

  cat("Scheduling operations:\n")
  for(i_hru in hru_attribute$hru) {

    # if (i_hru == 76) {
    #   cat('debug')
    # }

    attribute_hru_i <- filter(hru_attribute, hru == i_hru)

    assigned_hru_i <- assigned_hru %>%
      filter(!!sym(unit_lbl) == attribute_hru_i[[unit_lbl]]) %>%
      filter(!!sym(luse_lbl) == attribute_hru_i[[luse_lbl]])

    n_max <- max(assigned_hru_i$n)

    if(n_max < n_schedule) {
      mgt_hru_i <- mgt_schedule %>%
        filter(land_use == attribute_hru_i[[luse_lbl]])

      if(attribute_hru_i[[luse_lbl]] %in% unique(mgt_schedule$land_use)) {
        mgt_hru_i <- sample_management(mgt_hru_i) %>%
          filter_attributes(., attribute_hru_i) %>%
          select(-land_use, -management, -weight, -filter_attribute)
      }

      schedule_i <- list(init_crop = NULL, schedule = NULL)

      if(nrow(mgt_hru_i) > 0) {
        has_only_init <- all(mgt_hru_i$operation == init_lbl)

        if(has_only_init){
          schedule_i$init_crop <- schedule_init(mgt_hru_i, version)
        } else {
          j_op <- 1
          n_op <- nrow(mgt_hru_i)

          var_tbl <- prepare_variables(variables, var_con, i_hru, version)
          date_j <- var_tbl$date[1]

          prev_op <- date_j

          repeat{
            mgt_j <- mgt_hru_i[j_op,]
            var_tbl <- filter(var_tbl, date >= prev_op)

            if(nchar(mgt_j$condition_schedule) > 0 &
               !is.na(mgt_j$condition_schedule) &
               mgt_j$operation != init_lbl) {

              str_check <- str_remove_all(mgt_j$condition_schedule, '[:space:]')

              if(all(str_detect(str_check, c('year\\=\\=', 'year\\(prev_op\\)\\+1'))) &
                 is.null(schedule_i$schedule)) {
                prev_op <- prev_op - years(1)
              }

              date_j <- schedule_date_j(var_tbl, mgt_j$condition_schedule, prev_op)

              if(is.null(date_j) & mgt_j$operation == 'plnt') {
                stop('Scheduling operation number ', j_op, ' for the HRU ', i_hru,
                     " with the land_use '", hru_attribute$lu_mgt[i_hru], "' failed! \n",
                     'The date resulted in a NULL value. \n',
                     'A reason can be that the date ranges of the lines ', j_op - 1, ' and ',
                     j_op, " for the land_use '", hru_attribute$lu_mgt[i_hru], "' overlap.\n",
                     'Please fix and reload the management input table accordingly \n',
                     'and repeat the scheduling of the operations.')
              } else if(is.null(date_j)){
                op_skip <- document_op_skip(op_skip, attribute_hru_i, mgt_j, prev_op, j_op, version)
              } else if (date_j >= max(var_tbl$date)) {
                break()
              } else {
                prev_op <- date_j
              }
            }

            var_tbl <- compute_hu(var_tbl, mgt_j, lookup, date_j, schedule_i, version, i_hru)
            schedule_i <- schedule_op_j(schedule_i, mgt_j, date_j, init_lbl, version)
            j_op <- ifelse(j_op < n_op, j_op + 1, 1)
          }

          schedule_i$schedule <- add_end_year_flag(schedule_i$schedule, lookup)
          schedule_i$schedule <- add_skip_year_flag(schedule_i$schedule, variables[[1]], lookup)
          # schedule_i$schedule$date[schedule_i$schedule$operation == 0] <- NA
        }
      }
      if(is.null(schedule_i$schedule)) {
        schedule_name <- attribute_hru_i[[luse_lbl]]
        n_i <- 0
      } else {
        schedule_name <- attribute_hru_i[[luse_lbl]]%_%attribute_hru_i[[unit_lbl]]%_%(n_max + 1)
        n_i <- n_max + 1
      }

        assigned_hru[assigned_hru$hru == i_hru, 5] <- schedule_name
        assigned_hru[assigned_hru$hru == i_hru, 6] <- n_i

        mgt_db <- dbConnect(SQLite(), paste0(project_path, '/', project_name, '.mgts'))

        sql_schdl <- paste0("UPDATE assigned_hru SET schedule = '", schedule_name, "' WHERE hru = ", i_hru)
        rs <- dbSendStatement(conn = mgt_db, statement = sql_schdl)
        dbClearResult(rs)
        sql_n <- paste0("UPDATE assigned_hru SET n = '", n_i, "' WHERE hru = ", i_hru)
        rs <- dbSendStatement(conn = mgt_db, statement = sql_n)
        dbClearResult(rs)

        init_already_assigned <- n_i == 0 & schedule_name %in% assigned_hru$schedule
        if(!is.null(schedule_i$init_crop) & !init_already_assigned) {
          dbWriteTable(conn = mgt_db, name = paste0('init::',schedule_name), value = schedule_i$init_crop)
        }
        if(!is.null(schedule_i$schedule)) {
          dbWriteTable(conn = mgt_db, name = paste0('schd::',schedule_name), value = schedule_i$schedule)
        }
        dbDisconnect(mgt_db)
    } else {
      assign_i <- assigned_hru_i %>%
        filter(n > 0) %>%
        sample_n(., 1)

      assigned_hru[assigned_hru$hru == i_hru, c(5,6)] <- assign_i[,c(5,6)]

      sql_schdl <- paste0("UPDATE assigned_hru SET schedule = '", assign_i[[5]], "' WHERE hru = ", i_hru)
      rs <- dbSendStatement(conn = mgt_db, statement = sql_schdl)
      dbClearResult(rs)
      sql_n     <- paste0("UPDATE assigned_hru SET n = '", assign_i[[6]], "' WHERE hru = ", i_hru)
      rs <- dbSendStatement(conn = mgt_db, statement = sql_n)
      dbClearResult(rs)
    }
    display_progress(i_prg, nrow(hru_attribute), t0, "HRU")
    i_prg <- i_prg + 1
  }
  finish_progress(nrow(hru_attribute), t0, "Finished scheduling", "HRU")

  if (version == '2012') {
    return(list(scheduled_operations = schedule,
                assigned_hrus = assigned_hru,
                skipped_operations   = op_skip,
                scheduled_years = schdl_yrs))
  } else {
    return(list(scheduled_operations = schedule,
                assigned_hrus = assigned_hru,
                skipped_operations   = op_skip,
                scheduled_years = schdl_yrs))
  }
}

#' Prepare the management schedule connection table
#'
#' @param hru_attribute Tibble that provides the HRU attributes
#' @param version label indicating the SWAT version
#'
#' @importFrom dplyr mutate rename select %>%
#'
#' @keywords internal
#'
prepare_schedule_con <- function(hru_attribute, version) {
  if (version == 'plus') {
    schedule_con <- hru_attribute %>%
      select(rtu, hru, lu_mgt) %>%
      mutate(luse_upd = NA,
             mgt = NA,
             id  = NA) %>%
      rename(sub = rtu, luse = lu_mgt)
  } else if (version == '2012') {
    schedule_con <- hru_attribute %>%
      select(sub, hru, luse) %>%
      mutate(luse_upd = NA,
             mgt = NA,
             id  = NA)
  }
  return(schedule_con)
}

#' Sample management type if different management types are provided for the
#' same land use
#'
#' @param mgt_tbl Tibble that provides mgt parameters and rules
#'
#' @importFrom dplyr filter group_by sample_n summarise %>%
#'
#' @keywords internal
#'
sample_management <- function(mgt_tbl) {

  # unique_mgt <- unique(mgt_tbl$management)
  # unique_wgt <- unique(mgt_tbl$weight)
  # mgt_check <- (all(is.na(unique_mgt)) | all(!is.na(unique_mgt))) &
  #              (all(is.na(unique_wgt)) | all(!is.na(unique_wgt)))
  #
  # if(!mgt_check){
  #   stop("Issues found in the 'management' and 'weight' definition of '",
  #        mgt_tbl$land_use[1], "'.\n  Either all ar NA or all managements are named",
  #        " and do have weights.")
  # }

  if(!all(is.na(mgt_tbl$management))) {
    mgt_sel <- mgt_tbl %>%
      filter(., !is.na(management)) %>%
      group_by(management) %>%
      summarise(weight = max(weight, na.rm = TRUE))

    if (nrow(mgt_sel) > 0) {
      mgt_sel <- sample_n(mgt_sel, 1, weight = weight) %>%
      .$management
      mgt_tbl <- mgt_tbl %>%
        filter(., is.na(management) | management == mgt_sel)
    }
  }

  return(mgt_tbl)
}

#' Filter the operations based on the defined static rules
#'
#' @param mgt_tbl Tibble that provides mgt parameters and rules
#' @param attribute_hru_i Tibble with one line that provides the static HRU
#'   attributes
#'
#' @importFrom dplyr transmute %>%
#' @importFrom purrr map_df
#' @importFrom rlang parse_expr
#'
#' @keywords internal
#'
filter_attributes <- function(mgt_tbl, attribute_hru_i) {
  mgt_tbl$filter_attribute[is.na(mgt_tbl$filter_attribute)] <- TRUE

  sel_rule <- map_df(mgt_tbl$filter_attribute,
         ~transmute(attribute_hru_i, sel = !!parse_expr(.x))) %>%
    unlist()

  mgt_tbl[sel_rule,]
}

#' Schedule the initial crop operation
#'
#' @param mgt_op Tibble with one line for the current operation
#'
#' @importFrom dplyr %>%
#' @importFrom purrr set_names
#' @importFrom tibble as_tibble_row
#'
#' @keywords internal
#'
schedule_init <- function(mgt_op, version) {
  if (version == 'plus') {
    plnt_ini <- str_split(mgt_op$op_data2, ',', simplify = TRUE) %>%
      as.numeric(.) %>%
      set_names(., c('lai_init', 'bm_init', 'phu_init', 'plnt_pop', 'yrs_init', 'rsd_init')) %>%
      as_tibble_row(.) %>%
      add_column(., plt_name = mgt_op$op_data1, .before = 1)
  } else {
    plnt_ini <- set_names(mgt_op[1,3:6],c("plant_id", "lai_init", "bio_init", "phu_plant"))
  }
  return(plnt_ini)
}

#' Prepare the variable table for the respective HRU
#'
#' @param var_list List of tibbles that provides time series of variables for
#'   all subbasins that can be implemented in dynamic rules.
#' @param subbasin Number of the subbasin in which the HRU is located
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom lubridate yday ymd
#' @importFrom purrr map map_chr reduce set_names
#' @importFrom stringr str_remove
#' @importFrom tidyselect any_of
#'
#' @keywords internal
#'
prepare_variables <- function(var_list, hru_var_con, i_hru, version) {
  pcp_pos <- which(names(hru_var_con) == 'pcp')
  con_i <- filter(hru_var_con, hru == i_hru) %>%
    .[,pcp_pos:ncol(.)]

  # var_tbl <- map2(con_i, names(con_i), ~ select(var_list[[.y]], date, ends_with(.x))) %>%
  var_tbl <- map2(con_i, names(con_i), ~ var_list[[.y]][,c('date', .x)]) %>%
    map2(., names(.), ~ set_names(.x, c('date', .y))) %>%
    reduce(., left_join, by = 'date') %>%
    mutate(year = year(date),
           month = month(date),
           day = day(date),
           jdn = yday(date),
           md = 100*month + day,
           ymd = year*1e4 + md,
           .after = 1)

  if (version == '2012') {
    var_tbl <- mutate(var_tbl, hu = NA, hu_fr = NA)
  }

  return(var_tbl)
}

#' Schedule the operation date for the operation j
#'
#' @param var_tbl Tibble with the variable time series for the HRU
#' @param eval_str Text string expression of the dynamic rule that is evaulated
#'   to select the date.
#' @param prev_op Date of the previous opeation in ymd() format.
#'
#' @importFrom dplyr filter mutate sample_n %>%
#' @importFrom lubridate year ymd
#' @importFrom rlang parse_expr
#'
#' @keywords internal
#'
schedule_date_j <- function(var_tbl, eval_str, prev_op) {
  wgt_tbl <- var_tbl %>%
    mutate(wgt = !!parse_expr(eval_str))

  if(sum(wgt_tbl$wgt) > 0) {
    date_j <- wgt_tbl %>%
      # filter(wgt > 0) %>%
      # group_by(year) %>%
      sample_n(., 1, weight = wgt) %>%
      # ungroup() %>%
      # filter(year == min(year)) %>%
      .$date
  } else if (year(prev_op) == max(var_tbl$year)) {
    date_j <- ymd(99991231)
  } else {
    date_j <- NULL
  }
  # Deactivated catch function that checks if all weights 0. This is necessary
  # if no setting of operations should be a valid option.
  # else {
  #   stop("Scheduling operation ", j_op, " in HRU ", hru_attr$hru,
  #        " with the land use ", hru_attr$luse, " resulted in all zero weights!\n",
  #        "No date could be scheduled. Please use less strict rules for",
  #        " this operation and repeat the scheduling.")
  # }
  return(date_j)
}

#' Add the operation j in the opeation schedule for HRU i
#'
#' @param schedule_i Scheduled operations table for the HRU i
#' @param mgt_j j_th line of the mgt table that should be scheduled
#' @param date_j The calculated date for the mgt operation j
#'
#' @importFrom dplyr bind_rows select %>%
#' @importFrom purrr set_names
#' @importFrom tibble add_column
#'
#' @keywords internal
#'
schedule_op_j <- function(schedule_i, mgt_j, date_j, init_lbl, version) {
  if(mgt_j$operation == init_lbl & is.null(schedule_i$init_crop)) {
    schedule_i$init_crop <- schedule_init(mgt_j, version)
  } else if (mgt_j$operation != init_lbl & !is.null(date_j)) {
    op <- mgt_j %>%
      select(-condition_schedule) %>%
      add_column(date = date_j, .before = 1)

    if (is.null(schedule_i$schedule)) {
      schedule_i$schedule <- op
    } else {
      schedule_i$schedule <- bind_rows(schedule_i$schedule, op)
    }
  }
  return(schedule_i)
}

#' Document if an operation scheduling did not result in an operation date
#'
#' @param op_skip Tibble that documents the skipped operations
#' @param attribute_hru_i Tibble with one line that provides the static HRU
#' @param mgt_j j_th line of the mgt table that should be scheduled
#' @param prev_op Date of the previous opeation in ymd() format.
#' @param j_op index of the operation in the mgt schedule table.
#' @param version Text string that provides the SWAT version
#'
#' @importFrom dplyr bind_rows filter mutate select %>%
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
document_op_skip <- function(op_skip, attribute_hru_i, mgt_j, prev_op, j_op, version) {
  if (version == '2012') {
    skip_i <- tibble(file         = attribute_hru_i$file,
                     subbasin     = attribute_hru_i$subbasin,
                     hru          = attribute_hru_i$hru,
                     landuse      = attribute_hru_i$luse,
                     op_number    = j_op,
                     date_prev_op = prev_op,
                     rule         = mgt_j$condition_schedule,
                     mgt_op_code  = mgt_j$operation)
  } else if (version == 'plus') {
    skip_i <- tibble(rtu          = attribute_hru_i$rtu,
                     hru          = attribute_hru_i$hru,
                     landuse      = attribute_hru_i$lu_mgt,
                     op_number    = j_op,
                     date_prev_op = prev_op,
                     rule         = mgt_j$condition_schedule,
                     mgt_op_code  = mgt_j$operation)
  }
  if (is.null(op_skip)) {
    op_skip <- skip_i
  } else {
    op_skip <- bind_rows(op_skip, skip_i)
  }
  return(op_skip)
}

compute_hu <- function(var_tbl, mgt_j, lookup, date_j, schedule_i, version, i_hru) {

  init_lbl <- lookup$management$value[lookup$management$label == 'initial_plant']
  plnt_lbl <- lookup$management$value[lookup$management$label == 'plant']
  kill_lbls <- lookup$management$value[lookup$management$label %in% c('harvest_kill', 'kill_only')]
  harv_lbl  <- lookup$management$value[lookup$management$label == 'harvest_only']
  op_i <- mgt_j$operation

  if (version == '2012') {
    if(op_i == init_lbl & !is.null(date_j) & is.null(schedule_i$init_crop)) {
      plant_i <- mgt_j$mgt1
      t_base <- filter(lookup$plant, value == plant_i) %>% .$t_base %>% .[1]
      phu <- mgt_j$mgt4

      var_tbl <- var_tbl %>%
        mutate(hu = ifelse(date >= date_j, tav - t_base, 0),
               hu = ifelse(hu > 0, hu, 0),
               hu = cumsum(hu),
               hu_fr = hu / phu)
    } else if (op_i == plnt_lbl & !is.null(date_j)) {
      plant_i <- mgt_j$mgt1
      t_base <- filter(lookup$plant, value == plant_i) %>% .$t_base %>% .[1]
      phu <- mgt_j$mgt4

      var_tbl <- var_tbl %>%
        mutate(hu = ifelse(date >= date_j, tav - t_base, 0),
               hu = ifelse(hu > 0, hu, 0),
               hu = cumsum(hu),
               hu_fr = hu / phu)
    } else if(op_i %in% kill_lbls & !is.null(date_j)) {
      var_tbl <- var_tbl %>%
        mutate(hu = NA,
               hu_fr = NA)
    } else if(op_i == harv_lbl & !is.null(date_j)) {
      if(!is.na(mgt_j$mgt5)) {
        var_tbl <- var_tbl %>%
          mutate(hu = hu*(1 - mgt_j$mgt5),
                 hu_fr = hu_fr*(1 - mgt_j$mgt5))
      }
    }
  } else if (version == 'plus') {
    if(op_i == init_lbl & !is.null(date_j) & is.null(schedule_i$init_crop)) {
      plant_i <- mgt_j$op_data1
      hu_i <- mgt_j$op_data2 %>%
          str_split(., ',', simplify = TRUE) %>%
          .[3] %>%
          as.numeric(.)

      t_base <- lookup$plant[lookup$plant$name == plant_i,]$t_base

      var_tbl <- t_base %>%
        map(., ~ transmute(var_tbl, hu = ifelse(date >= date_j, tav - ., 0),
                                    hu = ifelse(hu > 0, hu, 0),
                                    hu = cumsum(hu),
                                    grw = ifelse(date >= date_j, 1, 0),
                                    grw = cumsum(grw))) %>%
        map2(., hu_i, ~ mutate(.x, hu = hu + .y)) %>%
        map2(., plant_i, ~set_names(.x, names(.x)%_%.y)) %>%
        bind_cols(var_tbl, .)
    } else if (op_i == plnt_lbl & !is.null(date_j)) {
      plant_i <- mgt_j$op_data1

      if('grw'%_%plant_i %in% names(var_tbl)) {
        # warning('HU for ', plant_i,' in HRU ', i_hru , ' overwritten.')
        # var_tbl <- select(var_tbl, -(c('hu', 'grw')%_%plant_i))
        stop("Plant '", plant_i,"' is already growing on HRU ", i_hru , '.\n',
             "Planting of '", plant_i,"' was scheduled another time before killing the plant.",
             '\nThere may be an issue with harvesting and killing this plant.\n',
             'Please check the management inputs for HRU ', i_hru, ' and fix the issue',
             '\nbefore you restart the management scheduling.')
      }

      t_base <- filter(lookup$plant, name == plant_i) %>% .$t_base
      var_tbl <- var_tbl %>%
        transmute(., hu = ifelse(date >= date_j, tav - t_base, 0),
                     hu = ifelse(hu > 0, hu, 0),
                     hu = cumsum(hu),
                     grw = ifelse(date >= date_j, 1, 0),
                     grw = cumsum(grw)) %>%
        set_names(., names(.)%_%plant_i) %>%
        bind_cols(var_tbl, .)
    } else if(op_i %in% kill_lbls & !is.null(date_j)) {
      plant_i <- mgt_j$op_data1
      var_tbl <- select(var_tbl, -(c('hu', 'grw')%_%plant_i))
    } else if(op_i == harv_lbl & !is.null(date_j)) {
      var_tbl <- var_tbl #maybe other considerations for hrvst only in future
    }
  }

  return(var_tbl)
}

#' Add the end of year flag at the end of each year in the scheduled mgt table
#'
#' @param schedule_tbl Tibble providing the scheduled operations for HRU i
#'
#' @importFrom dplyr bind_rows group_split mutate select %>%
#' @importFrom lubridate year ymd
#' @importFrom purrr map
#' @importFrom tibble add_row
#'
#' @keywords internal
#'
add_end_year_flag <- function(schedule_tbl, lookup) {
  op <- lookup$management$value[lookup$management$label == 'end_year']
  schedule_tbl <- schedule_tbl %>%
  mutate(yr = year(date)) %>%
  group_split(., yr) %>%
  map(., ~add_row(.x, date = ymd(.x$yr[1]%-%12%-%31), operation = op)) %>%
    bind_rows(.) %>%
    select(-yr)

  return(schedule_tbl)

  # Catch unequal number of operations deactivated. Want to keep the option of
  # skipping operations if no date is fits (all weights 0). Equal to if statement.
  # n_op_year <- map_int(schedule_list, nrow)
  # if(any(n_op_year != mean(n_op_year))) {
  #   stop("The scheduling in HRU ", hru_attr$hru," with the land use ",
  #        hru_attr$luse, " failed due to too strict date boundaries!\n",
  #        "Please use less strict date boundaries in the management schedule table",
  #        " and repeat the scheduling.")
  # }
}

#' Add the skip year flag for years in the scheduled mgt table where no
#' operations were set.
#'
#' @param schedule_tbl Tibble providing the scheduled operations for HRU i
#' @param variable Tibble of a variable required to extract all years
#'
#' @importFrom dplyr full_join group_split mutate select %>%
#' @importFrom lubridate year
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
add_skip_year_flag <- function(schedule_tbl, variable, lookup) {
  skip_flag <- lookup$management$value[lookup$management$label == 'skip']
  year_tbl <- tibble(yr = unique(year(variable$date)))

  schedule_tbl <- schedule_tbl %>%
    mutate(yr = year(date)) %>%
    full_join(., year_tbl, by = "yr") %>%
    group_split(., yr) %>%
    map_df(., ~ set_skip_flag(.x, skip_flag)) %>%
    select(-yr)

  return(schedule_tbl)
}

#' Set the skip flag for entries in the list of tibbles with only one row and
#' all NA values for operation
#'
#' @param tbl Tibble in the list of tibbles grouped by years
#'
#' @keywords internal
#'
set_skip_flag <- function(tbl, skip_flag) {
  if(nrow(tbl) == 1 & any(is.na(tbl$operation))) {
    tbl$operation <- skip_flag
  }
  return(tbl)
}

#' Check if all variables in the farmR project cover the entire time window
#' which was defined by the start and end years.
#'
#' @param var_time Overview table of time windows for all variables
#' @param start_year User defined start year for operation scheduling
#' @param end_year   User defined end year for operation scheduling
#'
#' @importFrom lubridate leap_year
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
check_var_times <- function(var_time, start_year, end_year) {

  is_leapyr <- leap_year(max(var_time$end_yr))

  wrong_start_jdn <- var_time$start_jdn != 1
  wrong_start_yr  <- var_time$start_yr  != min(start_year, var_time$start_yr)
  wrong_end_jdn   <- var_time$end_jdn   != ifelse(is_leapyr, 366, 365)
  wrong_end_yr    <- var_time$end_yr    != min(end_year, var_time$end_yr)

  if(any(wrong_start_jdn)) {
    var_wrong_start_jdn <- var_time$variable[which(wrong_start_jdn)]
    msg_wrong_start_jdn <- paste0('The variable time series do not start on the first day of the start_year:',
                                  '\n', paste(var_wrong_start_jdn, collapse = ', '), '\n\n')
  } else {
    msg_wrong_start_jdn <- ''
  }

  if(any(wrong_start_yr)) {
    var_wrong_start_yr <- var_time$variable[which(wrong_start_yr)]
    msg_wrong_start_yr <- paste0('The variable time series do not start in the start_year:',
                                 '\n', paste(var_wrong_start_yr, collapse = ', '), '\n\n')
  } else {
    msg_wrong_start_yr <- ''
  }

  if(any(wrong_end_jdn)) {
    var_wrong_end_jdn <- var_time$variable[which(wrong_end_jdn)]
    msg_wrong_end_jdn <- paste0('The variable time series do not end on the last day of the end_year:',
                                '\n', paste(var_wrong_end_jdn, collapse = ', '), '\n\n')
  } else {
    msg_wrong_end_jdn <- ''
  }

  if(any(wrong_end_yr)) {
    var_wrong_end_yr <- var_time$variable[which(wrong_end_yr)]
    msg_wrong_end_yr <- paste0('The variable time series do not end in the end_year:',
                               '\n', paste(var_wrong_end_yr, collapse = ', '), '\n\n')
  } else {
    msg_wrong_end_yr <- ''
  }

  msg_wrong_dates <- c(msg_wrong_start_jdn, msg_wrong_start_yr,
                       msg_wrong_end_jdn, msg_wrong_end_yr)

  if(any(nchar(msg_wrong_dates) > 0)) {
    stop('The following issues were identified for the dates of the variable time series:\n\n',
         msg_wrong_dates)
  }

  var_years <- tibble(start_year = var_time$start_yr[1],
                      end_year   = var_time$end_yr[1])

  return(var_years)
}
