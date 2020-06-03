#' Schedule management operations for all HRUs based on static and dynamic rules
#'
#' @param mgt_schedule Tibble that provides mgt parameters and rules
#' @param hru_attribute Tibble that provides static HRU attributes
#' @param variables List of tibbles that provides time series of variables for
#'   all subbasins that can be implemented in dynamic rules.
#' @param lookup list of tibbles with lookup tables for plant, tillage,
#'   fertilizer codes from SWAT data base.
#'
#' @importFrom dplyr filter select %>%
#' @importFrom lubridate now
#' @importFrom purrr set_names
#'
schedule_operation <- function(mgt_schedule, hru_attribute, variables, lookup) {
  schedule <- list()
  op_skip <- NULL
  t0 <- now()
  #-------------------------------------------------------------------------------
  # Possible parallel computing implementation. Little sketchy workaround with functions
  # library(parallel)
  # library(doSNOW)
  # cl <- makeCluster(4)
  # registerDoSNOW(cl)
  # n_hru <- nrow(hru_attribute)

  # cat("Scheduling operations:")
  # t0 <- now()
  # progress <- function(n){
  #   SWATfarmR:::display_progress(n, n_hru, t0, "HRU")
  # }
  # opts <- list(progress = progress)
  #
  # sim_result <- foreach(i_run = 1:n_hru,
  #                       .packages = c("dplyr", "lubridate", "purrr", "pasta", "SWATfarmR", "tibble", "stringr", "rlang"),
  #                       .export=ls.str(envir=globalenv()),
  #                       .options.snow = opts) %dopar% {
  cat("Scheduling operations:")
  for(i_hru in hru_attribute$hru) {
    attribute_hru_i <- hru_attribute[i_hru,]

    mgt_hru_i <- mgt_schedule %>%
      filter(land_use == attribute_hru_i$luse) %>%
      sample_management(.) %>%
      filter_static_rules(., attribute_hru_i) %>%
      select(-land_use, -management, -weight, -rules_static)

    schedule_i <- list(init_crop = NULL, schedule = NULL)

    if(nrow(mgt_hru_i) > 0) {
      if(all(mgt_hru_i$operation == 99)){
        schedule_i$init_crop <- mgt_j[,3:6] %>%
          set_names(c("plant_id", "lai_init", "bio_init", "phu_plant"))
      } else {
        j_op <- 1
        n_op <- nrow(mgt_hru_i)

        var_tbl <- prepare_variables(variables, attribute_hru_i$subbasin)
        date_j <- var_tbl$date[1]
        prev_op <- date_j

        repeat{
          mgt_j <- mgt_hru_i[j_op,]
          var_tbl <- filter(var_tbl, date >= prev_op)

          if(nchar(mgt_j$rules_dynamic) > 0 &
             !is.na(mgt_j$rules_dynamic) &
             mgt_j$operation != 99) {
            date_j <- schedule_date_j(var_tbl, mgt_j$rules_dynamic, prev_op,
                                      attribute_hru_i, j_op)

            if(is.null(date_j)){
              op_skip <- document_op_skip(op_skip, attribute_hru_i, mgt_j, prev_op, j_op)
            } else if (date_j >= max(var_tbl$date)) {
              break()
            } else {
              prev_op <- date_j
            }
          }

          schedule_i <- schedule_op_j(schedule_i, mgt_j, date_j)
          var_tbl <- compute_hu(var_tbl, mgt_j, lookup$plant, date_j)
          j_op <- ifelse(j_op < n_op, j_op + 1, 1)
        }

        schedule_i$schedule <- add_end_year_flag(schedule_i$schedule, attribute_hru_i)
        schedule_i$schedule <- add_skip_year_flag(schedule_i$schedule, variables[[1]])
        schedule_i$schedule$date[schedule_i$schedule$operation == 0] <- NA
      }
    }
    schedule[[i_hru]] <- schedule_i
    display_progress(i_hru, nrow(hru_attribute), t0, "HRU")
  }
  SWATfarmR:::finish_progress(nrow(hru_attribute), t0, "Finished scheduling for", "HRU")
  return(list(scheduled_operations = schedule,
              skipped_operations   = op_skip))
}

#' Sample management type if different management types are provided for the
#' same land use
#'
#' @param mgt_tbl Tibble that provides mgt parameters and rules
#'
#' @importFrom dplyr filter group_by sample_n summarize  %>%
#'
sample_management <- function(mgt_tbl) {
  mgt_sel <- mgt_tbl %>%
    group_by(management) %>%
    summarise(weight = max(weight, na.rm = TRUE)) %>%
    sample_n(., 1, weight = weight) %>%
    .$management

  mgt_tbl %>%
    filter(management == mgt_sel)
}

#' Filter the operations based on the defined static rules
#'
#' @param mgt_tbl Tibble that provides mgt parameters and rules
#' @param hru_attribute_i Tibble with one line that provides the static HRU
#'   attributes
#'
#' @importFrom dplyr transmute %>%
#' @importFrom purrr map_df
#' @importFrom rlang parse_expr
#'
filter_static_rules <- function(mgt_tbl, hru_attribute_i) {
  mgt_tbl$rules_static[is.na(mgt_tbl$rules_static)] <- TRUE

  sel_rule <- map_df(mgt_tbl$rules_static,
         ~transmute(hru_attribute_i, sel = !!parse_expr(.x))) %>%
    unlist()

  mgt_tbl[sel_rule,]
}

#' Prepare the variable table for the respective HRU
#'
#' @param var_list List of tibbles that provides time series of variables for
#'   all subbasins that can be implemented in dynamic rules.
#' @param subbasin Number of the subbasin in which the HRU is located
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom lubridate ymd
#' @importFrom purrr map map_chr reduce set_names
#' @importFrom stringr str_remove
#' @importFrom tidyselect any_of
#'
prepare_variables <- function(var_list, subbasin) {
  var_names <- map_chr(var_list, ~colnames(.x)[5]) %>%
    str_remove(., "_[:digit:]+$")

  var_col_sel <- var_names%_%subbasin

  var_tbl <- var_list %>%
    map(., ~select(.x, year, month, day, jdn, any_of(var_col_sel))) %>%
    reduce(., left_join, by = c("year","month", "day", "jdn")) %>%
    set_names(., str_remove(colnames(.), "_[:digit:]+$")) %>%
    mutate(md = 100*month + day,
           ymd = year*1e4 + md,
           date = ymd(ymd),
           hu = NA,
           hu_fr = NA)

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
schedule_op_j <- function(schedule_i, mgt_j, date_j) {
  if(mgt_j$operation == 99 & is.null(schedule_i$init_crop)) {
    schedule_i$init_crop <- mgt_j[,3:6] %>%
      set_names(c("plant_id", "lai_init", "bio_init", "phu_plant"))
  } else if (mgt_j$operation != 99 & !is.null(date_j)) {
    op <- mgt_j %>%
      select(-rules_dynamic) %>%
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
#' @param hru_attribute_i Tibble with one line that provides the static HRU
#' @param mgt_j j_th line of the mgt table that should be scheduled
#' @param prev_op Date of the previous opeation in ymd() format.
#' @param j_op index of the operation in the mgt schedule table.
#'
#' @importFrom dplyr bind_rows filter mutate select %>%
#' @importFrom tibble tibble
#'
document_op_skip <- function(op_skip, hru_attribute_i, mgt_j, prev_op, j_op) {
  skip_i <- tibble(subbasin     = hru_attribute_i$subbasin,
                   hru          = hru_attribute_i$hru,
                   landuse      = hru_attribute_i$luse,
                   op_number    = j_op,
                   date_prev_op = prev_op,
                   rule         = mgt_j$rules_dynamic,
                   mgt_op_code  = mgt_j$operation)
  if (is.null(op_skip)) {
    op_skip <- skip_i
  } else {
    op_skip <- bind_rows(op_skip, skip_i)
  }
  return(op_skip)
}

compute_hu <- function(var_tbl, mgt_j, plant_dat, date_j) {
  op_i <- mgt_j$operation
  if(op_i %in% c(1, 99) & !is.null(date_j)) {
    plant_i <- mgt_j$mgt1
    t_base <- filter(plant_dat, value == plant_i) %>% .$t_base
    phu <- mgt_j$mgt4
    var_tbl <- var_tbl %>%
      mutate(hu = ifelse(date >= date_j, tav - t_base, 0),
             hu = ifelse(hu > 0, hu, 0),
             hu = cumsum(hu),
             hu_fr = hu / phu)
  }

  if(op_i %in% c(5, 8) & !is.null(date_j)) {
    var_tbl <- var_tbl %>%
      mutate(hu = NA,
             hu_fr = NA)
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
add_end_year_flag <- function(schedule_tbl) {
  schedule_tbl <- schedule_tbl %>%
  mutate(yr = year(date)) %>%
  group_split(., yr) %>%
  map(., ~add_row(.x, date = ymd(.x$yr[1]%-%12%-%31), operation = 0)) %>%
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
add_skip_year_flag <- function(schedule_tbl, variable) {
  year_tbl <- tibble(yr = unique(variable$year))

  schedule_tbl <- schedule_tbl %>%
    mutate(yr = year(date)) %>%
    full_join(., year_tbl, by = "yr") %>%
    group_split(., yr) %>%
    map_df(., set_skip_flag) %>%
    select(-yr)

  return(schedule_tbl)
}

#' Set the skip flag for entries in the list of tibbles with only one row and
#' all NA values for operation
#'
#' @param tbl Tibble in the list of tibbles grouped by years
#'
set_skip_flag <- function(tbl) {
  if(nrow(tbl) == 1 & any(is.na(tbl$operation))) {
    tbl$operation <- 17
  }
  return(tbl)
}















