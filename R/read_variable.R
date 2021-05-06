#' Read the weather data
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#' @param version String that indicates what SWAT version the project is.
#'
#' @keywords internal
#'
#'
read_weather <- function(project_path, version) {
  if(version == 'plus') {
    wth_dat <- read_weather_plus(project_path)
  } else if(version == '2012') {
    wth_dat <- read_weather_2012(project_path)
  }
  return(wth_dat)
}

#' Read the weather data for a SWAT+ project
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom dplyr mutate select everything %>%
#' @importFrom lubridate month day
#' @importFrom purrr map map2_df reduce set_names
#' @importFrom readr cols col_character col_double col_integer read_table
#' @importFrom stringr str_remove
#' @importFrom tibble add_column
#' @importFrom tidyselect ends_with
#'
#' @keywords internal
#'
#testing
# project_path <- "C:/swat_testing/Kinzig/TxtInOut"
# library(tidyverse)

read_weather_plus <- function(project_path) {
  weather_sta <- read_table(project_path%//%'weather-sta.cli',
                            col_types = cols(.default = col_character()),
                            skip = 1)

  pcp_files <- unique(weather_sta$pcp)
  tmp_files <- unique(weather_sta$tmp)

  check_if_daily(pcp_files, project_path)
  check_if_daily(tmp_files, project_path)

  pcp <- map(pcp_files, ~ read_table(project_path%//%.x,
                                     col_names = c('year', 'jdn',
                                                   str_remove(.x, '.pcp')),
                                     col_types = cols(year = col_integer(),
                                                      jdn  = col_integer(),
                                                      .default = col_double()),
                                     skip = 3, progress = FALSE)) %>%
    map(., ~ jdnyr_to_date(.x)) %>%
    reduce(., full_join, by = 'date')

  tmp <- map(tmp_files, ~ read_table(project_path%//%.x,
                                     col_names = c('year', 'jdn',
                                                   str_remove(.x, '.tmp')%_%'max',
                                                   str_remove(.x, '.tmp')%_%'min'),
                                     col_types = cols(year = col_integer(),
                                                      jdn  = col_integer(),
                                                      .default = col_double()),
                                     skip = 3, progress = FALSE)) %>%
    map(., ~ jdnyr_to_date(.x)) %>%
    reduce(., full_join, by = 'date')

  tmax <- select(tmp, date, ends_with("_max")) %>%
    set_names(., str_remove(names(.), "_max"))
  tmin <- select(tmp, date, ends_with("_min"))%>%
    set_names(., str_remove(names(.), "_min"))
  tav  <- map2_df(select(tmax, -date), select(tmin, -date), ~ (.x + .y)/2) %>%
    add_column(., date = tmin$date, .before = 1)

  return(list(pcp = pcp, tmax = tmax, tmin = tmin, tav = tav))
}

#' Read the weather data for a SWAT2012 project
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom dplyr mutate select everything %>%
#' @importFrom lubridate month day
#' @importFrom purrr map2_df set_names
#' @importFrom readr cols fwf_widths read_fwf read_lines
#' @importFrom tibble add_column
#' @importFrom tidyselect starts_with
#'
#' @keywords internal
#'
#'
#testing
# project_path <- "C:/swat_testing/Kinzig/TxtInOut"
# library(tidyverse)

read_weather_2012 <- function(project_path) {

  weather_file <- list.files(project_path)
  weather_file <- weather_file[tolower(weather_file) %in% c("pcp1.pcp", "tmp1.tmp")]
  pcp_names <- get_station_names(project_path%//%weather_file[1])
  tmp_names <- get_station_names(project_path%//%weather_file[2])

  pcp <- read_weather_file(file = project_path%//%weather_file[1],
                      var = "pcp", skip = 4, digit_var = 5,  digit_date = c(4,3)) %>%
    set_names(c('date', pcp_names))

  tmp <- read_weather_file(file = project_path%//%weather_file[2],
                      var = c("tmax", "tmin"), skip = 4, digit_var = 5,
                      digit_date = c(4,3))

  tmin <- select(tmp, date, starts_with("tmin_")) %>%
    set_names(c('date', tmp_names))
  tmax <- select(tmp, date, starts_with("tmax_")) %>%
    set_names(c('date', tmp_names))
  tav  <- map2_df(select(tmax, -date), select(tmin, -date), ~ (.x + .y)/2) %>%
    add_column(., date = tmin$date, .before = 1)

  return(list(pcp = pcp, tmax = tmax, tmin = tmin, tav = tav))
}

#' Read a SWAT2012 weather input file
#'
#' @param file Path string to weather input file.
#' @param var  Character vector that defines the variables
#' @param skip Integer to skip the header
#' @param digit_var Integer, number of digits per variable
#' @param digit_date Vector of length 2 Integers give digits of year and jdn

#' @importFrom dplyr %>%
#' @importFrom readr cols fwf_widths read_fwf read_lines
#'
#' @keywords internal
#'
read_weather_file <- function(file, var, skip, digit_var, digit_date) {
  n_var <-  (nchar(read_lines(file, n_max = (skip + 1))[(skip + 1)]) -
               sum(digit_date)) / digit_var

  cols <- fwf_widths(c(digit_date[1],digit_date[2], rep(digit_var,n_var)),
                     col_names = c("year", "jdn", rep(var, n_var/length(var))%_%
                                     rep(1:(n_var/length(var)), each = length(var))))

  read_fwf(file = file, col_positions = cols,
           col_types = cols(.default = "d", year = "i", jdn = "i"),
           skip = 4) %>%
    jdnyr_to_date(.)
}

#' Connect the HRUs to the read weather stations
#'
#' @param project_path Path string to SWAT project folder.
#' @param variables List of tibbles with read weather station data
#' @param hru_attributes tibble for attributes of the HRUs
#'
#' @importFrom dplyr full_join mutate select %>%
#' @importFrom purrr map map_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
connect_unit_weather <- function(project_path, hru_attributes, variables, version) {
  if(version == 'plus') {
    wth_con <- connect_weather_plus(project_path, hru_attributes)
  } else if(version == '2012') {
    wth_con <- connect_weather_2012(project_path, hru_attributes, variables)
  }
  return(wth_con)
}

#' Connect the HRUs to the read weather stations for SWAT+
#'
#' @param project_path Path string to SWAT project folder.
#' @param variables List of tibbles with read weather station data
#' @param hru_attributes tibble for attributes of the HRUs
#'
#' @importFrom dplyr full_join mutate select %>%
#' @importFrom purrr map map_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
connect_weather_plus <- function(project_path, hru_attributes) {
  weather_sta <- read_table(project_path%//%'weather-sta.cli',
                            col_types = cols(.default = col_character()),
                            skip = 1)

  hru_con <- read_con_file(project_path%//%'hru.con') %>%
    select(id, name, wst) %>%
    left_join(., weather_sta, by = c('wst' = 'name')) %>%
    select(id, name, pcp, tmp) %>%
    mutate(pcp = str_remove(pcp, '.pcp'),
           tmp = str_remove(tmp, '.tmp'),
           tmax = tmp,
           tmin = tmp,
           tav  = tmp) %>%
    select(-tmp) %>%
    rename(hru = id, hru_name = name)

  hru_con <- hru_attributes %>%
    select(rtu, rtu_name, hru) %>%
    left_join(., hru_con, by = 'hru')

  return(hru_con)
}

#' Read a SWAT+ connect file
#'
#' @param con_path Path string to SWAT+ connect file.
#'
#' @importFrom dplyr bind_rows mutate_if %>%
#' @importFrom purrr map set_names
#' @importFrom readr read_lines
#'
#' @keywords internal
#'
read_con_file <- function(con_path) {

  con <- read_line_file(con_path)

  out_pos <- which(con$header == 'out_tot')
  if(length(con$header) > out_pos) {
    n_rep <- (con$n_max - out_pos) / (length(con$header) - out_pos)
    head_rep <- con$header[(out_pos + 1):length(con$header)]

    con$header <- c(con$header[1:out_pos],
                    paste(rep(head_rep, n_rep), rep(1:n_rep, each = length(head_rep)), sep = '_'))
  }

  con_tbl <- con$data %>%
    map(., ~.x[1:con$n_max]) %>%
    map(., ~set_names(.x, con$header)) %>%
    bind_rows() %>%
    mutate_if(., con$col_numeric, .funs = as.numeric)

  return(con_tbl)
}

#' Connect the HRUs to the read weather stations for SWAT2012
#'
#' @param project_path Path string to SWAT project folder.
#' @param variables List of tibbles with read weather station data
#' @param hru_attributes tibble for attributes of the HRUs
#'
#' @importFrom dplyr full_join mutate select %>%
#' @importFrom purrr map map_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
connect_weather_2012 <- function(project_path, hru_attributes, variables) {
  sub_list <- list.files(path = project_path, pattern = "[:0-9:].sub")
  sub_files <- map(project_path%//%sub_list, read_lines)

  var_tbl <- sub_files %>%
    map_df(., ~ tibble(pcp  = get_value(.x[7]),
                       tmin = get_value(.x[8]),
                       tmax = get_value(.x[8]),
                       tav  = get_value(.x[8]),
#                       slr = get_value(.x[9]), # can be added in future if other weather data relevant
#                       hmd = get_value(.x[10]),
#                       wnd = get_value(.x[11])
           )) %>%
    mutate(sub = str_remove(sub_list, "0000.sub") %>% as.numeric()) %>%
    full_join(., hru_attributes, by = 'sub') %>%
    select(sub, hru, pcp, tmin, tmax, tav)

  var_names <- map(variables, ~select(.x, -date) %>% names(.))

  for(i_var in names(var_names)) {
    var_tbl[[i_var]] <- var_names[[i_var]][var_tbl[[i_var]]]
  }

  return(var_tbl)
}

#' Add additional variables that can be used to define rules for
#'
#' @param data The table that should be added as a variable. The number of columns
#'   must be 1 or the number of subbasins. the row number must be the same as the
#'   number of weather records.
#' @param name Character string to define the name of the added variable (this
#'   name must be used in the rule set)
#' @param assign_unit tibble to link spatial units to the variables in data
#' @param variables Internal variables. Not defined by user. Required number of variables.
#' @param date  Internal variable. Not defined by user. Date vector that is added.
#'
#' @importFrom dplyr bind_cols left_join mutate select %>%
#' @importFrom lubridate is.Date ymd
#' @importFrom purrr set_names
#' @importFrom tibble add_column tibble
#'
#' @return Generates a new farmr_project in the working environment (as an R6
#'   object) and saves the project the TxtInOut folder.
#'
#' @keywords internal
#'
add_variable <- function(data, name, assign_unit, overwrite, con, variables) {
  if((name %in% names(variables)) & !overwrite) {
    stop("The variable '", name, "' already exists. Set 'overwrite' to 'TRUE'",
         " to replace the variable '", name, "'.")
    }

  if(is.null(dim(data))) {
    if (length(data) != nrow(variables[[1]])) {
      stop("Length of added variable vector is different to the length of the weather data.")
    }
    variables[[name]] <- variables[[1]] %>%
      select(year, month, day, jdn) %>%
      add_column(!!name := data)

    if (!is.null(assign_unit)) {
      warning("'data' is a single vector. 'assign_unit' overwritten and 'data' assigned to all HRUs.")
    }
    con <- mutate(con, !!name := name)

  } else {
    if(is.null(assign_unit)) {
      stop("Providing a 'data' tibble requires assigning the data to units (e.g. 'sub', 'rtu', 'hru').")
    }

    if (nrow(data) != nrow(variables[[1]])) {
      stop("The number of rows in 'data' is different to the number of rows of the weather data.")
    }

    if (is.Date(data[[1]])) {
      date <- variables[[1]]$date
      if(any(date != data[[1]])) {
        stop("A date column was provided in 'data'. The dates differ from the dates of the existing variables.")
      }
      data <- data[, 2:ncol(data)]
    }

    if(any(c('date', 'year', 'month', 'day', 'jdn', 'md', 'ymd', 'hu', 'hu_fr') %in% names(data))) {
      stop("The variable names 'date', 'year', 'month', 'day', 'jdn', 'md', 'ymd'",
           ", 'hu', or 'hu_fr' are not allowed as variable names in 'data'.")
    }

    var_name <- unique(assign_unit[[2]])
    var_miss <- var_name[!(var_name %in% names(data))]
    if(length(var_miss) > 0) {
      stop(paste0("The variable", plural(length(var_miss)), ": "),  paste(var_miss, collapse = ", "),
           " are defined in 'assign_unit', but are no variable in 'data'.")
    }
    unit_cnt <- table(assign_unit[[1]])
    if(!all(unit_cnt == 1)) {
      stop("The provided values for '", names(assign_unit)[1], "' must be unique.")
    }
    unit_val <- unique(assign_unit[[1]])
    unit_con <- unique(con[[names(assign_unit)[1]]])
    unit_miss <- unit_con[!(unit_con %in% unit_val)]
    if(length(unit_miss) > 0) {
      stop(paste0("The ", names(assign_unit)[1], plural(length(unit_miss)), ": "),
           paste(unit_miss, collapse = ", "), " are missing in 'assign_unit'.")
    }

    variables[[name]] <- bind_cols(variables[[1]]['date'], data)

    assign_unit <- set_names(assign_unit, c(names(assign_unit)[1], name))
    con <- left_join(con, assign_unit, by = names(assign_unit)[1])
  }
  return(list(variables = variables, con = con))
}

#' Get numeric value from line in input file
#'
#' @param x The text string line.
#'
#' @keywords internal
#'
get_value <- function(x) {
  as.numeric(substr(x, 1,16))
}

#' Check if all weather inputs are in a daily time interval
#'
#' @param files String vector of weather input files
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @importFrom stringr str_split
#' @importFrom purrr map map_lgl
#'
#' @keywords internal
#'
check_if_daily <- function(files, project_path) {
  is_daily <- map(files, ~read_lines(project_path%//%.x , skip = 2, n_max = 1)) %>%
    map(., ~str_split(.x, "\\s+")) %>%
    map(., ~.x[[1]][nchar(.x[[1]]) > 0]) %>%
    map(., as.numeric) %>%
    map_lgl(., ~.x[2] == 0)

  if(!all(is_daily)) {
    stop("All 'pcp' and 'tmp' weather inputs must have daily time steps to be used.")
  }
}

#' Convert jdn and year columns to year, month, day, and jdn columns
#'
#' @param tbl Tibble containing the year and the jdn columns
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom lubridate month day
#' @importFrom tidyselect everything
#'
#' @keywords internal
#'
jdnyr_to_ymdjdn <- function(tbl) {
  tbl %>%
  mutate(date  = as.Date(jdn%//%year, "%j/%Y"),
         month = month(date),
         day   = day(date)) %>%
    select(-date) %>%
    select(year, month, day, jdn, everything())
}

#' Convert jdn and year columns to a date column
#'
#' @param tbl Tibble containing the year and the jdn columns
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom tidyselect everything
#'
#' @keywords internal
#'
jdnyr_to_date <- function(tbl) {
  tbl %>%
    mutate(date  = as.Date(jdn%//%year, "%j/%Y")) %>%
    select(-year, -jdn) %>%
    select(date, everything())
}

#' Retrieve the weather station names from SWAT2012 weather input file
#'
#' @param file_path String path to weather file
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @importFrom stringr str_remove str_split
#'
#' @keywords internal
#'
get_station_names <- function(file_path) {
  read_lines(file_path, n_max = 1) %>%
    str_remove(., 'Station ') %>%
    str_split(., ',', simplify = TRUE) %>%
    trimws() %>%
    .[nchar(.)>0]
}
