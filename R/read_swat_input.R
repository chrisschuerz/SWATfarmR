#' Read HRU attributes from the .hru and .sol files
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr map map_df
#' @importFrom readr read_lines
#' @importFrom stringr str_remove
#' @importFrom tibble add_column
#'
#' @keywords internal
#'
read_hru_attributes <- function(project_path, t0) {

  file_list <- list.files(path = project_path, pattern = "[:0-9:].mgt") %>%
    str_remove(., ".mgt")

  if(length(file_list) == 0) {
    stop("No input files found. Please check the path to the SWAT2012 project.")
  }

  hru_files <- map(project_path%//%file_list%.%"hru", read_lines)
  sol_files <- map(project_path%//%file_list%.%"sol", read_lines)
  attr_list <- list()
  n_hru <- length(file_list)
  cat("Initializing farmR:\n")
  for (i in 1:n_hru) {
    attr_list[[i]] <-
      bind_cols(extract_hru_attr(hru_files[[i]]), extract_sol_attr(sol_files[[i]]))
    display_progress_pct(i, n_hru, t0)
  }
  attr_tbl <- map_df(attr_list, ~.x) %>%
    add_column(., file = file_list, .before = 1)
  return(attr_tbl)
}

#' Extract HRU attributes from the .hru files
#'
#' @param str_lines read lines of a .hru file
#'
#' @importFrom dplyr %>%
#' @importFrom readr read_lines
#' @importFrom stringr str_split str_sub
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
extract_hru_attr <- function(str_lines) {
  hru_attr <- str_split(str_lines[1], "\\ |\\:|\\: ") %>%
    unlist() %>%
    .[nchar(.) > 0] %>%
    list(subbasin  = as.numeric(.[grep("Subbasin", .)+1]),
         hru  = as.numeric(.[grep("HRU", .)[1]+1]),
         luse = .[grep("Luse", .)+1],
         soil = .[grep("Soil", .)+1],
         slope_class = .[grep("Slope", .)+1]) %>%
    .[2:length(.)]
  hru_attr$slope <- as.numeric(str_sub(str_lines[4], 1, 16))
  hru_attr$slope_length <- as.numeric(str_sub(str_lines[3], 1, 16))
  return(as_tibble(hru_attr))
}

#' Extract HRU attributes from the .sol files
#'
#' @param str_lines read lines of a .sol file
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
extract_sol_attr <- function(str_lines) {
  tibble(soil_hydr_group = str_split(str_lines[3], "\\:", simplify = TRUE)[2] %>%
           trimws(.),
         root_depth = str_split(str_lines[4], "\\:", simplify = TRUE)[2] %>%
           as.numeric(.))
}

#' Read a weather input file
#'
#' @param file Path string to weather input file.
#' @param var  Character vector that defines the variables
#' @param skip Integer to skip the header
#' @param digit_var Integer, number of digits per variable
#' @param digit_date Vector of length 2 Integers give digits of year and jdn

#' @importFrom dplyr mutate select everything %>%
#' @importFrom readr cols fwf_widths read_fwf read_lines
#' @importFrom lubridate month day
#'
#' @keywords internal
#'
read_weather <- function(file, var, skip, digit_var, digit_date) {
  n_var <-  (nchar(read_lines(file, n_max = (skip + 1))[(skip + 1)]) -
               sum(digit_date)) / digit_var

  cols <- fwf_widths(c(digit_date[1],digit_date[2], rep(digit_var,n_var)),
                     col_names = c("year", "jdn", rep(var, n_var/length(var))%_%
                                   rep(1:(n_var/length(var)), each = length(var))))

  read_fwf(file = file, col_positions = cols,
           col_types = cols(.default = "d", year = "i", jdn = "i"),
           skip = 4) %>%
    mutate(date  = as.Date(jdn%//%year, "%j/%Y"),
           month = month(date),
           day   = day(date)) %>%
    select(-date) %>%
    select(year, month, day, jdn, everything())
}

#' Read a weather input file
#'
#' @param file Path string to weather input file.
#' @param var  Character vector that defines the variables
#' @param skip Integer to skip the header
#' @param digit_var Integer, number of digits per variable
#' @param digit_date Vector of length 2 Integers give digits of year and jdn

#' @importFrom dplyr mutate select everything %>%
#' @importFrom readr cols fwf_widths read_fwf read_lines
#' @importFrom lubridate month day
#'
#' @keywords internal
#'
read_weather_plus <- function(project_path) {
  weather_sta <- read_table(project_path%//%'weather-sta.cli', skip = 1)

  pcp_files <- unique(weather_sta$pcp)
  tmp_files <- unique(weather_sta$tmp)

  check_if_daily(pcp_files, project_path)
  check_if_daily(tmp_files, project_path)

  pcp <- map(pcp_files, ~ read_table(project_path%//%.x,
                                     col_names = c('year', 'jdn',
                                                   str_remove(.x, '.pcp')),
                                     skip = 3)) %>%
    map(., ~ mutate(.x, date = as.Date(jdn%//%year, "%j/%Y"))) %>%
    map(., ~ .x[,c(4,3)]) %>%
    reduce(., full_join, by = 'date')


  tmp <- map(tmp_files, ~ read_table(project_path%//%.x,
                                     col_names = c('year', 'jdn',
                                                   str_remove(.x, '.tmp')%_%'max',
                                                   str_remove(.x, '.tmp')%_%'min'),
                                     skip = 3)) %>%
    map(., ~ mutate(.x, date = as.Date(jdn%//%year, "%j/%Y"))) %>%
    map(., ~ .x[,c(5,3,4)]) %>%
    reduce(., full_join, by = 'date')

  tmax <-  select(tmp, date, ends_with("_max"))
  tmin <-  select(tmp, date, ends_with("_min"))
  tav <-  map2_df(select(tmin, -date), select(tmax, -date), ~ (.x + .y)/2) %>%
    add_column(., date = tmin$date, .before = 1)
}

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

#' Assign the correct weather stations to the subbasins and return updated variables
#'
#' @param project_path Path string to SWAT project folder.
#' @param variables List of tibbles with read weather station data
#'
#' @importFrom dplyr mutate %>%
#' @importFrom purrr map map_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
assign_subbasin_weather <- function(project_path, variables) {
  sub_list <- list.files(path = project_path, pattern = "[:0-9:].sub")
  sub_files <- map(project_path%//%sub_list, read_lines)

  sub_tbl <- sub_files %>%
    map_df(., ~ tibble(pcp  = get_value(.x[7]),
                       tmin = get_value(.x[8]),
                       tmax = get_value(.x[8]),
                       tav  = get_value(.x[8]),
#                       slr = get_value(.x[9]), # can be added in future if other weather data relevant
#                       hmd = get_value(.x[10]),
#                       wnd = get_value(.x[11])
           )) %>%
    mutate(subbasin = str_remove(sub_list, "0000.sub") %>% as.numeric())

  variables <- map(names(variables), ~assign_var_i(variables, sub_tbl, .x)) %>%
    set_names(names(variables))

  return(variables)
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

#' Assign the correct weather stations to the subbasins for the variable i
#'
#' @param var List of weather variables tables.
#' @param sub Table that links weather stations to subbasins
#' @param var_i String label for the variable i
#'
#' @importFrom dplyr %>%
#' @importFrom purrr set_names
#'
#' @keywords internal
#'
assign_var_i <- function(var, sub, var_i) {
  var[[var_i]] <- var[[var_i]][,c(1:4, unlist(sub[var_i]) + 4)] %>%
    set_names(c("year", "month", "day", "jdn", var_i%_%sub$subbasin))
  return(var[[var_i]])
}

#' Read SWAT mgt input files
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom purrr map set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_remove
#'
#' @keywords internal
#'
read_mgt <- function(project_path) {
  mgt_list <- list.files(path = project_path, pattern = "[:0-9:].mgt")
  mgt_files <- map(project_path%//%mgt_list, read_lines) %>%
    set_names(str_remove(mgt_list, ".mgt"))
  return(mgt_files)
}

#' Add additional variables that can be used to define rules for
#'
#' @param data The table that should be added as a variable. TH number of columns
#'   must be 1 or the number of subbasins. the row number must be the same as the
#'   number of weather records.
#' @param name Character string to define the name of the added variable (this
#'   name must be used in the rule set)
#' @param n_var Internal variable. Not defined by user. Required number of variables.
#' @param n_obs Internal variable. Not defined by user. Required number of variables.
#' @param date  Internal variable. Not defined by user. Date vector that is added.
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr map_dfc set_names
#' @importFrom tibble tibble
#'
#' @return Generates a new farmr_project in the working environment (as an R6
#'   object) and saves the project the TxtInOut folder.
#'
#' @keywords internal
#'
add_variable <- function(data, name, n_var, n_obs, date) {
  if(is.null(dim(data))) {
    if (length(data) != n_obs) {
      stop("Length of added variable vector is different to the length of the weather data.")
    }
    tbl <- map_dfc(1:n_var, ~tibble(data)) %>%
      set_names(c(paste(name, 1:n_var, sep = "_")))
    tbl <- bind_cols(date, tbl)
  } else {
    if (nrow(data) != n_obs) {
      stop("The number of rows in 'data' is different to the number of rows of the weather data.")
    }
    if (ncol(data) != n_var) {
      stop("The number of columns in 'data' is different to the number of subbasins.")
    }
    tbl <- as_tibble(data) %>%
      set_names(paste(name, 1:n_var, sep = "_"))
    tbl <- bind_cols(date, tbl)
  }
  return(tbl)
}
