#' Read HRU attributes from the .hru and .sol files
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr map map_df
#'
read_hru_attributes <- function(project_path) {
  hru_list <- list.files(path = project_path, pattern = "[:0-9:].hru")
  sol_list <- list.files(path = project_path, pattern = "[:0-9:].sol")

  hru_files <- map(project_path%//%hru_list, read_lines)
  sol_files <- map(project_path%//%sol_list, read_lines)

  hru_attr <- map_df(hru_files, extract_hru_attr) %>%
    bind_cols(., map_df(sol_files, extract_sol_attr))

}

#' Extract HRU attributes from the .hru files
#'
#' @param str_lines read lines of a .hru file
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_split str_sub
#' @importFrom tibble as_tibble
#'
extract_hru_attr <- function(str_lines) {
  hru_attr <- str_split(str_lines[1], "\\ |\\:|\\: ") %>%
    unlist() %>%
    .[nchar(.) > 0] %>%
    list(subbasin  = as.numeric(.[grep("Subbasin", .)+1]),
         hru  = as.numeric(.[grep("HRU", .)[2]+1]),
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

#' @importFrom readr cols fwf_widths read_fwf
#' @importFrom dplyr mutate select everything
#' @importFrom lubridate month day
#' @importFrom magrittr %>%
#'
read_weather <- function(file, var, skip, digit_var, digit_date) {
  n_var <-  (nchar(readLines(file, n = (skip + 1))[(skip + 1)]) -
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
    if (ncol(data) != n_obs) {
      stop("The number of columns in 'data' is different to the number of subbasins.")
    }
    tbl <- tibble(data) %>%
      set_names(c(paste(name, 1:n_var, sep = "_")))
    tbl <- bind_cols(date, tbl)
  }
  return(tbl)
}

read_management <- function(file) {
  tbl <- read_csv(file, col_types = cols(management = "c",
                                         weight = "d",
                                         land_use = "c",
                                         crop = "c",
                                         rules = "c",
                                         operation = "c",
                                         mgt1 = "c",
                                        .default = "d")) %>%
    select(land_use, management, weight, crop, rules, operation, starts_with("mgt"))
  return(tbl)
}

read_lookup <- function(project_path) {
  lookup  <- list(management = tibble(value = c(seq(0,17),99),
                                      label = c("end_year",
                                                "plant",
                                                "irrigation",
                                                "fertilizer",
                                                "pesticide",
                                                "harvest_kill",
                                                "tillage",
                                                "harvest_only",
                                                "kill_only",
                                                "grazing",
                                                "auto_irrigation",
                                                "auto_fertilization",
                                                "street_sweeping",
                                                "release_impound",
                                                "cont_fert",
                                                "cont_pest",
                                                "burn",
                                                "skip",
                                                "initial_crop")))

  lookup$fertilizer <- read_table(file = project_path%//%"fert.dat", col_names = FALSE) %>%
    .[,1:2] %>%
    set_names(c("value", "label"))

  lookup$tillage <- read_table(file = project_path%//%"till.dat", col_names = FALSE) %>%
    .[,1:2] %>%
    set_names(c("value", "label"))

  lookup$plant <- read_lines(project_path%//%"plant.dat") %>%
    .[nchar(.) <=14] %>%
    str_split(string = ., pattern = "\\s+") %>%
    map_df(., ~tibble(value = .x[2], label = .x[3]))
  return(lookup)
}

read_cnop <- function(file, ) {

}

check_mgt_table <- function(mgt_tbl, lookup, hru_attribute) {

}

translate_mgt_table <- function(mgt_tbl, lookup) {
  mgt_tbl %>%
    left_join(., lookup$management, by = c("operation" = "label")) %>%
    mutate(operation = value) %>%
    select(-value) %>%
    left_join(., lookup$plant, by = c("mgt1" = "label")) %>%
    mutate(mgt1 = ifelse(operation %in% c(1,99), value, mgt1)) %>%
    select(-value) %>%
    left_join(., lookup$fertilizer, by = c("mgt1" = "label")) %>%
    mutate(mgt1 = ifelse(operation == 3, value, mgt1)) %>%
    select(-value) %>%
    left_join(., lookup$tillage, by = c("mgt1" = "label")) %>%
    mutate(mgt1 = ifelse(operation == 6, value, mgt1)) %>%
    select(-value) %>%
    mutate(mgt1 = as.integer(mgt1))
}
