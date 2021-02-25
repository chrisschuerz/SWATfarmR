#' Read the weather data
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#' @param version String that indicates what SWAT version the project is.
#' @param t0 date time object storing initial start of initialization
#'
#' @keywords internal
#'
#'
read_hru_attributes <- function(project_path, version, t0) {
  if(version == 'plus') {
    hru_attr <- read_attributes_plus(project_path)
  } else if(version == '2012') {
    hru_attr <- read_attributes_2012(project_path, t0)
  }
  return(hru_attr)
}

#' Extract HRU attributes for SWAT+ projects
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom dplyr filter left_join rename select %>%
#' @importFrom readr cols col_character col_double read_table
#'
#' @keywords internal
#'
read_attributes_plus <- function(project_path) {
  rtu_def <- read_rtu_def(project_path)

  hru_data <- read_table(project_path%//%'hru-data.hru', skip = 1,
                         col_types = cols(id = col_double(),
                                          .default = col_character())) %>%
    rename(hru = id, hru_name = name)

  soils <- read_table(project_path%//%'soils.sol', skip = 1,
                      col_types = cols(
                        .default = col_double(),
                        name = col_character(),
                        hyd_grp = col_character(),
                        texture = col_character()
                      )) %>%
    select(name, hyd_grp) %>%
    filter(nchar(name)>0)

  topo <- read_table(project_path%//%'topography.hyd', skip = 1,
                     col_types = cols(
                       name = col_character(),
                       .default = col_double()
                     )) %>%
    select(name, slp, slp_len)

  hru_attr <- rtu_def %>%
    left_join(., hru_data, by = c('hru', 'hru_name')) %>%
    left_join(., soils, by = c('soil' = 'name')) %>%
    left_join(., topo, by = c('topo' = 'name')) %>%
    select(rtu, rtu_name, hru, hru_name, lu_mgt, soil, slp, slp_len, hyd_grp)

  return(hru_attr)
}

#' Routine to read SWAT+ input files line wise when necessary due to its structure
#'
#' @param file_path String. Path to the file
#'
#' @importFrom dplyr bind_rows filter select %>%
#' @importFrom purrr map map_dbl
#' @importFrom readr read_lines
#' @importFrom stringr str_split
#'
#' @keywords internal
#'
read_line_file <- function(file_path) {
  line_file <- read_lines(file_path, skip = 1)

  file_data <- line_file[2:length(line_file)] %>%
    str_split(. , "\\s+") %>%
    map(., ~.x[nchar(.x) > 0])

  n_max  <- max(map_dbl(file_data, length))
  id_max <- which.max(map_dbl(file_data, length))
  col_numeric <- !is.na(suppressWarnings((as.numeric(file_data[[n_max]]))))

  file_head <-line_file[1] %>%
    str_split(. , "\\s+", simplify = TRUE) %>%
    .[nchar(.) > 0]

  return(list(data = file_data, header = file_head,
              n_max = n_max, id_max = id_max, col_numeric = col_numeric))
}

#' Routine to read the SWAT+ routing unit definition
#'
#' @param project_path String. Path to the TxtInOut folder of the SWAT project
#'
#' @importFrom dplyr bind_rows filter left_join select %>%
#' @importFrom purrr map map2 reduce set_names
#' @importFrom readr cols col_character col_double read_table
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
read_rtu_def <- function(project_path) {

  rtu_def <- read_line_file(project_path%//%'rout_unit.def')
  rtu_ele <- read_table(project_path%//%'rout_unit.ele', skip = 1,
                        col_types = cols(name = col_character(),
                                         obj_typ = col_character(),
                                         .default = col_double())) %>%
    filter(obj_typ == 'hru') %>%
    select(id, obj_id, name) %>%
    set_names(c('ele_id', 'hru', 'hru_name'))

  rtu_id <- rtu_def$data %>%
    map(., ~.x[1:2]) %>%
    map(., ~set_names(.x, c('rtu', 'rtu_name'))) %>%
    bind_rows() %>%
    mutate(rtu = as.numeric(rtu))

  rtu_tbl <- rtu_def$data %>%
    map(., ~.x[4:rtu_def$n_max]) %>%
    map(., as.numeric) %>%
    map(., c_idx) %>%
    set_names(rtu_id$rtu_name) %>%
    map2(., names(.), ~tibble(rtu_name = .y, ele_id = .x)) %>%
    reduce(., bind_rows) %>%
    left_join(., rtu_id, by = 'rtu_name') %>%
    select(rtu, rtu_name, ele_id) %>%
    left_join(., rtu_ele, by = 'ele_id') %>%
    select(-ele_id)

  return(rtu_tbl)
}

#' Concatinate ranges of idx and idx to a vector of idx
#'
#' @param x Vector providing idx values and ranges
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map reduce
#'
#' @keywords internal
#'
c_idx <- function(x) {
  is_rng <- which(x < 0)
  map(is_rng, ~ (x[.x-1] + 1):(-x[.x])) %>%
    reduce(., c) %>%
    c(.,x[-is_rng]) %>%
    sort(.)
}

#' Read HRU attributes from the .hru and .sol files in SWAT2012 projects
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
read_attributes_2012 <- function(project_path, t0) {

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
      bind_cols(extract_hru_2012(hru_files[[i]]), extract_sol_2012(sol_files[[i]]))
    display_progress_pct(i, n_hru, t0)
  }
  attr_tbl <- map_df(attr_list, ~.x) %>%
    add_column(., file = file_list, .before = 1)
  return(attr_tbl)
}

#' Extract HRU attributes from the SWAT2012 .hru files
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
extract_hru_2012 <- function(str_lines) {
  hru_attr <- str_split(str_lines[1], "\\ |\\:|\\: ") %>%
    unlist() %>%
    .[nchar(.) > 0] %>%
    list(sub  = as.numeric(.[grep("Subbasin", .)+1]),
         hru  = as.numeric(.[grep("HRU", .)[1]+1]),
         luse = .[grep("Luse", .)+1],
         soil = .[grep("Soil", .)+1],
         slp = .[grep("Slope", .)+1]) %>%
    .[2:length(.)]
  hru_attr$slp_val <- as.numeric(str_sub(str_lines[4], 1, 16))
  hru_attr$slp_len <- as.numeric(str_sub(str_lines[3], 1, 16))
  return(as_tibble(hru_attr))
}

#' Extract HRU attributes from the SWAT2012 .sol files
#'
#' @param str_lines read lines of a .sol file
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
extract_sol_2012 <- function(str_lines) {
  tibble(hyd_grp = str_split(str_lines[3], "\\:", simplify = TRUE)[2] %>%
           trimws(.),
         root_dep = str_split(str_lines[4], "\\:", simplify = TRUE)[2] %>%
           as.numeric(.))
}