#' Read the management schedule table from a csv file
#'
#' @param file Text string path to the csv file
#' @param version String that indicates what SWAT version the project is.
#' @param version String that indicates the SWAT version.
#' @param version String that indicates what SWAT version the project is.
#'
#' @keywords internal
#'
#'
read_mgt_lkp <- function(file, version, project_path, hru_attribute) {
  if(version == 'plus') {
    mgt_text <- read_mgt_table_plus(file)
    lookup  <- read_lookup_plus(project_path)
    check_mgt_table_plus(mgt_text, lookup, hru_attribute)
    mgt_code <- translate_mgt_table_plus(mgt_text, lookup)
    mgt_text <- NULL
  } else if(version == '2012') {
    mgt_text <- read_mgt_table_2012(file)
    lookup  <- read_lookup_2012(project_path)
    check_mgt_table_2012(mgt_text, lookup, hru_attribute)
    mgt_code <- translate_mgt_table_2012(mgt_text, lookup)
  }
  return(list(mgt_text = mgt_text, mgt_code = mgt_code, lookup = lookup))
}

#' Read the management schedule table from a csv file for SWAT+
#'
#' @param file Text string path to the csv file
#'
#' @importFrom dplyr select %>%
#' @importFrom readr cols read_csv
#'
#' @keywords internal
#'
read_mgt_table_plus <- function(file) {
  tbl <- read_csv(file, col_types = cols(weight = 'd',
                                         op_data3 = 'd',
                                         .default = 'c'), lazy = FALSE) %>%
    select(land_use, management, weight, rules_static, rules_dynamic, operation, 'op_data'%&%1:3)
  return(tbl)
}

#' Read the management schedule table from a csv file for SWAT2012
#'
#' @param file Text string path to the csv file
#'
#' @importFrom dplyr select %>%
#' @importFrom readr cols read_csv
#'
#' @keywords internal
#'
read_mgt_table_2012 <- function(file) {
  tbl <- read_csv(file, col_types = cols(management = "c",
                                         weight = "d",
                                         land_use = "c",
                                         rules_static = "c",
                                         rules_dynamic = "c",
                                         operation = "c",
                                         mgt1 = "c",
                                         .default = "d"), lazy = FALSE) %>%
    select(land_use, management, weight, rules_static, rules_dynamic, operation, 'mgt'%&%1:9)
  return(tbl)
}

#' Read the lookup tables for plant, fertilizer, and tillage codes from the SWAT
#' project
#'
#' @param project_path Text string path SWAT TxtInOut folder
#'
#' @importFrom dplyr select %>%
#' @importFrom purrr map map_dbl map_df set_names
#' @importFrom readr cols read_lines read_table
#' @importFrom stringr str_split
#' @importFrom tibble tribble
#' @importFrom tidyselect starts_with
#'
#' @keywords internal
#'
read_lookup_plus <- function(project_path) {
  lookup  <- list(management = tribble(
                                       ~value,    ~label,
                                        "skip",   "end_year",
                                        "plnt",   "plant",
                                        "irrm",   "irrigation",
                                        "fert",   "fertilizer",
                                        "pest",   "pesticide",
                                        "hvkl",   "harvest_kill",
                                        "till",   "tillage",
                                        "harv",   "harvest_only",
                                        "kill",   "kill_only",
                                        "graz",   "grazing",
                                        "swep",   "street_sweeping",
                                        "burn",   "burn",
                                        "skip",   "skip",
                                        "inip",   "initial_plant"))
  lookup$fertilizer <- read_table_linewise(file = project_path%//%"fertilizer.frt",
                                  col_names = c('name', 'fminn', 'fminp', 'forgn',
                                                'forgp', 'fnh3n', 'pathogens', 'description'),
                                  col_types = c('c', rep('d', 5), 'c', 'c'), n_skip = 2)

  lookup$tillage <- read_table_linewise(file = project_path%//%"tillage.til",
                                        col_names = c('name', 'mix_eff', 'mix_dp', 'rough',
                                                      'ridge_ht', 'ridge_sp', 'description'),
                                        col_types = c('c', rep('d', 5), 'c'), n_skip = 2)

  lookup$plt_comm <- read_plt_comm(comm_file = project_path%//%"plant.ini")

  lookup$plant <- read_table(file = project_path%//%"plants.plt",
                             col_names = TRUE, col_types = cols(), skip = 1) %>%
    rename(t_base = tmp_base)

  return(lookup)
}

#' Read the SWAT+ plant.ini file
#'
#' @param comm_file Text string path to the plant.ini file
#'
#' @importFrom dplyr filter mutate mutate_at %>%
#' @importFrom purrr map map_chr map_dbl map2 map2_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_replace_all str_split str_trim
#' @importFrom tibble as_tibble
#'
#' @keywords internal
#'
read_plt_comm <- function(comm_file) {
  tbl <- read_lines(file = comm_file, skip = 1, lazy = FALSE) %>%
    str_replace_all(., '\t', ' ')

  comm_head <- tbl[1] %>%
    str_trim(.) %>%
    str_split(., '[:space:]+', simplify = TRUE)
  tbl <- tbl[2:length(tbl)]

  plt_cnt <- str_split(tbl, '[:space:]+') %>%
    map_dbl(., ~ as_num(.x[2]))

  comm_pos <-  which(!is.na(plt_cnt))

  comm_name <- str_split(tbl[comm_pos], '[:space:]+') %>%
    map_chr(., ~ .x[1])

  comm_plt <- map2(comm_pos, plt_cnt[comm_pos], ~ tbl[(.x + 1): (.x+.y)]) %>%
    map(., ~ str_trim(.x, side = 'both')) %>%
    map(., ~ str_split(.x, '[:space:]+', simplify = TRUE)) %>%
    map(., ~ as_tibble(.x, .name_repair = 'minimal')) %>%
    map(., ~ set_names(.x, comm_head[4:length(comm_head)])) %>%
    map2_df(., comm_name, ~ mutate(.x, plt_comm = .y, .before = 1)) %>%
    filter(lc_status == 'y') %>%
    mutate_at(., 4:ncol(.), as.numeric)

  return(comm_plt)
}

#' Convert string to numeric without warning
#'
#' @param x Text string
#'
#' @keywords internal
#'
as_num <- function(x) {
    suppressWarnings(as.numeric(x))
}

#' Read the lookup tables for plant, fertilizer, and tillage codes from the SWAT
#' project
#'
#' @param project_path Text string path SWAT TxtInOut folder
#'
#' @importFrom dplyr select %>%
#' @importFrom purrr map map_dbl map_df set_names
#' @importFrom readr cols read_lines read_table
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyselect starts_with
#'
#' @keywords internal
#'
read_lookup_2012 <- function(project_path) {
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
                                                "initial_plant")))

  lookup$fertilizer <- read_table(file = project_path%//%"fert.dat",
                                  col_names = FALSE, col_types = cols()) %>%
    .[,1:2] %>%
    set_names(c("value", "label"))

  lookup$tillage <- read_table(file = project_path%//%"till.dat",
                               col_names = FALSE, col_types = cols()) %>%
    .[,1:2] %>%
    set_names(c("value", "label"))

  lookup$plant <- read_lines(project_path%//%"plant.dat", lazy = FALSE) %>%
    .[nchar(.) <=14] %>%
    str_split(string = ., pattern = "\\s+") %>%
    map(., ~.x[nchar(.x) > 0]) %>%
    map_df(., ~tibble(value = .x[1], label = .x[2]))

  lookup$plant$t_base <- read_lines(project_path%//%"plant.dat", lazy = FALSE) %>%
    .[which(nchar(.) <=14) + 2]%>%
    str_split(string = ., pattern = "\\s+") %>%
    map(., ~.x[nchar(.x) > 0]) %>%
    map_dbl(., ~as.numeric(.x[2]))

  return(lookup)
}

#' Check the read SWAT+ management schedule tables
#'
#' @param mgt_tbl The read management table in tibble format
#' @param lookup List with lookup tables for plant, tillage, and fertilizer
#' @param hru_attribute Tibble providing attributes for the HRUs
#'
#' @importFrom dplyr filter %>%
#'
#' @keywords internal
#'
check_mgt_table_plus <- function(mgt_tbl, lookup, hru_attribute) {
  plant_mgt <-  filter(mgt_tbl, operation == "plant") %>% .$op_data1 %>% unique(.)
  plant_lkp <- unique(lookup$plant$name)
  plant_miss <- plant_mgt[!(plant_mgt %in% plant_lkp)]
  if(length(plant_miss) > 0) {
    stop("The following plants are not defined in the SWAT data base" %&%
           ", but were found in the management table:\n  " %&%
           paste(plant_miss, collapse = ", ") %&%
           "\n  Please check the inputs in the management table!")
  }
  fert_mgt <-  filter(mgt_tbl, operation == "fertilizer") %>% .$op_data1 %>% unique(.)
  fert_lkp <- unique(lookup$fertilizer$name)
  fert_miss <- fert_mgt[!(fert_mgt %in% fert_lkp)]
  if(length(fert_miss) > 0) {
    stop("The following fertilizers are not defined in the SWAT data base" %&%
           ", but were found in the management table:\n  " %&%
           paste(fert_miss, collapse = ", ") %&%
           "\n  Please check the inputs in the management table!")
  }
  no_fert_amount <- which(mgt_tbl$operation == 'fertilizer' & is.na(mgt_tbl$op_data3))
  if(length(no_fert_amount) > 0) {
    stop("In the following lines fertilizer operations are given without a " %&%
         "fertilizer amount ('op_data3'):\n  " %&%
           paste(no_fert_amount, collapse = ", ") %&%
           "\n  Please check the inputs in the management table!")
  }
  till_mgt <-  filter(mgt_tbl, operation == "tillage") %>% .$op_data1 %>% unique(.)
  till_lkp <- unique(lookup$tillage$name)
  till_miss <- till_mgt[!(till_mgt %in% till_lkp)]
  if(length(till_miss) > 0) {
    stop("The following tillage types are not defined in the SWAT data base" %&%
           ", but were found in the management table:\n  " %&%
           paste(fert_miss, collapse = ", ") %&%
           "\n  Please check the inputs in the management table!")
  }
  luse_hru <- unique(hru_attribute$lu_mgt)
  luse_miss <- luse_hru[!(luse_hru %in% unique(mgt_tbl$land_use))]
  if(length(luse_miss) > 0) {
    warning("No management schedules were found for the following SWAT land uses: \n  " %&%
              paste(luse_miss, collapse = ", ") %&%
              "\n  If managements should be written for any of these land uses\n"%&%
              "  please add them to the management table and read the table again.")
  }
}

#' Check the read SWAT2012 management schedule tables
#'
#' @param mgt_tbl The read management table in tibble format
#' @param lookup List with lookup tables for plant, tillage, and fertilizer
#' @param hru_attribute Tibble providing attributes for the HRUs
#'
#' @importFrom dplyr filter %>%
#'
#' @keywords internal
#'
check_mgt_table_2012 <- function(mgt_tbl, lookup, hru_attribute) {
  plant_mgt <-  filter(mgt_tbl, operation == "plant") %>% .$mgt1 %>% unique(.)
  plant_lkp <- unique(lookup$plant$label)
  plant_miss <- plant_mgt[!(plant_mgt %in% plant_lkp)]
  if(length(plant_miss) > 0) {
    stop("The following plants are not defined in the SWAT data base" %&%
           ", but were found in the management table:\n  " %&%
           paste(plant_miss, collapse = ", ") %&%
           "\n  Please check the inputs in the management table!")
  }
  fert_mgt <-  filter(mgt_tbl, operation == "fertilizer") %>% .$mgt1 %>% unique(.)
  fert_lkp <- unique(lookup$fertilizer$label)
  fert_miss <- fert_mgt[!(fert_mgt %in% fert_lkp)]
  if(length(fert_miss) > 0) {
    stop("The following fertilizers are not defined in the SWAT data base" %&%
           ", but were found in the management table:\n  " %&%
           paste(fert_miss, collapse = ", ") %&%
           "\n  Please check the inputs in the management table!")
  }
  till_mgt <-  filter(mgt_tbl, operation == "tillage") %>% .$mgt1 %>% unique(.)
  till_lkp <- unique(lookup$tillage$label)
  till_miss <- till_mgt[!(till_mgt %in% till_lkp)]
  if(length(till_miss) > 0) {
    stop("The following tillage types are not defined in the SWAT data base" %&%
           ", but were found in the management table:\n  " %&%
           paste(fert_miss, collapse = ", ") %&%
           "\n  Please check the inputs in the management table!")
  }
  luse_hru <- unique(hru_attribute$luse)
  luse_miss <- luse_hru[!(luse_hru %in% unique(mgt_tbl$land_use))]
  if(length(luse_miss) > 0) {
    warning("No management schedules were found for the following SWAT land uses: \n  " %&%
              paste(luse_miss, collapse = ", ") %&%
              "\n  If managements should be written for any of these land uses\n"%&%
              "  please add them to the management table and read the table again.")
  }
}

#' Translate the operation and management labels into SWAT+ labels
#'
#' @param mgt_tbl Loaded tibble with the management operation schedules
#' @param lookup  List of lookup tables
#'
#' @importFrom dplyr %>%
#'
#' @keywords internal
#'
translate_mgt_table_plus <- function(mgt_tbl, lookup) {
  for (i_op in 1:nrow(mgt_tbl)) {
    if(mgt_tbl$operation[i_op] %in% lookup$management$label) {
      mgt_tbl$operation[i_op] <-
        lookup$management$value[lookup$management$label ==  mgt_tbl$operation[i_op]]
    } else if(!(mgt_tbl$operation[i_op] %in% lookup$management$label) &
              !(mgt_tbl$operation[i_op] %in% lookup$management$value))
    stop("Error in line ", i_op, " of the management table. Unknown 'operation'.")
  }
  return(mgt_tbl)
}

#' Translate the operation and management labels into SWAT codes
#'
#' @param mgt_tbl Loaded tibble with the management operation schedules
#' @param lookup  List of lookup tables
#'
#' @importFrom dplyr left_join mutate select %>%
#'
#' @keywords internal
#'
translate_mgt_table_2012 <- function(mgt_tbl, lookup) {
  mgt_tbl %>%
    left_join(., lookup$management, by = c("operation" = "label")) %>%
    mutate(operation = value) %>%
    select(-value) %>%
    left_join(., lookup$plant, by = c("mgt1" = "label")) %>%
    mutate(mgt1 = ifelse(operation %in% c(1,99), value, mgt1)) %>%
    select(-value, -t_base) %>%
    left_join(., lookup$fertilizer, by = c("mgt1" = "label")) %>%
    mutate(mgt1 = ifelse(operation == 3, value, mgt1)) %>%
    select(-value) %>%
    left_join(., lookup$tillage, by = c("mgt1" = "label")) %>%
    mutate(mgt1 = ifelse(operation == 6, value, mgt1)) %>%
    select(-value) %>%
    mutate(mgt1 = as.integer(mgt1))
}
