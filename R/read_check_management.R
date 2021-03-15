#' Read the management schedule table from a csv file
#'
#' @param file Text string path to the csv file
#' @param version String that indicates what SWAT version the project is.
#'
#' @keywords internal
#'
#'
read_mgt_table <- function(file, version) {
  if(version == 'plus') {
    mgt_dat <- read_mgt_table_plus(file)
  } else if(version == '2012') {
    mgt_dat <- read_mgt_table_2012(file)
  }
  return(mgt_dat)
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
                                         .default = 'c')) %>%
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
                                         .default = "d")) %>%
    select(land_use, management, weight, rules_static, rules_dynamic, operation, 'mgt'%&%1:9)
  return(tbl)
}

#' Read the lookup tables for plant, fertilizer, and tillage codes from the SWAT
#' project
#'
#' @param project_path Text string path SWAT TxtInOut folder
#' @param version String that indicates what SWAT version the project is.
#'
#' @keywords internal
#'
#'
read_lookup <- function(project_path, version) {
  if(version == 'plus') {
    mgt_dat <- read_lookup_plus(project_path)
  } else if(version == '2012') {
    mgt_dat <- read_lookup_2012(project_path)
  }
  return(mgt_dat)
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
read_lookup_plus <- function(project_path) {
  lookup  <- list(management = tribble(
                                       ~value,    ~label,
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
                                        "pini",   "plant_ini"))

  lookup$fertilizer <- read_table(file = project_path%//%"fertilizer.frt",
                                  col_names = TRUE, col_types = cols(), skip = 1)

  lookup$tillage <- read_table(file = project_path%//%"tillage.til",
                               col_names = TRUE, col_types = cols(), skip = 1)

  lookup$plant <- read_table(file = project_path%//%"plants.plt",
                             col_names = TRUE, col_types = cols(), skip = 1) %>%
    rename(t_base = tmp_base)

  return(lookup)
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
                                                "initial_crop")))

  lookup$fertilizer <- read_table(file = project_path%//%"fert.dat",
                                  col_names = FALSE, col_types = cols()) %>%
    .[,1:2] %>%
    set_names(c("value", "label"))

  lookup$tillage <- read_table(file = project_path%//%"till.dat",
                               col_names = FALSE, col_types = cols()) %>%
    .[,1:2] %>%
    set_names(c("value", "label"))

  lookup$plant <- read_lines(project_path%//%"plant.dat") %>%
    .[nchar(.) <=14] %>%
    str_split(string = ., pattern = "\\s+") %>%
    map(., ~.x[nchar(.x) > 0]) %>%
    map_df(., ~tibble(value = .x[1], label = .x[2]))

  lookup$plant$t_base <- read_lines(project_path%//%"plant.dat") %>%
    .[which(nchar(.) <=14) + 2]%>%
    str_split(string = ., pattern = "\\s+") %>%
    map(., ~.x[nchar(.x) > 0]) %>%
    map_dbl(., ~as.numeric(.x[2]))

  return(lookup)
}

check_mgt_table <- function(mgt_tbl, lookup, hru_attribute) {
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

#' Translate the operation and management labels into SWAT codes
#'
#' @param mgt_tbl Loaded tibble with the management operation schedules
#' @param lookup  List of lookup tables
#'
#' @importFrom dplyr left_join mutate select %>%
#'
#' @keywords internal
#'
translate_mgt_table <- function(mgt_tbl, lookup) {
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
