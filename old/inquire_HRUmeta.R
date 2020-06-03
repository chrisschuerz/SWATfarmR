# inquire_HRUmeta(mgmt_file, soil_file, crop_df, cnop_df) -----------------------
# Function extracts meta information, such as subbasin number, land use, or
# soil, for an HRU from the header of the management file as well as from the
# soil file and returns it as a list.
inquire_HRUmeta <- function(mgt_file, soil_file, input_lst, mgtcnop_sel) {
  cnop <- input_lst$mgt_cnop[["cnop"%_%mgtcnop_sel]]
  hru_meta <- mgt_file[1] %>%
    strsplit(., "\\ |\\:|\\: ") %>%
    unlist(.) %>%
    list(SUB  = sprintf("%03d",
                        as.numeric(.[grep("Subbasin", .)+1])),
         HRU  = as.numeric(.[grep("HRU", .)[2]+1]),
         LUSE = .[grep("Luse", .)+1])

  hru_meta$LUID <- input_lst$lookup$crop
  hru_meta$SOIL <- scan(text = soil_file[3], what = "",
                        quiet = TRUE)[4]

  if(!is.null(cnop)){
    hru_meta$CNOP <- cnop %>%
      select(., OPERATION, CROP,
             which(colnames(cnop) ==
                     hru_meta$SOIL))
    colnames(hru_meta$CNOP) <- c("OP","CROP", "CN")
  }
  return(hru_meta)
}

hru_list <- list.files(path = project_path, pattern = "[:0-9:].hru")
sol_list <- list.files(path = project_path, pattern = "[:0-9:].sol")

hru_files <- map(project_path%//%hru_list, read_lines)
sol_files <- map(project_path%//%sol_list, read_lines)
a <- map_df(hru_files, extract_hru_attr)

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
