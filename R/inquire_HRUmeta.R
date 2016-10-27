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
  hru_meta$CNOP <- cnop %>%
    select(., OPERATION,
           which(colnames(cnop) ==
                   hru_meta$SOIL))
  colnames(hru_meta$CNOP) <- c("OP", "CN")
  return(hru_meta)
}
