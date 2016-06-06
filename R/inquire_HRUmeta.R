# inquire_HRUmeta(mgmt_file, soil_file, crop_df, cnop_df) -----------------------
# Function extracts meta information, such as subbasin number, land use, or
# soil, for an HRU from the header of the management file as well as from the
# soil file and returns it as a list.
inquire_HRUmeta <- function(mgt_file, soil_file, crop_df, cnop_df) {
  hru_meta <- mgt_file[1] %>%
    strsplit(., "\\ |\\:|\\: ") %>%
    unlist(.) %>%
    list(SUB  = sprintf("%03d",
                        as.numeric(.[grep("Subbasin", .)+1])),
         HRU  = as.numeric(.[grep("HRU", .)[2]+1]),
         LUSE = .[grep("Luse", .)+1])

  hru_meta$LUID <- which(crop_df$CPNM == hru_meta$LUSE)
  hru_meta$SOIL <- scan(text = soil_file[3], what = "",
                        quiet = TRUE)[4]
  hru_meta$CNOP <- cnop_df %>%
    filter(., CROP == hru_meta$LUSE) %>%
    select(., OPERATION,
           which(colnames(cnop_df) ==
                   hru_meta$SOIL))
  colnames(hru_meta$CNOP) <- c("OP", "CN")
  return(hru_meta)
}
