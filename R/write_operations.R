
write_operation <- function(path, mgt_raw, schedule, write_all) {
  hru_files <- names(schedule)
  for (i_hru in 1:length(schedule)) {
    hru_file_i <- hru_files[i_hru]
    mgt_raw_i <- mgt_raw[[hru_file_i]]
    schedule_i <- schedule[[hru_file_i]]

    if(write_all){

    }
  }
}

init_crp <- function(mgt_file, sdl_df, i_op, crop_lkp) {
  mgt_file[4] <- paste(sprintf("%16i", 1),
                       "   | IGRO: Land cover status: 0-none growing; 1-growing")
  crop_id   <- which(crop_lkp$CPNM == sdl_df[i_op,]$MGT_1)
  mgt_file[5] <- paste(sprintf("%16i", crop_id),
                       "   | PLANT_ID: Land cover ID number (IGRO = 1)")
  mgt_file[6] <- paste(sprintf("%16.2f", sdl_df[i_op,]$MGT_2),
                       "   | LAI_INIT: Initial leaf are index (IGRO = 1)")
  mgt_file[7] <- paste(sprintf("%16.2f", sdl_df[i_op,]$MGT_3),
                       "   | BIO_INIT: Initial biomass (kg/ha) (IGRO = 1)")
  mgt_file[8] <- paste(sprintf("%16.2f", sdl_df[i_op,]$MGT_4),
                       "   | PHU_PLT: Number of heat units to bring plant to maturity (IGRO = 1)")
  return(mgt_file)
}
