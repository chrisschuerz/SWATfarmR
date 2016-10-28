## init_crp(mgt_file, sdl_df, i_op) --------------------------------------
## Function writes initial plant cover into the management operation file.
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

## end_year(mgt_file, sdl_df, i_op, mgt_df) -----------------------------
## Function writes end of year operation into the management operation file.
end_year <- function(mgt_file, sdl_df, i_op, mgt_df) {
  sdl <- sdl_df[-c(1:4)]
  sdl[c(1:3,5:13)] <- NA
  sdl[4] <- mgt_df$OPNUM[mgt_df$OP ==  sdl[4]]
  mgt_line <- format_mgtstringout(sdl)

  return(mgt_line)
}
## plnt.crp(mgt_file, sdl_df, i_op, meta_data, mgt_df, crop_df, ---------
##          pcp_df, tmp_df, amc_df)
plnt_crp <- function(mgt_file, sdl_df, i_op, meta_data, input_lst, thrs,
                     day_rnd, day_ssp, select_type){
  op_year   <- sdl_df[2]
  plnt_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  plnt_sdl[,1:2]   <- select_date(sdl_df, i_op, op_year, prev_date, meta_data,
                                  input_lst, thrs, day_rnd, day_ssp, select_type)

  plnt_sdl$OP      <- input_lst$lookup$management$OPNUM[
                      input_lst$lookup$management$OP == sdl_df[8]]
  plnt_sdl$MGT_1   <- input_lst$lookup$crop$ICNUM[
                      input_lst$lookup$crop$CPNM == sdl_df[9]]
  plnt_sdl[,6:12]  <- sdl_df[10:16]

  if(!is.null(meta_data$CNOP)){
    plnt_sdl$MGT_9    <- meta_data$CNOP$CN[meta_data$CNOP$OP ==
                                          sdl_df[8] &
                                          meta_data$CNOP$CROP ==
                                          sdl_df[1]]
  }

  mgt_line <- format_mgtstringout(plnt_sdl)

  return(mgt_line)
}

## fert_crp(mgt_file, sdl_file, i_op, meta_data, mgt_df, fert_df, -------
##          pcp_df, tmp_df, amc_df)
fert_crp <- function(mgt_file, sdl_df, i_op, meta_data, input_lst, thrs,
                     day_rnd, day_ssp, select_type) {
  op_year   <- sdl_df[2]
  fert_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  fert_sdl[,1:2]   <- select_date(sdl_df, i_op, op_year, prev_date, meta_data,
                                  input_lst, thrs, day_rnd, day_ssp, select_type)
  fert_sdl$OP      <- input_lst$lookup$management$OPNUM[
                      input_lst$lookup$management$OP == sdl_df[8]]
  fert_sdl$MGT_1 <- input_lst$lookup$fertilizer$IFNUM[
                      input_lst$lookup$fertilizer$FERTNM == sdl_df[9]]
  fert_sdl[,6:13]  <- sdl_df[10:17]

  mgt_line <- format_mgtstringout(fert_sdl)

  return(mgt_line)
}

## hrv_kill(mgt_file, sdl_file, i_op, meta_data, mgt_df, pcp_df, --------
##          tmp_df, amc_df)
hrv_kill <- function(mgt_file, sdl_df, i_op, meta_data, input_lst, thrs,
                     day_rnd, day_ssp, select_type) {

  op_year   <- sdl_df[2]
  hvst_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  hvst_sdl[,1:2]   <- select_date(sdl_df, i_op, op_year, prev_date, meta_data,
                                  input_lst, thrs, day_rnd, day_ssp, select_type)
  hvst_sdl$OP      <- input_lst$lookup$management$OPNUM[
                      input_lst$lookup$management$OP == sdl_df[8]]
  hvst_sdl[,6:13]  <- sdl_df[10:17]

  if(!is.null(meta_data$CNOP)){
    hvst_sdl$MGT_4    <- meta_data$CNOP$CN[meta_data$CNOP$OP ==
                                            sdl_df[8] &
                                            meta_data$CNOP$CROP ==
                                            sdl_df[1]]
  }


  mgt_line <- format_mgtstringout(hvst_sdl)

  return(mgt_line)
}

## till_op (mgt_file, sdl_file, i_op, meta_data, mgt_df, till_df, -------
##          pcp_df, tmp_df, amc_df)
till_op  <- function(mgt_file, sdl_df, i_op, meta_data, input_lst, thrs,
                     day_rnd, day_ssp, select_type) {
  op_year   <- sdl_df[2]
  till_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  till_sdl[,1:2]   <- select_date(sdl_df, i_op, op_year, prev_date, meta_data,
                                  input_lst, thrs, day_rnd, day_ssp, select_type)
  till_sdl$OP      <- input_lst$lookup$management$OPNUM[
                      input_lst$lookup$management$OP == sdl_df[8]]
  till_sdl$MGT_1 <- input_lst$lookup$tillage$ITNUM[
                      input_lst$lookup$tillage$TILLNM == sdl_df[9]]
  till_sdl[,6:13]  <- sdl_df[10:17]

  if(!is.null(meta_data$CNOP)){
    till_sdl$MGT_4    <- meta_data$CNOP$CN[meta_data$CNOP$OP ==
                                             sdl_df[8] &
                                             meta_data$CNOP$CROP ==
                                             sdl_df[1]]
  }

  mgt_line <- format_mgtstringout(till_sdl)

  return(mgt_line)
}

## hrv_only(mgt_file, sdl_file, i_op, meta_data, mgt_df, pcp_df, -------
##          tmp_df, amc_df)
hrv_only <- function(mgt_file, sdl_df, i_op, meta_data, input_lst, thrs,
                     day_rnd, day_ssp, select_type) {
  op_year   <- sdl_df[2]
  hvst_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  hvst_sdl[,1:2]   <- select_date(sdl_df, i_op, op_year, prev_date, meta_data,
                                  input_lst, thrs, day_rnd, day_ssp, select_type)
  hvst_sdl$OP      <- input_lst$lookup$management$OPNUM[
                      input_lst$lookup$management$OP == sdl_df[8]]
  hvst_sdl[,6:13]  <- sdl_df[10:17]

  mgt_line <- format_mgtstringout(hvst_sdl)

  return(mgt_line)
}

## skip(mgt_file, sdl_df, i_op, mgt_df) ---------------------------------
skip <- function(mgt_file, sdl_df, i_op, mgt_df) {
  sdl <- sdl_df[-c(1:4)]
  sdl[,c(1:3,5:13)] <- NA
  sdl$OPERATION <- mgt_df$OPNUM[mgt_df$OP ==  sdl[8]]

  mgt_line <- format_mgtstringout(sdl)

  return(mgt_line)
}
