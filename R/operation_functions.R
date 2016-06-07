## init.crp(mgt_file, sdl_df, i_op) --------------------------------------
## Function writes initial plant cover into the management operation file.
init_crp <- function(mgt_file, sdl_df, i_op, crop_lkp) {
  mgt_file[4] <- paste(sprintf("%16i", 1),
                        "   | IGRO: Land cover status: 0-none growing; 1-growing")
  crop_id   <- which(crop_lkp$CPNM == sdl_df[i_op,]$OP_TYPE)
  mgt_file[5] <- paste(sprintf("%16i", crop_id),
                        "   | PLANT_ID: Land cover ID number (IGRO = 1)")
  mgt_file[6] <- paste(sprintf("%16.2f", sdl_df[i_op,]$OP_VAL_1),
                        "   | LAI_INIT: Initial leaf are index (IGRO = 1)")
  mgt_file[7] <- paste(sprintf("%16.2f", sdl_df[i_op,]$OP_VAL_2),
                        "   | BIO_INIT: Initial biomass (kg/ha) (IGRO = 1)")
  mgt_file[8] <- paste(sprintf("%16.2f", sdl_df[i_op,]$OP_VAL_3),
                        "   | PHU_PLT: Number of heat units to bring plant to maturity (IGRO = 1)")
  return(mgt_file)
}

## end.year(mgt_file, sdl_df, i_op, mgt_df) -----------------------------
## Function writes end of year operation into the management operation file.
end_year <- function(mgt_file, sdl_df, i_op, mgt_df) {
  sdl <- sdl_df[i_op, -c(1,2)]
  sdl$OPERATION <- mgt_df$OPNUM[mgt_df$OP ==  sdl$OPERATION]
  sdl <- format_mgtstringout(sdl)
  mgt_file <- c(mgt_i, sdl)
}
## plnt.crp(mgt_file, sdl_df, i_op, meta_data, mgt_df, crop_df, ---------
##          pcp_df, tmp_df, amc_df)
plnt_crp <- function(mgt_file, sdl_df, i_op, meta_data, input_lst){
                     # mgt_df, crop_df, pcp_df, tmp_df, amc_df) {
  op_year   <- sdl_df$YEAR[i_op]
  plnt_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  plnt_sdl[,1:2]   <- select_OPdate(sdl_df, i_op, op_year, prev_date, meta_data,
                                    input_lst,
                                    # tmp_df, pcp_df, amc_df,
                                        mon_thrs = 5, thrs_rule = "<")
  plnt_sdl$OP      <- mgt_df$OPNUM[input_lst$lookup$management$OP ==
                                   sdl_df$OPERATION[i_op]]
  plnt_sdl$OP_TYPE <- crop_df$ICNUM[input_lst$lookup$crop$CPNM ==
                                    sdl_df$OP_TYPE[i_op]]
  plnt_sdl[,6:12]  <- sdl_df[i_op,8:14]
  plnt_sdl$PAR8    <- meta_data$CNOP$CN[meta_data$CNOP$OP ==
                                          sdl_df$OPERATION[i_op]]

  mgt_file <- plnt_sdl %>%
    mgt.format(.) %>%
    append(mgt_file, .)

  return(mgt_file)
}

## fert.crp(mgt_file, sdl_file, i_op, meta_data, mgt_df, fert_df, -------
##          pcp_df, tmp_df, amc_df)
fert_crp <- function(mgt_file, sdl_df, i_op, meta_data, mgt_df, fert_df,
                     pcp_df, tmp_df, amc_df) {
  op_year   <- sdl_df$YEAR[i_op]
  fert_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  fert_sdl[,1:2]   <- select_OPdate(sdl_df, i_op, op_year, prev_date,
                                        meta_data, tmp_df, pcp_df, amc_df,
                                        mon_thrs = 5, thrs_rule = "<")
  fert_sdl$OP      <- mgt_df$OPNUM[mgt_df$OP == sdl_df$OPERATION[i_op]]
  fert_sdl$OP_TYPE <- fert_df$IFNUM[fert_df$FERTNM == sdl_df$OP_TYPE[i_op]]
  fert_sdl[,6:13]  <- sdl_df[i_op,8:15]

  mgt_file <- fert_sdl %>%
    mgt.format(.) %>%
    append(mgt_file, .)

  return(mgt_file)
}

## hrv.kill(mgt_file, sdl_file, i_op, meta_data, mgt_df, pcp_df, --------
##          tmp_df, amc_df)
hrv_kill <- function(mgt_file, sdl_df, i_op, meta_data, mgt_df, pcp_df,
                     tmp_df, amc_df) {

  op_year   <- sdl_df$YEAR[i_op]
  hvst_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  hvst_sdl[,1:2]   <- select_OPdate(sdl_df, i_op, op_year, prev_date,
                                        meta_data, tmp_df, pcp_df, amc_df,
                                        mon_thrs = 7, thrs_rule = ">")
  hvst_sdl$OP      <- mgt_df$OPNUM[mgt_df$OP == sdl_df$OPERATION[i_op]]
  hvst_sdl[,6:13]  <- sdl_df[i_op,8:15]
  hvst_sdl$PAR3    <- meta_data$CNOP$CN[meta_data$CNOP$OP ==
                                          sdl_df$OPERATION[i_op]]

  mgt_file <- hvst_sdl %>%
    mgt.format(.) %>%
    append(mgt_file, .)

  return(mgt_file)
}

## till.op (mgt_file, sdl_file, i_op, meta_data, mgt_df, till_df, -------
##          pcp_df, tmp_df, amc_df)
till_op  <- function(mgt_file, sdl_df, i_op, meta_data, mgt_df, till_df,
                     pcp_df, tmp_df, amc_df) {
  op_year   <- sdl_df$YEAR[i_op]
  till_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  till_sdl[,1:2]   <- select_OPdate(sdl_df, i_op, op_year, prev_date,
                                        meta_data, tmp_df, pcp_df, amc_df,
                                        mon_thrs = 5, thrs_rule = "<")
  till_sdl$OP      <- mgt_df$OPNUM[mgt_df$OP == sdl_df$OPERATION[i_op]]
  till_sdl$OP_TYPE <- till_df$ITNUM[till_df$TILLNM == sdl_df$OP_TYPE[i_op]]
  till_sdl[,6:13]  <- sdl_df[i_op,8:15]
  till_sdl$PAR3    <- meta_data$CNOP$CN[meta_data$CNOP$OP ==
                                          sdl_df$OPERATION[i_op]]

  mgt_file <- till_sdl %>%
    mgt.format(.) %>%
    append(mgt_file, .)

  return(mgt_file)
}

## hrv.only(mgt_file, sdl_file, i_op, meta_data, mgt_df, pcp_df, -------
##          tmp_df, amc_df)
hrv_only <- function(mgt_file, sdl_df, i_op, meta_data, mgt_df, pcp_df,
                     tmp_df, amc_df) {
  op_year   <- sdl_df$YEAR[i_op]
  hvst_sdl  <- initialize_line()
  prev_date <- inquire_prevdate(mgt_file, op_year)

  hvst_sdl[,1:2]   <- select_OPdate(sdl_df, i_op, op_year, prev_date,
                                        meta_data, tmp_df, pcp_df, amc_df,
                                        mon_thrs = 5, thrs_rule = ">")
  hvst_sdl$OP      <- mgt_df$OPNUM[mgt_df$OP == sdl_df$OPERATION[i_op]]
  hvst_sdl[,6:13]  <- sdl_df[i_op,8:15]

  mgt_file <- hvst_sdl %>%
    mgt.format(.) %>%
    append(mgt_file, .)

  return(mgt_file)
}

## skip(mgt_file, sdl_df, i_op, mgt_df) ---------------------------------
skip <- function(mgt_file, sdl_df, i_op, mgt_df) {
  sdl <- sdl_df[i_op, -c(1,2)]
  sdl$OPERATION <- mgt_df$OPNUM[mgt_df$OP ==  sdl$OPERATION]
  sdl <- format_mgtstringout(sdl)
  mgt_file <- c(mgt_file, sdl)
  return(mgt_file)
}
