## init.crp(mgmt_file, sdl_df, i_op) --------------------------------------
## Function writes initial plant cover into the management operation file.
init.crp <- function(mgmt_file, sdl_df, i_op, crop_lkp) {
  mgmt_file[4] <- paste(sprintf("%16i", 1),
                        "   | IGRO: Land cover status: 0-none growing; 1-growing")
  crop_id   <- which(crop_lkp$CPNM == sdl_df[i_op,]$OP_TYPE)
  mgmt_file[5] <- paste(sprintf("%16i", crop_id),
                        "   | PLANT_ID: Land cover ID number (IGRO = 1)")
  mgmt_file[6] <- paste(sprintf("%16.2f", sdl_df[i_op,]$OP_VAL_1),
                        "   | LAI_INIT: Initial leaf are index (IGRO = 1)")
  mgmt_file[7] <- paste(sprintf("%16.2f", sdl_df[i_op,]$OP_VAL_2),
                        "   | BIO_INIT: Initial biomass (kg/ha) (IGRO = 1)")
  mgmt_file[8] <- paste(sprintf("%16.2f", sdl_df[i_op,]$OP_VAL_3),
                        "   | PHU_PLT: Number of heat units to bring plant to maturity (IGRO = 1)")
  return(mgmt_file)
}

## end.year(mgmt_file, sdl_df, i_op, mgmt_df) -----------------------------
## Function writes end of year operation into the management operation file.
end.year <- function(mgmt_file, sdl_df, i_op, mgmt_df) {
  temp <- sdl_df[i_op, -c(1,2)]
  temp$OPERATION <- mgmt_df$OPNUM[mgmt_df$OP ==  temp$OPERATION]
  temp <- mgmt.format(temp)
  mgmt_file <- c(mgmt_i, temp)
}
## plnt.crp(mgmt_file, sdl_df, i_op, meta_data, mgmt_df, crop_df, ---------
##          pcp_df, tmp_df, amc_df)
plnt.crp <- function(mgmt_file, sdl_df, i_op, meta_data,
                     mgmt_df, crop_df, pcp_df, tmp_df, amc_df) {
  op_year   <- sdl_df$YEAR[i_op]
  plnt_sdl  <- line.init()
  prev_date <- previous.date(mgmt_file, op_year)

  plnt_sdl[,1:2]   <- weatherdep.opdate(sdl_df, i_op, op_year, prev_date,
                                        meta_data, tmp_df, pcp_df, amc_df,
                                        mon_thrs = 5, thrs_rule = "<")
  plnt_sdl$OP      <- mgmt_df$OPNUM[mgmt_df$OP == sdl_df$OPERATION[i_op]]
  plnt_sdl$OP_TYPE <- crop_df$ICNUM[crop_df$CPNM == sdl_df$OP_TYPE[i_op]]
  plnt_sdl[,6:12]  <- sdl_df[i_op,8:14]
  plnt_sdl$PAR8    <- meta_data$CNOP$CN[meta_data$CNOP$OP ==
                                          sdl_df$OPERATION[i_op]]

  mgmt_file <- plnt_sdl %>%
    mgmt.format(.) %>%
    append(mgmt_file, .)

  return(mgmt_file)
}

## fert.crp(mgmt_file, sdl_file, i_op, meta_data, mgmt_df, fert_df, -------
##          pcp_df, tmp_df, amc_df)
fert.crp <- function(mgmt_file, sdl_df, i_op, meta_data, mgmt_df, fert_df,
                     pcp_df, tmp_df, amc_df) {
  op_year   <- sdl_df$YEAR[i_op]
  fert_sdl  <- line.init()
  prev_date <- previous.date(mgmt_file, op_year)

  fert_sdl[,1:2]   <- weatherdep.opdate(sdl_df, i_op, op_year, prev_date,
                                        meta_data, tmp_df, pcp_df, amc_df,
                                        mon_thrs = 5, thrs_rule = "<")
  fert_sdl$OP      <- mgmt_df$OPNUM[mgmt_df$OP == sdl_df$OPERATION[i_op]]
  fert_sdl$OP_TYPE <- fert_df$IFNUM[fert_df$FERTNM == sdl_df$OP_TYPE[i_op]]
  fert_sdl[,6:13]  <- sdl_df[i_op,8:15]

  mgmt_file <- fert_sdl %>%
    mgmt.format(.) %>%
    append(mgmt_file, .)

  return(mgmt_file)
}

## hrv.kill(mgmt_file, sdl_file, i_op, meta_data, mgmt_df, pcp_df, --------
##          tmp_df, amc_df)
hrv.kill <- function(mgmt_file, sdl_df, i_op, meta_data, mgmt_df, pcp_df,
                     tmp_df, amc_df) {

  op_year   <- sdl_df$YEAR[i_op]
  hvst_sdl  <- line.init()
  prev_date <- previous.date(mgmt_file, op_year)

  hvst_sdl[,1:2]   <- weatherdep.opdate(sdl_df, i_op, op_year, prev_date,
                                        meta_data, tmp_df, pcp_df, amc_df,
                                        mon_thrs = 7, thrs_rule = ">")
  hvst_sdl$OP      <- mgmt_df$OPNUM[mgmt_df$OP == sdl_df$OPERATION[i_op]]
  hvst_sdl[,6:13]  <- sdl_df[i_op,8:15]
  hvst_sdl$PAR3    <- meta_data$CNOP$CN[meta_data$CNOP$OP ==
                                          sdl_df$OPERATION[i_op]]

  mgmt_file <- hvst_sdl %>%
    mgmt.format(.) %>%
    append(mgmt_file, .)

  return(mgmt_file)
}

## till.op (mgmt_file, sdl_file, i_op, meta_data, mgmt_df, till_df, -------
##          pcp_df, tmp_df, amc_df)
till.op  <- function(mgmt_file, sdl_df, i_op, meta_data, mgmt_df, till_df,
                     pcp_df, tmp_df, amc_df) {
  op_year   <- sdl_df$YEAR[i_op]
  till_sdl  <- line.init()
  prev_date <- previous.date(mgmt_file, op_year)

  till_sdl[,1:2]   <- weatherdep.opdate(sdl_df, i_op, op_year, prev_date,
                                        meta_data, tmp_df, pcp_df, amc_df,
                                        mon_thrs = 5, thrs_rule = "<")
  till_sdl$OP      <- mgmt_df$OPNUM[mgmt_df$OP == sdl_df$OPERATION[i_op]]
  till_sdl$OP_TYPE <- till_df$ITNUM[till_df$TILLNM == sdl_df$OP_TYPE[i_op]]
  till_sdl[,6:13]  <- sdl_df[i_op,8:15]
  till_sdl$PAR3    <- meta_data$CNOP$CN[meta_data$CNOP$OP ==
                                          sdl_df$OPERATION[i_op]]

  mgmt_file <- till_sdl %>%
    mgmt.format(.) %>%
    append(mgmt_file, .)

  return(mgmt_file)
}

## hrv.only(mgmt_file, sdl_file, i_op, meta_data, mgmt_df, pcp_df, -------
##          tmp_df, amc_df)
hrv.only <- function(mgmt_file, sdl_df, i_op, meta_data, mgmt_df, pcp_df,
                     tmp_df, amc_df) {
  op_year   <- sdl_df$YEAR[i_op]
  hvst_sdl  <- line.init()
  prev_date <- previous.date(mgmt_file, op_year)

  hvst_sdl[,1:2]   <- weatherdep.opdate(sdl_df, i_op, op_year, prev_date,
                                        meta_data, tmp_df, pcp_df, amc_df,
                                        mon_thrs = 5, thrs_rule = ">")
  hvst_sdl$OP      <- mgmt_df$OPNUM[mgmt_df$OP == sdl_df$OPERATION[i_op]]
  hvst_sdl[,6:13]  <- sdl_df[i_op,8:15]

  mgmt_file <- hvst_sdl %>%
    mgmt.format(.) %>%
    append(mgmt_file, .)

  return(mgmt_file)
}

## skip(mgmt_file, sdl_df, i_op, mgmt_df) ---------------------------------
skip.fun <- function(mgmt_file, sdl_df, i_op, mgmt_df) {
  temp <- sdl_df[i_op, -c(1,2)]
  temp$OPERATION <- mgmt_df$OPNUM[mgmt_df$OP ==  temp$OPERATION]
  temp <- mgmt.format(temp)
  mgmt_file <- c(mgmt_file, temp)
  return(mgmt_file)
}
