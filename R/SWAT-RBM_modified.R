#### #######################################################################
#                             SWAT-RBM v0.1                               #
# A Rule based management operation scheduling model for SWAT 2012 v0.1   #
#                                                                         #
#-------------------------------------------------------------------------#
# About:                                                                  #
#                                                                         #
# SWAT-RBM is a simple rule based model to schedule agricultural manage-  #
# ment operations for 2012. It was developed to solve problems that are   #
# encountered using the PHU concept implemented in SWAT. The rules        #
# implemented are:                                                        #
# - Operations in the spring planting and fall harvesting season are      #
#   tempearature dependend. Warm spring or warm fall lead to prolonged    #
#   seasons.                                                              #
# - The defined dates are rondomized by +/- 5 days.                       #
# - Operations are only set on dates where no precipitation occurs.       #
#                                                                         #
# Addtionally different farming practices can be considered providing     #
# several management scheduling input files and indicate their share in   #
# the given catchment.                                                    #
#                                                                         #
#-------------------------------------------------------------------------#
# Inputs:                                                                 #
#                                                                         #
#  mgmt_i - management scheduling input files :                           #
# At least one management input file must be provided in .csv format that #
# gives boundaries for time periods to set operations for all crops       #
# applied in the model. (see template file)                               #
#                                                                         #
# cnop_i - curve number operation files:                                  #
# At least one curve number operation file must be provided, giving infor-#
# mation about CN2 for the oparations plant, harvest, and tillage.        #
# For each mgmt_i file one corresponding cnop_i file must be provided.    #
# All files must be given in one directory. The path to the directory is  #
# asked by the routine.                                                   #
#                                                                         #
# Path to the TxtInOut directory.                                         #
#                                                                         #
#-------------------------------------------------------------------------#
# Output:                                                                 #
# The routine overwrites the management opeartion files directly in the   #
# defined TxtInOut directory. In order that the routine knows the struc-  #
# ture of the HRUs the HRU files of the SWAT model must be generated      #
# before applying this routine!                                           #
#                                                                         #
#-------------------------------------------------------------------------#
# Author:  Christoph Schuerz                                              #
# Contact: christoph.schuerz@boku.ac.at                                   #
###########################################################################

# Select paths for txtIO and mgmt -----------------------------------------
# mgmt_pth  <- choose.dir(caption = "Set path to management and cnop files")
# txtIO_pth <- choose.dir(caption = "Set path for txtIO directory")
mgmt_pth  <- "D:/Projects_R/AM_LUSEMOD/AM_RBM/Input"
txtIO_pth <- "D:/Projects_R/AM_LUSEMOD/AM_RBM/TxtInOut"

# Libraries ---------------------------------------------------------------
library(dplyr)
library(magrittr)
library(lubridate)
library(data.table)

# Functions ###############################################################
## sub.weather.lookup() ---------------------------------------------------
## Function to create the station lookup table for the subbasins found in the
## TxtIO directory. For running the function TxtIO must be set as working
## directory. The output is a data.frame holding the corresponding pcp and tmp
## stations to each subbasin.
sub.weater.lookup <- function(sub_path) {
  sub_list <- file.names(file_path = sub_path, pat_str = "\\.sub$")
  stat_lookup <- data.frame(SUB   = character(),
                            I_PCP = numeric(),
                            I_TMP = numeric(),
                            I_LAT = numeric())
  for (i in sub_list){
    temp  <- readLines(sub_path%/%i, warn = FALSE)
    i_sub <- scan(text = temp[1], what = "", quiet = TRUE)[4]
    i_pcp <- scan(text = temp[7], what = "", quiet = TRUE)[1]
    i_tmp <- scan(text = temp[8], what = "", quiet = TRUE)[1]
    i_lat <- scan(text = temp[5], what = "", quiet = TRUE)[1]

    stat_lookup <- rbind(stat_lookup,
                         data.frame(SUB   = as.numeric(i_sub),
                                    I_PCP = as.numeric(i_pcp),
                                    I_TMP = as.numeric(i_tmp),
                                    I_LAT = as.numeric(i_lat)))
  }
  return(stat_lookup)
}
## read.crop.lookup(plnt_dat) ---------------------------------------------
## Function to read the plant.dat file from the TxtInOut directory and to create
## the crop lookup table from this file requiered to convert crop labels to
## codes.
read.crop.lookup <- function (plnt_dat) {
  crop_lkp <- readLines(plnt_dat)
  keep_i   <- seq(1, length(crop_lkp), 5)
  crop_lkp <-  crop_lkp[keep_i]
  crop_lkp <- scan(text = crop_lkp, what = "", quiet = TRUE)
  crop_lkp <- t(matrix(data = crop_lkp, nrow = 3))
  crop_lkp <- data.frame(ICNUM = as.numeric(crop_lkp[,1]),
                         CPNM  = as.character(crop_lkp[,2]),
                         stringsAsFactors = FALSE)
  return(crop_lkp)
}
## mgmt.format(v_out) -----------------------------------------------------
## Function to set the format of the management operations right for writing
## them to the text files. As input the vector of one line of mgt operations is
## given. Output is the formatted string.
mgmt.format <- function(v_out){
  options(warn=-1)
  empty_spc <- c("   ", "  ", "        ", "  ", "    ", "   ", "  ", "            ",
                 "      ", "           ", "    ", "      ", "     ")
  var_fmt <- c("%3.0f", "%2.0f", "%8.3f", "%2.0f", "%4.0f", "%3.0f", "%2.0f", "%12.5f",
               "%6.2f", "%11.5f", "%4.2f", "%6.2f", "%5.2f")
  v_out <- sprintf(var_fmt, v_out)
  v_out[grep("NA", v_out)] <- empty_spc[grep("NA", v_out)]

  str_out <- paste(v_out[1:13], collapse = " ")
  return(str_out)
}

## hru.meta(mgmt_file, soil_file, crop_df, cnop_df) -----------------------
## Function extracts meta information, such as subbasin number, land use, or
## soil, for an HRU from the header of the management file as well as from the
## soil file and returns it as a list.
hru.meta <- function(mgmt_file, soil_file, crop_df, cnop_df) {
  hru_meta <- mgmt_file[1] %>%
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
## schedule.format(sdl_df, meta_data, n_yrs) -----------------------------
## Function takes the management schedule file and prepares it for writing the
## the operations to the management file.
schedule.format <- function(sdl_df, meta_data, lookup) {

  temp_sdl  <- filter(sdl_df, CROP == meta_data$LUSE)
  temp_sdl[temp_sdl == ""] <- NA
  n_op <- dim(temp_sdl)[1]

  temp_sdl <- temp_sdl[rep(seq(1, n_op), lookup$n_years),]
  temp_sdl %<>%  mutate(.,
                       YEAR = seq(lookup$bound_yrs[1],
                                  lookup$bound_yrs[2]) %>%
                              rep(. , each = n_op),
                       JDN1 = paste(YEAR,
                                    MON_1 %>% sprintf("%02d", .),
                                    DAY_1 %>% sprintf("%02d", .),
                                    sep = "") %>%
                              as.Date(.,"%Y%m%d") %>%
                              yday(.),
                       JDN2 = paste(YEAR,
                                    MON_2 %>% sprintf("%02d", .),
                                    DAY_2 %>% sprintf("%02d", .),
                                    sep = "") %>%
                              as.Date(.,"%Y%m%d") %>%
                              yday(.)) %>%
                select(., YEAR, JDN1, JDN2, MON_1, MON_2,
                          starts_with("OP"))

  temp_sdl <- rbind(filter(temp_sdl, OPERATION == "Initial crop")[1,],
                      filter(temp_sdl, OPERATION != "Initial crop"))
  temp_sdl <- filter(temp_sdl, !is.na(OPERATION))

  return(temp_sdl)
}
## previous.date(mgm_file, year_num) ----------------------------------
## Function extracts the date of the previous operation in the management file
## in JDN format.
previous.date <- function(mgmt_file, year_num) {
  prv_date <- NULL
  if(length(mgmt_file) > 30){
    prv_date <- mgmt_file %>%
                last(.) %>%
                substr(., 1, 6) %>%
                scan(text = ., what = " ", quiet = TRUE) %>%
                as.numeric(.) %>%
                sprintf("%02d", .)
    if (length(prv_date) > 0){
      prv_date <- paste(year_num,
                        prv_date[1],
                        prv_date[2],
                        sep = "-") %>%
                  as.Date(.) %>%
                  yday(.)
    }else prv_date = 0
  }else prv_date = 0
  return(prv_date)
}

## line.init() ------------------------------------------------------------
## Function to initialize the line that will be written into the management file
## as empty data.frame.

line.init <- function() {
  data.frame(MON = NA, DAY = NA, HUSC = NA, OP = NA, OP_TYPE = NA,
             PAR1 = NA, PAR2 = NA, PAR3 = NA, PAR4 = NA, PAR5 = NA,
             PAR6 = NA, PAR7 = NA, PAR8 = NA )
}
## jdn.to.monday(jdn_num, year_num, prv_date) -----------------------------
## Function takes julian day and year and converts them to integer values of
## month and day. Additionally the date is compared to previous date. If the
## date is lower than the previous date then it is set to prev_date + 1 before
## conversion.
jdn.to.monday <- function(jdn_num, year_num, prv_date) {
  if(jdn_num <= prv_date) jdn_num <- prv_date + 1

  paste(year_num,jdn_num, sep = "") %>%
    as.Date(., "%Y%j") %>%
    as.character(.) %>%
    strsplit(., "-") %>%
    unlist(.) %>%
    as.numeric(.) %>%
    .[2:3]
}
## jdn.to_mon(jdn_num, year_num) ------------------------------------------
## Function takes julian day and year and converts them to integer values of
## month.
jdn.to_ymd <- function(jdn_num, year_num) {
  paste(year_num,jdn_num, sep = "") %>%
    as.Date(., "%Y%j") %>%
    as.character(.) %>%
    strsplit(., "-") %>%
    unlist(.) %>%
    as.numeric(.) %>%
    .[3]
}
## select.op.date <- function(pcp_date, amc_date, pcp_thrs, amc_thrs) -----
##
select.op.date <- function(pcp_date, amc_date, pcp_thrs, amc_thrs){
  pcp_sel <- pcp_date %>% filter(., PCP < pcp_thrs)
  amc_sel <- amc_date %>% filter(., AMC < amc_thrs)
  op_date <- inner_join(pcp_sel, amc_sel, by = "JDN")

  if(dim(op_date)[1] > 0){
    op_date %<>%
      select(., JDN) %>%
      sample_n(., 1)
  } else {
    op_date <- inner_join(pcp_date, amc_date, by = "JDN") %>%
      mutate(., WGT = 10*PCP + AMC) %>%
      filter(., .$WGT == min(.$WGT)) %>%
      select(., JDN)
  }
  return(op_date)
}
## read.mgmt.cnop(mgmt_pth) -----------------------------------------------
read.mgmt.cnop <- function(mgmt_pth){

  mgmt_files <- list.files(mgmt_pth, pattern = "mgmt")
  name <- mgmt_files %>%
    strsplit(., "\\.|\\_") %>%
    unlist(.) %>%
    .[. != "mgmt" & . != "csv"]

  mgmt_cnop <- list()
  fraction <- data.frame(NAME = numeric(),
                         FRAC = numeric())

  for(i in name){
    mgmt <- read.csv(file = mgmt_pth%/%"mgmt"%_%i%.%"csv",
                           header = TRUE, sep = ",", stringsAsFactors = FALSE)
    cnop <- read.csv(file = mgmt_pth%/%"cnop"%_%i%.%"csv",
                    header = TRUE, sep = ",", stringsAsFactors = FALSE)
    fraction <- rbind(fraction,
                       data.frame(NAME = name,
                                  FRAC = as.numeric(
                                  mgmt$OP_TYPE[mgmt$OPERATION == "Fraction"]),
                                  stringsAsFactors = FALSE))
    mgmt <- mgmt[!mgmt$OPERATION == "Fraction",]
    mgmt_cnop[["mgmt"%_%i]] <- mgmt
    mgmt_cnop[["cnop"%_%i]] <- cnop
  }
  fraction$FRAC <- cumsum(fraction$FRAC) / sum(fraction$FRAC)
  mgmt_cnop[["fraction"]] <- fraction
  return(mgmt_cnop)
}
## read.lookup_tables(txtIO_pth) ------------------------------------------
read.lookup_tables <- function(txtIO_pth){
  lookup <- list()
  lookup[["management"]] <- data.frame(OPNUM = seq(0,17),
                                       OP = c("End of year",
                                              "Plant",
                                              "Irrigation",
                                              "Fertilizer",
                                              "Pesticide",
                                              "Harvest & Kill",
                                              "Tillage",
                                              "Harvest only",
                                              "Kill only",
                                              "Grazing",
                                              "Auto irrigation",
                                              "Auto fertilization",
                                              "Street sweeping",
                                              "Release/impound",
                                              "Cont/Fert",
                                              "Cont/Pest",
                                              "Burn",
                                              "Skip"))

  lookup[["fertilizer"]] <- txtIO_pth%/%"fert.dat" %>%
    read.table(file = ., stringsAsFactors = FALSE) %>%
    .[,1:2] %>%
    rename.col(.,c("IFNUM", "FERTNM"))

  lookup[["tillage"]] <- txtIO_pth%/%"till.dat" %>%
    read.fwf(file = ., widths = c(4,12,53), stringsAsFactors = FALSE) %>%
    .[,1:2] %>%
    rename.col(.,c("ITNUM", "TILLNM")) %>%
    trim.col(.,2)

  lookup[["crop"]] <- read.crop.lookup(txtIO_pth%/%"plant.dat")

  lookup[["station"]] <- sub.weater.lookup(txtIO_pth)
  lookup[["n_subbasin"]] <- dim(lookup[["station"]])[1]

  lookup[["n_years"]] <- txtIO_pth%/%"file.cio" %>%
    readLines(.) %>%
    .[8] %>%
    scan(text = ., what = "", quiet = TRUE) %>%
    .[1] %>%
    as.numeric(.)

  lookup[["bound_yrs"]] <- txtIO_pth%/%"file.cio" %>%
    readLines(.) %>%
    .[9] %>%
    scan(text = ., what = "", quiet = TRUE) %>%
    .[1] %>%
    as.numeric(.)

  lookup[["bound_yrs"]] <- c(lookup[["bound_yrs"]],
                             lookup[["bound_yrs"]] + lookup[["n_years"]] - 1)

  return(lookup)
}
## select.mgmt.cnop(mgmt_cnop) --------------------------------------------
select.mgmt.cnop <- function(mgmt_cnop){
  prob_sel <- which(mgmt_cnop$fraction$FRAC >= runif(1))[1]
  mgmt_cnop$fraction$NAME[prob_sel]
}
## calc.norm_temp(tmp_df, lookup) -----------------------------------------
calc.norm_temp <- function (tmp_df, lookup) {
  #### Aggregate data to monthly daily mean temperature values
  tmp_df %<>%
    group_by(., YEAR, MON) %>%
    summarize_each(.,funs(mean.na_rm)) %>%
    select(., YEAR, MON, starts_with("SUB"))

  #### Calculate normalized deviations to the monthly daily mean tempeartures
  #### for each subbasin.
  tmp_df_stat <- aggregate(x = tmp_df,
                            by = list(tmp_df$MON),
                            FUN = mean.na_rm)
  tmp_df_stat <- rowMeans(tmp_df_stat[,4:dim(tmp_df_stat)[2]])

  tmp_norm <- tmp_df[, 3:dim(tmp_df)[2]] - tmp_df_stat[rep(1:12, lookup$n_years)]

  norm_min_max <- matrix(data = as.matrix(tmp_norm), nrow = 12)
  norm_min_max <- data.frame(MON = 1:12,
                             MIN = apply(norm_min_max, 1, min),
                             MAX = apply(norm_min_max, 1, max))
  norm_min_max <- norm_min_max[rep(1:12, lookup$n_years),]

  tmp_norm_pos <- tmp_norm
  tmp_norm_pos[tmp_norm_pos < 0 ] <- 0

  tmp_norm_neg <- tmp_norm
  tmp_norm_neg[tmp_norm_neg > 0 ] <- 0

  tmp_norm <- tmp_norm_pos / (2*abs(norm_min_max$MAX)) +
              tmp_norm_neg / (2*abs(norm_min_max$MIN))
  tmp_norm <- cbind(tmp_df[,1:2], tmp_norm)

  tmp_norm <- rename.col(tmp_norm,
                         c("YEAR", "MON",
                           "SUB"%_%sprintf("%03d",seq(1:lookup$n_subbasin))))
  return(tmp_norm)
}
## ET0.FAOHargreaves(t_min, t_max, sub_info) ------------------------------
ET0.FAOHargreaves <- function (t_min, t_max, lookup) {
  #### Calculate ET values applying FAO-56 Hargreaves (Allen et al., 1998)
  # Estimate Ra according to Allen at al., 1998
  # (http://www.fao.org/docrep/x0490e/x0490e07.htm#calculation%20procedures)
  G_sc <- 0.0820
  delta <- 0.409*sin(2*pi*t_min$JDN/365 - 1.39)
  d_r <- 1 + 0.033*cos(2*pi*t_min$JDN/365)

  t_min  %<>% select(., starts_with("SUB"))
  t_max  %<>% select(., starts_with("SUB"))

  R_a <- data.frame(matrix(data = NA,
                           nrow = dim(t_min)[1],
                           ncol = lookup$n_subbasin))
  for(i in 1:lookup$n_subbasin){
    phi <- lookup$station$I_LAT [i]*pi/180
    w_s <- acos(-tan(phi)*tan(delta))
    R_a[i] <- 24*60/pi*0.408*G_sc*d_r*(w_s*sin(phi)*sin(delta) +
                                    cos(phi)*cos(delta)*sin(w_s))
  }

  # Estimate ET0 according to Allen at al., 1998
  # http://www.fao.org/docrep/x0490e/x0490e07.htm#an%20alternative%20equation
  # %20for%20eto%20when%20weather%20data%20are%20missing

  et0 <- 0.0023*((t_min + t_max)/2 + 17.8)* (t_max - t_min)^0.5 * R_a

  et0 <- rename.col(et0, "SUB"%_%sprintf("%03d",1:lookup$n_subbasin))
  return(et0)
}
## ACM.estimate(wtr_bal, k_ret) -------------------------------------------
#Function takes time series of water balance (P - ET0) and estimates the
#antecedent soil moisture (ACM) for each subbasin and respective hydrologic soil
#group (A to D).
AMC.estimate <- function (wtr_bal, k_ret, n_prevday) {

  if(length(k_ret) != 4) {stop("Length of k_ret not equal to 4!")}

  ant_moist <- list()

  ant.days <- function(ind, var_ts, n_days){
    var_fill <- NULL
    if(ind <= n_days){
      var_fill   <- rep(0, (n_days+1) - ind)
      n_days <- ind - 1
    }
    var_ant <- c(var_fill,var_ts[(ind - n_days):ind])
    return(var_ant)
  }

  ant.bal.sum <- function(ant_bal_i, k){
    n_day <- seq(length(ant_bal_i) -1, 0, -1)
    k_day <- k^n_day
    sum(ant_bal_i*k_day)
  }

  for (i in 1:4){
    ant_bal_i <- t(sapply(1:length(wtr_bal), ant.days,
                          wtr_bal, n_prevday, simplify = TRUE))
    ant_moist_i <- apply(ant_bal_i, 1, ant.bal.sum, k = k_ret[i])
    ant_moist_i[ant_moist_i < 0] <- 0
    ant_moist[[LETTERS[i]]] <- ant_moist_i
  }
  return(ant_moist)
}

## weatherdep.opdate(sdl_df, i_op, op_year, prv_date, meta_data, ----------
##                   tmp_df, pcp_df, mon_thrs)
weatherdep.opdate <- function(sdl_df, i_op, op_year, prv_date, meta_data,
                              tmp_df, pcp_df, amc_df, mon_thrs, thrs_rule) {

  # Routine to avoid errors if previous line is empty or NA
  same_date = FALSE
  if(length(sdl_df$JDN1[i_op - 1]) != 0 ) {
    if(!is.na(sdl_df$JDN1[i_op - 1])) {
      if((sdl_df$JDN1[i_op] == sdl_df$JDN1[i_op-1]) &
         (sdl_df$JDN2[i_op] == sdl_df$JDN2[i_op-1])) {
        same_date = TRUE
      }
    }
  }

  if(same_date) {
    op_date <- jdn.to.monday(prv_date, op_year, 0)
  } else if (do.call(thrs_rule, list(sdl_df$MON_1[i_op], mon_thrs))) {

    if(thrs_rule == "<") {
      prev_mon <- -1
      next_mon <-  0
    } else {
      prev_mon <-  0
      next_mon <-  1
    }
    tmp_op <- tmp_df %>%
      filter(., YEAR == op_year) %>%
      filter(., MON >= (sdl_df$MON_1[i_op] + prev_mon) &
                MON <= (sdl_df$MON_2[i_op] + next_mon)) %>%
      select(., contains(meta_data$SUB)) %>%
      .[,1] %>%
      mean(., na.rm = TRUE)

    jdn_dates <- c(sdl_df$JDN2[i_op],sdl_df$JDN1[i_op])
    jdn_init  <- (mean(jdn_dates) +
                  diff(jdn_dates)*tmp_op*(prev_mon + next_mon)) %>%
                 round(., digits = 0)

    pcp_date <- pcp_df %>%
      filter(., YEAR == op_year) %>%
      filter(., JDN >= (jdn_init - 5) &
                JDN <= (jdn_init + 5)) %>%
      select(., JDN, contains(meta_data$SUB)) %>%
      rename.col(.,c("JDN", "PCP"))

    amc_date <- amc_df %>%
      .[["SUB"%_%meta_data$SUB]] %>%
      filter(., YEAR == op_year) %>%
      filter(., JDN >= (jdn_init - 5) &
               JDN <= (jdn_init + 5)) %>%
      select(., JDN, ends_with(meta_data$SOIL)) %>%
      rename.col(.,c("JDN", "AMC"))

    op_date <- select.op.date(pcp_date, amc_date, 3, 25) %>%
      jdn.to.monday(., op_year, prv_date)
  } else {
    pcp_date <- pcp_df %>%
      filter(., YEAR == op_year) %>%
      filter(., JDN >= (sdl_df$JDN1[i_op] - 5) &
               JDN <= (sdl_df$JDN2[i_op] + 5)) %>%
      select(., JDN, contains(meta_data$SUB)) %>%
      rename.col(.,c("JDN", "PCP"))

    amc_date <- amc_df %>%
      .[["SUB"%_%meta_data$SUB]] %>%
      filter(., YEAR == op_year) %>%
      filter(., JDN >= (sdl_df$JDN1[i_op] - 5) &
               JDN <= (sdl_df$JDN2[i_op] + 5)) %>%
      select(., JDN, ends_with(meta_data$SOIL)) %>%
      rename.col(.,c("JDN", "AMC"))

    op_date <- select.op.date(pcp_date, amc_date, 5, 25) %>%
      jdn.to.monday(., op_year, prv_date)
  }
  return(op_date)
}
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

# Read and edit input data ################################################
## Initiate progress bar for writing MGT files ----------------------------
prgr_bar <- winProgressBar(title = "Reading/Editing input files",
                           label = "0% done", min = 0, max = 100, initial = 0)
## Read weather inputs ----------------------------------------------------
### Read the min/max tempeartures for the stations:
tmp_dat <- read.weather_input(txtIO_pth%/%"Tmp1.Tmp", "XXX.X")
setWinProgressBar(prgr_bar, 30, label= "30% done")

### Read the precipitation data for the stations:
pcp_dat <- read.weather_input(txtIO_pth%/%"pcp1.pcp", "XXX.XX")
setWinProgressBar(prgr_bar, 80, label= "80% done")
## Read management schedule file and all lookup tables --------------------
### Management schedule files and lookup tables for curve numbers (CN) according
### to operation and soil type
mgmt_cnop_data <- read.mgmt.cnop(mgmt_pth)
rm(mgmt_pth)

### List of lookup tables holding management, fertilizer, tillage, and crop types.
### The subbasin files contain the information which weather stations SWAT
### allocates to which subbasins. The output is a lookup table of station
### allocations. This is also added to the lookup list.
lookup <- read.lookup_tables(txtIO_pth)
## Edit weather inputs ----------------------------------------------------
### Edit precipitation data
#### Assign the station precipitation data to the respective subbasins
#### and aggregate the precipitation data to daily accumulated values
pcp_dat %<>% mutate.weather_input(., lookup, "PCP") %>%
             trim.timeseries(., lookup)

### Edit temperature data
#### Assign the station temperature data to the respective subbasins
tmp_min <- tmp_dat[,c(1,seq(3, dim(tmp_dat)[2], 2))]
tmp_max <- tmp_dat[,c(1,seq(2, dim(tmp_dat)[2], 2))]
tmp_dat <- cbind(tmp_dat[,1],
                 (tmp_dat[,seq(2, dim(tmp_dat)[2], 2)] +
                  tmp_dat[,seq(3, dim(tmp_dat)[2], 2)])/2)

tmp_min %<>% mutate.weather_input(., lookup, "TMP") %>%
             trim.timeseries(., lookup)
tmp_max %<>% mutate.weather_input(., lookup, "TMP") %>%
             trim.timeseries(., lookup)
tmp_dat %<>% mutate.weather_input(., lookup, "TMP")

#### Calculate normalized deviations to the monthly daily mean tempeartures
#### for each subbasin.
tmp_norm <- calc.norm_temp(tmp_dat, lookup) %>%
  trim.timeseries(., lookup)
rm(tmp_dat)
setWinProgressBar(prgr_bar, 90, label= "90% done")
#### Calculate antecedent water content
et0 <- ET0.FAOHargreaves(tmp_min, tmp_max, lookup)
pcp_et_bal <- pcp_dat[,5:dim(pcp_dat)[2]] - et0
amc_dat <- apply(pcp_et_bal, 2, AMC.estimate, c(0.8,0.85,0.90, 0.95), 5)

for(i in 1:lookup$n_subbasin){
  amc_dat[[i]] <- cbind(pcp_dat[,1:4],amc_dat[[i]])
}
rm(tmp_min, tmp_max, i, et0,pcp_et_bal)

#### set and close progress bar
setWinProgressBar(prgr_bar, 100, label= "100% finished")
Sys.sleep(1)
close(prgr_bar)

# Edit and write management files #########################################
## Initiate progress bar for writing MGT files ----------------------------
prgr_bar <- winProgressBar(title = "Rewriting MGT files", label = "0% written",
                           min = 0, max = 100, initial = 0)
## List all management and soil files in TxtIO ----------------------------
hru_list <- file.names(pat_str = "\\.hru$", file_path = txtIO_pth)
hru_list <- substr(hru_list, 1, 9)
## Get individual mgmt files and edit them --------------------------------
for (i_hru in hru_list){
  mgmt_i <- readLines(txtIO_pth%/%i_hru%.%"mgt", warn = FALSE)
  soil_i <- readLines(txtIO_pth%/%i_hru%.%"sol", warn = FALSE)

  mgmt_cnop_sel <- select.mgmt.cnop(mgmt_cnop_data)

  mgmt_i_meta <- hru.meta(mgmt_i,
                          soil_i,
                          lookup$crop,
                          mgmt_cnop_data[["cnop"%_%mgmt_cnop_sel]])

  mgmt_i_sdl  <- schedule.format(mgmt_cnop_data[["mgmt"%_%mgmt_cnop_sel]],
                                 mgmt_i_meta,
                                 lookup)

  n_op  <- dim(mgmt_i_sdl)[1]
  n_rot <- length(which(mgmt_i_sdl$OPERATION == "End of year"))
  mgmt_i[29] <- paste(sprintf("%16i", n_rot),
                      "   | NROT: number of years of rotation")
  mgmt_i <- mgmt_i[1:30]

  for (i in 1:n_op){
    mgmt_i <-   switch (mgmt_i_sdl[i,]$OPERATION,
                        "Initial crop"   = init.crp(mgmt_i,
                                                    mgmt_i_sdl,
                                                    i,
                                                    lookup$crop),
                        "End of year"    = end.year(mgmt_i,
                                                    mgmt_i_sdl,
                                                    i,
                                                    lookup$management),
                        "Plant"          = plnt.crp(mgmt_i,
                                                    mgmt_i_sdl,
                                                    i,
                                                    mgmt_i_meta,
                                                    lookup$management,
                                                    lookup$crop,
                                                    pcp_dat,
                                                    tmp_norm,
                                                    amc_dat),
                        "Fertilizer"     = fert.crp(mgmt_i,
                                                    mgmt_i_sdl,
                                                    i,
                                                    mgmt_i_meta,
                                                    lookup$management,
                                                    lookup$fertilizer,
                                                    pcp_dat,
                                                    tmp_norm,
                                                    amc_dat),
                        "Harvest & Kill" = hrv.kill(mgmt_i,
                                                    mgmt_i_sdl,
                                                    i,
                                                    mgmt_i_meta,
                                                    lookup$management,
                                                    pcp_dat,
                                                    tmp_norm,
                                                    amc_dat),
                        "Tillage"        = till.op (mgmt_i,
                                                    mgmt_i_sdl,
                                                    i,
                                                    mgmt_i_meta,
                                                    lookup$management,
                                                    lookup$tillage,
                                                    pcp_dat,
                                                    tmp_norm,
                                                    amc_dat),
                        "Harvest only"   = hrv.only(mgmt_i,
                                                    mgmt_i_sdl,
                                                    i,
                                                    mgmt_i_meta,
                                                    lookup$management,
                                                    pcp_dat,
                                                    tmp_norm,
                                                    amc_dat),
                        "Skip"           = skip.fun(mgmt_i,
                                                    mgmt_i_sdl,
                                                    i,
                                                    lookup$management)
                        )
  }
## Write files in TxtIO ---------------------------------------------------
  writeLines(mgmt_i, con = txtIO_pth%/%i_hru%.%"mgt")
## Update progress bar for writing MGT files ------------------------------
  i_prog <- which(i_hru == hru_list)/length(hru_list)
  lbl_up <- sprintf("%d%% written", round(i_prog*100))
  setWinProgressBar(prgr_bar, i_prog*100, label=lbl_up)
}
close(prgr_bar)


