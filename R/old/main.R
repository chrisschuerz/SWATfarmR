# Select paths for txtIO and mgmt -----------------------------------------
# mgmt_pth  <- choose.dir(caption = "Set path to management and cnop files")
# txtIO_pth <- choose.dir(caption = "Set path for txtIO directory")
mgt_pth  <- "D:/Projects_R/AM_LUSEMOD/AM_RBM/Input"
txtIO_pth <- "D:/Projects_R/AM_LUSEMOD/AM_RBM/TxtInOut"

# Libraries ---------------------------------------------------------------
library(dplyr)
library(reshape2)
library(magrittr)
library(lubridate)
library(data.table)


# Read and edit input data ################################################
# Initiate progress bar for writing MGT files ----------------------------
print("Read and prepare input data:")
prgr_bar <- txtProgressBar(min = 0, max = 100, initial = 0, style = 3)
# Read weather inputs ----------------------------------------------------
# Read the min/max tempeartures for the stations:
temp_data <- read_weather(txtIO_pth%//%"Tmp1.Tmp", "XXX.X")
setTxtProgressBar(prgr_bar, 30)

# Read the precipitation data for the stations:
precip_data <- read_weather(txtIO_pth%//%"pcp1.pcp", "XXX.XX")
setTxtProgressBar(prgr_bar, 80)
# Read management schedule file and all lookup tables --------------------
# Management schedule files and lookup tables for curve numbers (CN) according
# to operation and soil type
mgt_cnop <- read_mgtcnop(mgt_pth)
rm(mgt_pth)

# List of lookup tables holding management, fertilizer, tillage, and crop types.
# The subbasin files contain the information which weather stations SWAT
# allocates to which subbasins. The output is a lookup table of station
# allocations. This is also added to the lookup list.
lookup <- read_lookuptables(txtIO_pth)

# Edit weather inputs ----------------------------------------------------
# Edit precipitation data
# Assign the station precipitation data to the respective subbasins
# and aggregate the precipitation data to daily accumulated values
precip_data %<>% modify_weather(., lookup, "PCP") %>%
                 limit_timespan(., lookup)

# Edit temperature data
# Assign the station temperature data to the respective subbasins
temp_min <- temp_data[,c(1,seq(3, dim(temp_data)[2], 2))]
temp_max <- temp_data[,c(1,seq(2, dim(temp_data)[2], 2))]
temp_mean <- cbind(temp_data[,1],
                  (temp_data[,seq(2, dim(temp_data)[2], 2)] +
                   temp_data[,seq(3, dim(temp_data)[2], 2)])/2)
rm(temp_data)

temp_min %<>%  modify_weather(., lookup, "TMP") %>%
               limit_timespan(., lookup)
temp_max %<>%  modify_weather(., lookup, "TMP") %>%
               limit_timespan(., lookup)
temp_mean %<>% modify_weather(., lookup, "TMP")

# Calculate normalized deviations to the monthly daily mean tempeartures
# for each subbasin.
temp_index <- compute_TIndex(temp_mean, lookup) %>%
  limit_timespan(., lookup)
rm(temp_mean)
setTxtProgressBar(prgr_bar, 90)
# Calculate antecedent water content
t1 <- system.time({
amc_dat <- (select(precip_data, starts_with("SUB")) -
  compute_ET0Hargreaves(temp_min, temp_max, lookup)) %>%
  apply(., 2, compute_AMC, c(0.8,0.85,0.90, 0.95), 5) %>%
  lapply(., cbind, precip_data[,1:4]) %>%
  lapply(., select,c(YEAR,MON,DAY,JDN,A,B,C,D))

})
rm(temp_min, temp_max)

# set and close progress bar
setTxtProgressBar(prgr_bar, 100)
Sys.sleep(1)
close(prgr_bar)

# Edit and write management files #########################################
## Initiate progress bar for writing MGT files ----------------------------
print("Rewrite management input files:")
prgr_bar <- txtProgressBar(min = 0, max = 100, initial = 0)
## List all management and soil files in TxtIO ----------------------------
hru_list <- file.names(pat_str = "\\.hru$", file_path = txtIO_pth)
hru_list <- substr(hru_list, 1, 9)
## Get individual mgmt files and edit them --------------------------------
for (i_hru in hru_list){
  mgmt_i <- readLines(txtIO_pth%//%i_hru%.%"mgt", warn = FALSE)
  soil_i <- readLines(txtIO_pth%//%i_hru%.%"sol", warn = FALSE)

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
  writeLines(mgmt_i, con = txtIO_pth%//%i_hru%.%"mgt")
## Update progress bar for writing MGT files ------------------------------
  i_prog <- which(i_hru == hru_list)/length(hru_list)
  setTxtProgressBar(prgr_bar, i_prog*100)
}
close(prgr_bar)


