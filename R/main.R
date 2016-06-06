# Select paths for txtIO and mgmt -----------------------------------------
# mgmt_pth  <- choose.dir(caption = "Set path to management and cnop files")
# txtIO_pth <- choose.dir(caption = "Set path for txtIO directory")
mgt_pth  <- "D:/Projects_R/AM_LUSEMOD/AM_RBM/Input"
txtIO_pth <- "D:/Projects_R/AM_LUSEMOD/AM_RBM/TxtInOut"

# Libraries ---------------------------------------------------------------
library(dplyr)
library(magrittr)
library(lubridate)
library(data.table)


# Read and edit input data ################################################
## Initiate progress bar for writing MGT files ----------------------------
print("Read and prepare input data:")
prgr_bar <- txtProgressBar(min = 0, max = 100, initial = 0, style = 3)
## Read weather inputs ----------------------------------------------------
### Read the min/max tempeartures for the stations:
tmp_dat <- read_weather(txtIO_pth%/%"Tmp1.Tmp", "XXX.X")
setTxtProgressBar(prgr_bar, 30)

### Read the precipitation data for the stations:
pcp_dat <- read_weather(txtIO_pth%/%"pcp1.pcp", "XXX.XX")
setTxtProgressBar(prgr_bar, 80)
## Read management schedule file and all lookup tables --------------------
### Management schedule files and lookup tables for curve numbers (CN) according
### to operation and soil type
mgt_cnop <- read_mgmtcnop(mgt_pth)
rm(mgt_pth)

### List of lookup tables holding management, fertilizer, tillage, and crop types.
### The subbasin files contain the information which weather stations SWAT
### allocates to which subbasins. The output is a lookup table of station
### allocations. This is also added to the lookup list.
lookup <- read.lookup_tables(txtIO_pth)
## Edit weather inputs ----------------------------------------------------
### Edit precipitation data
#### Assign the station precipitation data to the respective subbasins
#### and aggregate the precipitation data to daily accumulated values
pcp_dat %<>% modify_weather(., lookup, "PCP") %>%
             limit_timespan(., lookup)

### Edit temperature data
#### Assign the station temperature data to the respective subbasins
tmp_min <- tmp_dat[,c(1,seq(3, dim(tmp_dat)[2], 2))]
tmp_max <- tmp_dat[,c(1,seq(2, dim(tmp_dat)[2], 2))]
tmp_dat <- cbind(tmp_dat[,1],
                 (tmp_dat[,seq(2, dim(tmp_dat)[2], 2)] +
                  tmp_dat[,seq(3, dim(tmp_dat)[2], 2)])/2)

tmp_min %<>% modify_weather(., lookup, "TMP") %>%
             limit_timespan(., lookup)
tmp_max %<>% modify_weather(., lookup, "TMP") %>%
             limit_timespan(., lookup)
tmp_dat %<>% modify_weather(., lookup, "TMP")

#### Calculate normalized deviations to the monthly daily mean tempeartures
#### for each subbasin.
tmp_norm <- calc.norm_temp(tmp_dat, lookup) %>%
  trim.timeseries(., lookup)
rm(tmp_dat)
setTxtProgressBar(prgr_bar, 90)
#### Calculate antecedent water content
et0 <- ET0.FAOHargreaves(tmp_min, tmp_max, lookup)
pcp_et_bal <- pcp_dat[,5:dim(pcp_dat)[2]] - et0
amc_dat <- apply(pcp_et_bal, 2, AMC.estimate, c(0.8,0.85,0.90, 0.95), 5)

for(i in 1:lookup$n_subbasin){
  amc_dat[[i]] <- cbind(pcp_dat[,1:4],amc_dat[[i]])
}
rm(tmp_min, tmp_max, i, et0,pcp_et_bal)

#### set and close progress bar
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
  setTxtProgressBar(prgr_bar, i_prog*100)
}
close(prgr_bar)


