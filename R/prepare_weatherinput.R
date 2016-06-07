
# Libraries ---------------------------------------------------------------
library(dplyr)
library(reshape2)
library(magrittr)
library(lubridate)
library(data.table)


# Initiate progress bar for writing MGT files ----------------------------
print("Read and prepare input data:")
prgr_bar <- txtProgressBar(min = 0, max = 100, initial = 0, style = 3)

# Read weather inputs ----------------------------------------------------
# Read the min/max tempeartures for the stations:
temp_data <- read_weather(txtIO_pth%/%"Tmp1.Tmp", "XXX.X")
setTxtProgressBar(prgr_bar, 30)

# Read the precipitation data for the stations:
precip_data <- read_weather(txtIO_pth%/%"pcp1.pcp", "XXX.XX")
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
api_data <- (select(precip_data, starts_with("SUB")) -
              compute_ET0Hargreaves(temp_min, temp_max, lookup)) %>%
  apply(., 2, compute_AMC, c(0.8,0.85,0.90, 0.95), 5) %>%
  lapply(., cbind, precip_data[,1:4]) %>%
  lapply(., select,c(YEAR,MON,DAY,JDN,A,B,C,D))

rm(tmp_min, tmp_max)

weather_input <- list(Precipitation = precip_data,
                      Antecedent_Precip = api_data,
                      Temperature_Ind = temp_index)

# set and close progress bar
setTxtProgressBar(prgr_bar, 100)
Sys.sleep(1)
close(prgr_bar)

return(weather_input)
