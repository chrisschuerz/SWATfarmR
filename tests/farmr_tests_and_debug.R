library(SWATfarmR)
library(tidyverse)

logit_wgt <- function(x, lwr, upr) {
  k   <- 9.19024 / (upr - lwr)
  x_0 <- upr/2 + lwr/2
  1 / (1 + exp(-k*(x - x_0)))
}

lin_wgt <- function(x, lw_bd, up_bd) {
  (x >= up_bd) + (x < up_bd & x > lw_bd) * (x - lw_bd) / (up_bd - lw_bd)
}

pth <- "D:/_Projekte_/NitroClimAT/02_swat_setup/Scenarios/Default/TxtInOut"
pth_c <- "C:/TxtInOut"
mgt_file <- "D:/_Projekte_/NitroClimAT/01_data/fertilizer/mgt_sub_fert_zaya.csv"
new_farmr("zaya", pth)

load_farmr("D:/_Projekte_/NitroClimAT/02_swat_setup/Scenarios/Default/TxtInOut/zaya.farm")
zaya$read_management(file = mgt_file)
zaya$schedule_management_operations()
zaya$write_mgt_files(write_all = TRUE,
                     start_year = 1996,
                     end_year = 2005)
zaya$reset_mgt_files()


path <- zaya$.data$meta$project_path
mgt_raw <- zaya$.data$meta$mgt_raw
schedule  <- zaya$.data$scheduled_operations$scheduled_operations
variable <- zaya$.data$variables[[1]]
start_year = NULL
end_year = NULL
write_all = TRUE
write_operation(path = zaya$.data$meta$project_path,
                mgt_raw = zaya$.data$meta$mgt_raw,
                schedule = zaya$.data$scheduled_operations$scheduled_operations,
                variable = zaya$.data$variables[[1]],
                write_all = TRUE,
                start_year = 1996,
                end_year = 2005)
hru_attribute <- zaya$.data$meta$hru_attributes
mgt_schedule <- zaya$.data$management$mgt_codes
lookup <- zaya$.data$meta$parameter_lookup
variables <- zaya$.data$variables

idx_frsd <- hru_attribute$hru[hru_attribute$luse == "FRSD"]
mgt_list <- list.files(path = pth, pattern = "[:0-9:].mgt")[idx_frsd]
mgt_files <- map(pth%//%mgt_list, read_lines)
mgt_files <- map(mgt_files, ~c(.x[1:4],
                               "               7    | PLANT_ID: Land cover ID number (IGRO = 1)",
                               "               2    | LAI_INIT: Initial leaf are index (IGRO = 1)",
                               "            1000    | BIO_INIT: Initial biomass (kg/ha) (IGRO = 1)",
                               "            2640    | PHU_PLT: Number of heat units to bring plant to maturity (IGRO = 1)",
                               .x[9:31]))


walk2(mgt_list, mgt_files, ~write_lines(.y, pth%//%.x))
walk2(mgt_list, mgt_files, ~write_lines(.y, pth_c%//%.x))

luse_name <- unique(hru_attribute$luse)
luse_1 <- luse_name[c(2,4,5,6,9:12)]
luse_2 <- luse_name[c(7,8)]
luse_4 <- "FRSD"

idx_1 <- hru_attribute$hru[hru_attribute$luse %in% luse_1]
idx_2 <- hru_attribute$hru[hru_attribute$luse %in% luse_2]
idx_4 <- hru_attribute$hru[hru_attribute$luse  ==  "FRSD"]

hru_list <- list.files(path = pth, pattern = "[:0-9:].hru")
hru_files <- map(pth%//%hru_list, read_lines)
interc <- rep(0, length(mgt_list))
interc[idx_1] <- 1
interc[idx_2] <- 2
interc[idx_4] <- 4

hru_files <- map2(hru_files, interc, ~c(.x[1:8],
                               paste0("           ", .y,".000    | CANMX : Maximum canopy storage [mm]"),
                               .x[10:45]))
# schedule_operation(mgt_schedule, hru_attribute, variables, lookup)
walk2(hru_list, hru_files, ~write_lines(.y, pth%//%.x))
