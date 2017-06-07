library(tidyverse)
library(magrittr)

hru <- read_csv(file = "D:/UnLoadC3/00_RB_SWAT/UnLoad_framework/framework_setup/in_data/hrus_sb4.csv")

hru %<>% mutate(hru_frct = ARSLP/ARSUB)

hru1 <- hru %>%
  filter(SUBBASIN == 1, LANDUSE %in% "FESC")

lup <- hru %>%
  mutate(id       = HRU_ID,
         hru_init = hru_frct,
         hru_updt = NA) %>%
  select(id, hru_init, hru_updt)

