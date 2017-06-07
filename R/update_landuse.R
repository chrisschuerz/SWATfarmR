library(tidyverse)
library(magrittr)
library(pasta)


luc_tbl <- read_csv("D:/Projects_R/SWATfarmR/data/luc_tbl.csv")

txt_pth <- "D:/UnLoadC3/00_RB_SWAT/raab_sb4/Scenarios/Default/TxtInOut"

hru_list <- inquire_filenames(file_pattern = ".hru$",
                              file_path = txt_pth)

hru <- tibble(hru  = rep(NA_integer_, length(hru_list)),
              sub  = rep(NA_integer_, length(hru_list)),
              luse = rep(NA_character_, length(hru_list)),
              soil = rep(NA_character_, length(hru_list)),
              slp  = rep(NA_character_, length(hru_list)),
              frct = rep(NA_real_, length(hru_list)))

for(i_hru in 1:length(hru_list)){
  hru_i <- readLines(txt_pth%//%hru_list[i_hru])
  hru_meta <- strsplit(hru_i[1], "\\ |\\:") %>% unlist()
  hru[i_hru, 1] <- as.numeric(hru_meta[6])
  hru[i_hru, 2] <- as.numeric(hru_meta[8])
  hru[i_hru, 3] <- hru_meta[12]
  hru[i_hru, 4] <- hru_meta[15]
  hru[i_hru, 5] <- hru_meta[18]
  hru[i_hru, 6] <- as.numeric(substr(hru_i[2], 1, 16))
}

frct_chg <- 0.1

lup <- hru %>%
  mutate(hru_init = frct,
         hru_updt = NA) %>%
  select(hru, hru_init, hru_updt)

hru_decr <- hru %>%
  select(hru) %>%
  mutate(frct_decr = 0)

hru_incr <- hru %>%
  select(hru) %>%
  mutate(frct_incr = 0)

hru_from <- hru %>%
  filter(luse %in% "FESC") %>%
  mutate(frct_mod = frct * frct_chg)

hru_to   <- hru %>%
  filter(luse %in% "CORN") %>%
  right_join(., hru_from %>% select(sub, soil, slp, frct_mod),
             by = c("sub", "soil", "slp"))

hru_res <- hru_to %>%
  filter(is.na(frct)) %>%
  group_by(sub) %>%
  summarise(frct_res = sum(frct_mod)) %>%
  left_join(hru_to %>% filter(!is.na(frct)) %>% count(sub), by = "sub") %>%
  mutate(frct_res = frct_res/n) %>%
  select(sub, frct_res)

hru_to %<>%
  filter(!is.na(frct)) %>%
  left_join(hru_res, by = "sub") %>%
  mutate(frct_res = ifelse(is.na(frct_res), 0, frct_res),
         frct_mod = frct_mod + frct_res)

hru_decr %<>%
  left_join(hru_from, by = "hru") %>%
  mutate(frct_mod  = ifelse(is.na(frct_mod), 0, frct_mod),
         frct_decr = frct_decr + frct_mod) %>%
  select(hru, frct_decr)

hru_incr %<>%
  left_join(hru_to, by = "hru") %>%
  mutate(frct_mod  = ifelse(is.na(frct_mod), 0, frct_mod),
         frct_incr = frct_incr + frct_mod) %>%
  select(hru, frct_incr)

lup$hru_updt <- lup$hru_init + hru_incr$frct_incr - hru_decr$frct_decr
