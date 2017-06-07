library(tidyverse)
library(magrittr)
library(pasta)



# Read HRU data from txtinout folder ----------------------------------
txt_pth <- "D:/UnLoadC3/00_RB_SWAT/raab_sb4/Scenarios/Default/TxtInOut"

hru_list <- inquire_filenames(file_pattern = ".hru$",
                              file_path = txt_pth)
n_hru <- length(hru_list)

hru <- tibble(hru  = rep(NA_integer_, n_hru),
              sub  = rep(NA_integer_, n_hru),
              luse = rep(NA_character_, n_hru),
              soil = rep(NA_character_, n_hru),
              slp  = rep(NA_character_, n_hru),
              frct = rep(NA_real_, n_hru))

for(i_hru in 1:n_hru){
  hru_i <- readLines(txt_pth%//%hru_list[i_hru])
  hru_meta <- strsplit(hru_i[1], "\\ |\\:") %>% unlist()
  hru[i_hru, 1] <- as.numeric(hru_meta[6])
  hru[i_hru, 2] <- as.numeric(hru_meta[8])
  hru[i_hru, 3] <- hru_meta[12]
  hru[i_hru, 4] <- hru_meta[15]
  hru[i_hru, 5] <- hru_meta[18]
  hru[i_hru, 6] <- as.numeric(substr(hru_i[2], 1, 16))
}

# Read and modify luc_tbl ---------------------------------------------
luc_tbl <- read_csv("D:/Projects_R/SWATfarmR/data/luc_tbl.csv")
sub_all <- unique(hru$sub)
sol_all <- "'"%&%unique(hru$soil)%&%"'"
slp_all <- "'"%&%unique(hru$slp)%&%"'"

luc_tbl %<>%
  mutate(sub =   ifelse(is.na(sub),
                        "c("%&%paste(sub_all, collapse = ",")%&%")",
                        sub),
         soil =  ifelse(is.na(soil),
                       "c("%&%paste(sol_all, collapse = ",")%&%")",
                       soil),
         slope = ifelse(is.na(slope),
                        "c("%&%paste(slp_all, collapse = ",")%&%")",
                        slope))

# Build lup table -----------------------------------------------------
yr_min <- min(luc_tbl$year_from)
yr_max <- max(luc_tbl$year_to)
n_yr <- yr_max - yr_min + 1

lup <- matrix(data = hru$frct, ncol = n_yr, nrow = n_hru) %>%
  as_tibble() %>%
  set_colnames(min(luc_tbl$year_from):max(luc_tbl$year_to))



# Testing one step of land use change ---------------------------------
for(i_luc in 1:nrow(luc_tbl)){
  hru_from <- hru %>%
    filter(luse ==   luc_tbl$luse_from[i_luc]) %>%
    filter(sub   %in% eval(parse(text = luc_tbl$sub[i_luc]))) %>%
    filter(soil  %in% eval(parse(text = luc_tbl$soil[i_luc]))) %>%
    filter(slp   %in% eval(parse(text = luc_tbl$slope[i_luc]))) %>%
    mutate(frct_mod = frct * luc_tbl$fraction[i_luc])

  hru_to   <- hru %>%
    filter(luse %in% luc_tbl$luse_to[i_luc]) %>%
    filter(sub   %in% eval(parse(text = luc_tbl$sub[i_luc]))) %>%
    filter(soil  %in% eval(parse(text = luc_tbl$soil[i_luc]))) %>%
    filter(slp   %in% eval(parse(text = luc_tbl$slope[i_luc]))) %>%
    full_join(., hru_from %>% select(sub, soil, slp, frct_mod),
              by = c("sub", "soil", "slp"))

  hru_res <- hru_to %>%
    filter(is.na(frct)) %>%
    group_by(sub) %>%
    summarise(frct_res = sum(frct_mod)) %>%
    left_join(hru_to %>% filter(!is.na(frct)) %>% count(sub), by = "sub") %>%
    mutate(frct_res = frct_res/n) %>%
    select(sub, frct_res)

  hru_from %<>%
    right_join(hru %>% select(hru), by = "hru") %>%
    mutate(frct_mod = ifelse(is.na(frct_mod), 0, frct_mod)) %>%
    select(frct_mod)

  hru_to %<>%
    filter(!is.na(frct)) %>%
    left_join(hru_res, by = "sub") %>%
    mutate(frct_res = ifelse(is.na(frct_res), 0, frct_res),
           frct_mod = frct_mod + frct_res) %>%
    right_join(hru %>% select(hru), by = "hru") %>%
    mutate(frct_mod = ifelse(is.na(frct_mod), 0, frct_mod)) %>%
    select(frct_mod)

  luc_ramp <- c(rep(0, (luc_tbl$year_from[i_luc] - yr_min)),
                seq(0, 1, length.out = (luc_tbl$year_to[i_luc] -
                                        luc_tbl$year_from[i_luc] + 2))[-1],
                rep(1, (yr_max - luc_tbl$year_to[i_luc])))

  hru_decr <- matrix(data = luc_ramp, ncol = n_hru, nrow = n_yr) %>%
    t() %>%
    as_tibble() %>%
    set_colnames(min(luc_tbl$year_from):max(luc_tbl$year_to))

  hru_incr <- hru_decr * hru_to[[1]]
  hru_decr <- hru_decr * hru_from[[1]]

  lup <- lup + hru_incr - hru_decr
}
