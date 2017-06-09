#' Create progressive land use update (LUP) files for a SWAT project
#'
#' @param lup_tbl Either a data.frame or the path to a .csv file that
#'   provides the information for the progressive land use changes. See the
#'   examples below.
#' @param hru_pth Path to the txtInOut folder of the project for which the
#'   LUP files should be created.
#'
#' @return Writes the lup.dat file and the lup input files for the time
#'   period given in the lup_tbl into the txtInOut project folder.
#' @importFrom dplyr mutate select filter full_join left_join right_join
#'   group_by ungroup summarise count
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr %>% %<>% set_colnames
#' @importFrom pasta %&% %//%
#' @export
#'
#' @examples
#' # To get a template for the lup_tbl
#'
#' write_luctemp(choose.dir()) # only windows
#' write_luctemp() #Returns tibble to .Globalenv

update_landuse <- function(lup_tbl, hru_pth) {
  # Read HRU data from txtinout folder ----------------------------------

  substr_right <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  if (substr_right(hru_pth, 4) == ".csv"){
    hru <- read.csv(hru_path) %>%
      mutate(frct = ARSLP/ARSUB) %>%
      rename(hru  = HRU_ID,
             sub  = SUBBASIN,
             luse = LANDUSE,
             soil = SOIL,
             slp  = SLP) %>%
      select(hru, sub, luse, soil, slp, frct)

  } else {
    hru_list <- inquire_filenames(file_pattern = ".hru$",
                                  file_path = hru_pth)
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
  }


  # Read and modify lup_tbl ---------------------------------------------
  sub_all <- unique(hru$sub)
  sol_all <- "'"%&%unique(hru$soil)%&%"'"
  slp_all <- "'"%&%unique(hru$slp)%&%"'"

  lup_tbl %<>%
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
  yr_min <- min(lup_tbl$year_from)
  yr_max <- max(lup_tbl$year_to)
  n_yr <- yr_max - yr_min + 1

  lup <- matrix(data = hru$frct, ncol = n_yr, nrow = n_hru) %>%
    as_tibble() %>%
    set_colnames(min(lup_tbl$year_from):max(lup_tbl$year_to))



  # calculating updated HRU fractions -----------------------------------
  for(i_luc in 1:nrow(lup_tbl)){
    hru_from <- hru %>%
      filter(luse == lup_tbl$luse_from[i_luc]) %>%
      filter(sub   %in% eval(parse(text = lup_tbl$sub[i_luc]))) %>%
      filter(soil  %in% eval(parse(text = lup_tbl$soil[i_luc]))) %>%
      filter(slp   %in% eval(parse(text = lup_tbl$slope[i_luc]))) %>%
      mutate(frct_mod = frct * lup_tbl$fraction[i_luc])

    hru_to   <- hru %>%
      filter(luse == lup_tbl$luse_to[i_luc]) %>%
      filter(sub   %in% eval(parse(text = lup_tbl$sub[i_luc]))) %>%
      filter(soil  %in% eval(parse(text = lup_tbl$soil[i_luc]))) %>%
      filter(slp   %in% eval(parse(text = lup_tbl$slope[i_luc]))) %>%
      full_join(., hru_from %>% select(sub, soil, slp, frct_mod),
                by = c("sub", "soil", "slp")) %>%
      filter(!is.na(frct))

    frct_sum <- hru_from %>%
      group_by(sub) %>%
      summarise(frct_mod = sum(frct_mod)) %>%
      full_join(., hru_to %>%
                     group_by(sub) %>%
                     summarise(frct_mod = sum(frct_mod)),
                by = "sub") %>%
      mutate(corr_fct = frct_mod.x/frct_mod.y) %>%
      select(sub,corr_fct)

    hru_from %<>%
      left_join(frct_sum, by = "sub") %>%
      mutate(frct_mod = ifelse(is.na(corr_fct), 0, frct_mod)) %>%
      right_join(hru %>% select(hru), by = "hru") %>%
      mutate(frct_mod = ifelse(is.na(frct_mod), 0, frct_mod)) %>%
      select(frct_mod)

    hru_to %<>%
      left_join(frct_sum, by = "sub") %>%
      mutate(frct_mod = ifelse(is.na(corr_fct), 0, frct_mod*corr_fct)) %>%
      right_join(hru %>% select(hru), by = "hru") %>%
      mutate(frct_mod = ifelse(is.na(frct_mod), 0, frct_mod)) %>%
      select(frct_mod)

    luc_ramp <- c(rep(0, (lup_tbl$year_from[i_luc] - yr_min)),
                  seq(0, 1, length.out = (lup_tbl$year_to[i_luc] -
                                          lup_tbl$year_from[i_luc] + 2))[-1],
                  rep(1, (yr_max - lup_tbl$year_to[i_luc])))

    hru_decr <- matrix(data = luc_ramp, ncol = n_hru, nrow = n_yr) %>%
      t() %>%
      as_tibble() %>%
      set_colnames(min(lup_tbl$year_from):max(lup_tbl$year_to))

    hru_incr <- hru_decr * hru_to[[1]]
    hru_decr <- hru_decr * hru_from[[1]]

    lup <- lup + hru_incr - hru_decr
  }
  lup[lup[,] <= 1e-6] <- 1e-6
  # Write LUP input files for SWAT model to txtIO_path ------------------
  lup_dat <- tibble(idx  = sprintf("%5d", 1:ncol(lup)),
                    day  = sprintf("%4d", rep(1,ncol(lup))),
                    mon  = day,
                    year = sprintf("%4d", yr_min:yr_max),
                    file = sprintf("%13s", "lupin"%&% yr_min:yr_max%.%"dat"))

  write.table(x = lup_dat, file = hru_pth%//%"lup.dat",
              quote = FALSE, col.names = FALSE, row.names = FALSE)

  for(i_yr in yr_min:yr_max){
    lup_tmp <- tibble(hru = sprintf("%5d", hru$hru),
                      lup = round(lup[[as.character(i_yr)]], digits = 6) %>%
                            sprintf("%.6f", .))

    write.table(x = lup_tmp, file = hru_pth%//%"lupin"%&%i_yr%.%"dat",
                quote = FALSE, row.names = FALSE)
  }
}




