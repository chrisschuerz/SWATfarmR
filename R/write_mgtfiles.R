#' Write management dates for the SWAT project
#'
#' @param input
#' @param txtIO_pth
#' @param precip_thrs
#' @param days_random
#'
#' @return
#' @export


write_mgtfiles <- function(input, txtIO_pth,
                           precip_thrs = c(2, 10, 25),
                           days_random = c(0,0),
                           day_ssp = 3,
                           select_type = "unif"){

# Libraries ---------------------------------------------------------------
library(dplyr)
library(reshape2)
library(magrittr)
library(lubridate)

  ## Initiate progress bar for writing MGT files ----------------------------
  print("Rewrite management input files: Be patient! This may take a while :)")
  prgr_bar <- txtProgressBar(min = 0, max = 100, initial = 0, style = 3)


  ## List all management and soil files in TxtIO ----------------------------
  hru_list <- inquire_filenames(file_path = txtIO_pth,
                                file_pattern = "\\.hru$")
  hru_list <- substr(hru_list, 1, 9)
  ## Get individual mgt files and edit them --------------------------------
  for (i_hru in hru_list){
    mgt_i  <- readLines(txtIO_pth%/%i_hru%.%"mgt", warn = FALSE)
    soil_i <- readLines(txtIO_pth%/%i_hru%.%"sol", warn = FALSE)

    mgtcnop_sel <- select_mgtcnop(input$mgt_cnop)
    mgt_i_meta  <- inquire_HRUmeta(mgt_i, soil_i, input, mgtcnop_sel)
    mgt_i_sdl   <- format_mgtschedule(input, mgt_i_meta, mgtcnop_sel)

    n_op  <- dim(mgt_i_sdl)[1]
    n_rot <- length(which(mgt_i_sdl$OPERATION == "End of year"))
    mgt_i[29] <- paste(sprintf("%16i", n_rot),
                        "   | NROT: number of years of rotation")
    mgt_i <- mgt_i[1:30]

    for (i in 1:n_op){
      mgt_i <-   switch (mgt_i_sdl[i,]$OPERATION,
                          "Initial crop"   = init_crp(mgt_i,
                                                      mgt_i_sdl,
                                                      i,
                                                      input$lookup$crop),
                          "End of year"    = end_year(mgt_i,
                                                      mgt_i_sdl,
                                                      i,
                                                      input$lookup$management),
                          "Plant"          = plnt_crp(mgt_i,
                                                      mgt_i_sdl,
                                                      i,
                                                      mgt_i_meta,
                                                      input,
                                                      precip_thrs,
                                                      days_random,
                                                      day_ssp,
                                                      select_type),
                          "Fertilizer"     = fert_crp(mgt_i,
                                                      mgt_i_sdl,
                                                      i,
                                                      mgt_i_meta,
                                                      input,
                                                      precip_thrs,
                                                      days_random,
                                                      day_ssp,
                                                      select_type),
                          "Harvest & Kill" = hrv_kill(mgt_i,
                                                      mgt_i_sdl,
                                                      i,
                                                      mgt_i_meta,
                                                      input,
                                                      precip_thrs,
                                                      days_random,
                                                      day_ssp,
                                                      select_type),
                          "Tillage"        = till_op (mgt_i,
                                                      mgt_i_sdl,
                                                      i,
                                                      mgt_i_meta,
                                                      input,
                                                      precip_thrs,
                                                      days_random,
                                                      day_ssp,
                                                      select_type),
                          "Harvest only"   = hrv_only(mgt_i,
                                                      mgt_i_sdl,
                                                      i,
                                                      mgt_i_meta,
                                                      input,
                                                      precip_thrs,
                                                      days_random,
                                                      day_ssp,
                                                      select_type),
                          "Skip"           =     skip(mgt_i,
                                                      mgt_i_sdl,
                                                      i,
                                                      input$lookup$management)
      )
    }
    ## Write files in TxtIO ---------------------------------------------------
    writeLines(mgt_i, con = txtIO_pth%/%i_hru%.%"mgt")
    ## Update progress bar for writing MGT files ------------------------------
    i_prog <- which(i_hru == hru_list)/length(hru_list)
    setTxtProgressBar(prgr_bar, i_prog*100)
  }
  close(prgr_bar)
}
