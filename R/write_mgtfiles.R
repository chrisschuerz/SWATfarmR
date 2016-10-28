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
library(doSNOW)

  ## Initiate progress bar for writing MGT files ----------------------------
  print("Rewrite management input files: Be patient! This may take a while :)")


  ## List all management and soil files in TxtIO ----------------------------
  hru_list <- inquire_filenames(file_path = txtIO_pth,
                                file_pattern = "\\.hru$")
  hru_list <- substr(hru_list, 1, 9)
  ## Get individual mgt files and edit them --------------------------------
  cl <- makeCluster(detectCores())
  registerDoSNOW(cl)

    prgr_bar <- txtProgressBar(max = length(hru_list), initial = 0, style = 3)
  progress <- function(n) setTxtProgressBar(prgr_bar, n)
  opts <- list(progress = progress)

  foreach (i_hru = 1:length(hru_list),
           .packages = c("dplyr", "lubridate", "magrittr", "reshape2"),
           .options.snow = opts)  %dopar% {

    mgt_i  <- readLines(txtIO_pth%//%hru_list[i_hru]%.%"mgt", warn = FALSE)
    soil_i <- readLines(txtIO_pth%//%hru_list[i_hru]%.%"sol", warn = FALSE)

    mgtcnop_sel <- select_mgtcnop(input$mgt_cnop)
    mgt_i_meta  <- inquire_HRUmeta(mgt_i, soil_i, input, mgtcnop_sel)
    mgt_i_sdl   <- format_mgtschedule(input, mgt_i_meta, mgtcnop_sel)

    n_op  <- dim(mgt_i_sdl)[1]
    n_rot <- length(which(mgt_i_sdl$OPERATION == "End of year"))
    mgt_i[29] <- paste(sprintf("%16i", n_rot),
                        "   | NROT: number of years of rotation")
    mgt_i <- mgt_i[1:30]

    mgt_op <- rep(NA, n_op)

    if(mgt_i_sdl[1,]$OPERATION == "Initial crop"){
      mgt_i <- init_crp(mgt_i, mgt_i_sdl, 1, input$lookup$crop)
      i_init <- 2
      mgt_i_sdl <- mgt_i_sdl[2:nrow(mgt_i_sdl),]
    } else {
      i_init <- 1
    }

    mgt_op <- apply(mgt_i_sdl, 1, select_opwrite, mgt_i,input, i, mgt_i_meta,
                    precip_thrs, days_random, day_ssp,
                    select_type)

    mgt_i <- c(mgt_i, mgt_op)
    ## Write files in TxtIO ---------------------------------------------------
    writeLines(mgt_i, con = txtIO_pth%//%hru_list[i_hru]%.%"mgt")
  }
  close(prgr_bar)
  stopCluster(cl)
}

      select_opwrite <- function(mgt_i_sdl, mgt_i,input, i, mgt_i_meta,
                                 precip_thrs, days_random, day_ssp,
                                 select_type) {
        switch (mgt_i_sdl[8],
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
