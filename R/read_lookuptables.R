## read_lookuptables(txtIO_pth) ------------------------------------------
read_lookuptables <- function(txtIO_pth){
  lookup <- list()
  lookup[["management"]] <- data.frame(OPNUM = seq(0,17),
                                       OP = c("End of year",
                                              "Plant",
                                              "Irrigation",
                                              "Fertilizer",
                                              "Pesticide",
                                              "Harvest & Kill",
                                              "Tillage",
                                              "Harvest only",
                                              "Kill only",
                                              "Grazing",
                                              "Auto irrigation",
                                              "Auto fertilization",
                                              "Street sweeping",
                                              "Release/impound",
                                              "Cont/Fert",
                                              "Cont/Pest",
                                              "Burn",
                                              "Skip"))

  lookup[["fertilizer"]] <- txtIO_pth%/%"fert.dat" %>%
    read.table(file = ., stringsAsFactors = FALSE) %>%
    .[,1:2] %>%
    rename_col(.,c("IFNUM", "FERTNM"))

  lookup[["tillage"]] <- txtIO_pth%/%"till.dat" %>%
    read.fwf(file = ., widths = c(4,12,53), stringsAsFactors = FALSE) %>%
    .[,1:2] %>%
    rename_col(.,c("ITNUM", "TILLNM")) %>%
    trim_col(.,2)

  lookup[["crop"]] <- read_croplookup(txtIO_pth, "plant.dat")

  lookup[["station"]] <- read_weatherlookup(txtIO_pth)
  lookup[["n_subbasin"]] <- dim(lookup[["station"]])[1]

  lookup[["n_years"]] <- txtIO_pth%/%"file.cio" %>%
    readLines(.) %>%
    .[8] %>%
    scan(text = ., what = "", quiet = TRUE) %>%
    .[1] %>%
    as.numeric(.)

  lookup[["bound_yrs"]] <- txtIO_pth%/%"file.cio" %>%
    readLines(.) %>%
    .[9] %>%
    scan(text = ., what = "", quiet = TRUE) %>%
    .[1] %>%
    as.numeric(.)

  lookup[["bound_yrs"]] <- c(lookup[["bound_yrs"]],
                             lookup[["bound_yrs"]] + lookup[["n_years"]] - 1)

  return(lookup)
}



## Subfunctions -----------------------------------------------------------
## read_weatherlookup()
## Function to create the station lookup table for the subbasins found in the
## TxtIO directory. For running the function TxtIO must be set as working
## directory. The output is a data.frame holding the corresponding pcp and tmp
## stations to each subbasin.
read_weatherlookup <- function(txtIO_pth) {
  sub_list <- file.names(file_path = txtIO_pth, pat_str = "\\.sub$")
  stat_lookup <- data.frame(SUB   = character(),
                            I_PCP = numeric(),
                            I_TMP = numeric(),
                            I_LAT = numeric())
  for (i in sub_list){
    temp  <- readLines(sub_path%/%i, warn = FALSE)
    i_sub <- scan(text = temp[1], what = "", quiet = TRUE)[4]
    i_pcp <- scan(text = temp[7], what = "", quiet = TRUE)[1]
    i_tmp <- scan(text = temp[8], what = "", quiet = TRUE)[1]
    i_lat <- scan(text = temp[5], what = "", quiet = TRUE)[1]

    stat_lookup <- rbind(stat_lookup,
                         data.frame(SUB   = as.numeric(i_sub),
                                    I_PCP = as.numeric(i_pcp),
                                    I_TMP = as.numeric(i_tmp),
                                    I_LAT = as.numeric(i_lat)))
  }
  return(stat_lookup)
}

## read_croplookup(plnt_dat)
## Function to read the plant.dat file from the TxtInOut directory and to create
## the crop lookup table from this file requiered to convert crop labels to
## codes.
read_croplookup <- function (txtIO_pth, plant_file) {
  plant_pth <- txtIO_pth%/%"plant.dat"
  crop_lkp <- readLines(plant_pth)
  keep_i   <- seq(1, length(crop_lkp), 5)
  crop_lkp <-  crop_lkp[keep_i]
  crop_lkp <- scan(text = crop_lkp, what = "", quiet = TRUE)
  crop_lkp <- t(matrix(data = crop_lkp, nrow = 3))
  crop_lkp <- data.frame(ICNUM = as.numeric(crop_lkp[,1]),
                         CPNM  = as.character(crop_lkp[,2]),
                         stringsAsFactors = FALSE)
  return(crop_lkp)
}

## trim_col(df, col_nr)
## Function returns columns holding character strings w/o leading or
## trailing whitespace
trim_col <- function (df, col_nr){
  trim <- function(chr) gsub("^\\s+|\\s+$", "", chr)
  if(length(col_nr) == 0) stop("No columns selected!")
  if(length(col_nr) == 1){
    df[,col_nr] <- trim(df[,col_nr])
  } else {
    df[,col_nr] <-  apply(df[,col_nr], 2, trim)
  }
  return(df)
}


