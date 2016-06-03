## read.crop.lookup(plnt_dat) ---------------------------------------------
## Function to read the plant.dat file from the TxtInOut directory and to create
## the crop lookup table from this file requiered to convert crop labels to
## codes.
read.crop.lookup <- function (plnt_dat) {
  crop_lkp <- readLines(plnt_dat)
  keep_i   <- seq(1, length(crop_lkp), 5)
  crop_lkp <-  crop_lkp[keep_i]
  crop_lkp <- scan(text = crop_lkp, what = "", quiet = TRUE)
  crop_lkp <- t(matrix(data = crop_lkp, nrow = 3))
  crop_lkp <- data.frame(ICNUM = as.numeric(crop_lkp[,1]),
                         CPNM  = as.character(crop_lkp[,2]),
                         stringsAsFactors = FALSE)
  return(crop_lkp)
}
