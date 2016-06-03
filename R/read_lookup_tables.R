## read.lookup_tables(txtIO_pth) ------------------------------------------
read.lookup_tables <- function(txtIO_pth){
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
    rename.col(.,c("IFNUM", "FERTNM"))

  lookup[["tillage"]] <- txtIO_pth%/%"till.dat" %>%
    read.fwf(file = ., widths = c(4,12,53), stringsAsFactors = FALSE) %>%
    .[,1:2] %>%
    rename.col(.,c("ITNUM", "TILLNM")) %>%
    trim.col(.,2)

  lookup[["crop"]] <- read.crop.lookup(txtIO_pth%/%"plant.dat")

  lookup[["station"]] <- sub.weater.lookup(txtIO_pth)
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
