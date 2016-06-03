## read.mgmt.cnop(mgmt_pth) -----------------------------------------------
read.mgmt.cnop <- function(mgmt_pth){

  mgmt_files <- list.files(mgmt_pth, pattern = "mgmt")
  name <- mgmt_files %>%
    strsplit(., "\\.|\\_") %>%
    unlist(.) %>%
    .[. != "mgmt" & . != "csv"]

  mgmt_cnop <- list()
  fraction <- data.frame(NAME = numeric(),
                         FRAC = numeric())

  for(i in name){
    mgmt <- read.csv(file = mgmt_pth%/%"mgmt"%_%i%.%"csv",
                     header = TRUE, sep = ",", stringsAsFactors = FALSE)
    cnop <- read.csv(file = mgmt_pth%/%"cnop"%_%i%.%"csv",
                     header = TRUE, sep = ",", stringsAsFactors = FALSE)
    fraction <- rbind(fraction,
                      data.frame(NAME = name,
                                 FRAC = as.numeric(
                                   mgmt$OP_TYPE[mgmt$OPERATION == "Fraction"]),
                                 stringsAsFactors = FALSE))
    mgmt <- mgmt[!mgmt$OPERATION == "Fraction",]
    mgmt_cnop[["mgmt"%_%i]] <- mgmt
    mgmt_cnop[["cnop"%_%i]] <- cnop
  }
  fraction$FRAC <- cumsum(fraction$FRAC) / sum(fraction$FRAC)
  mgmt_cnop[["fraction"]] <- fraction
  return(mgmt_cnop)
}
