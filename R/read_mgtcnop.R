## read_mgtcnop(mgt_pth) -------------------------------------------------
read_mgtcnop <- function(mgt_pth){

  mgt_files <- list.files(mgt_pth, pattern = "mgt")
  name <- mgt_files %>%
    strsplit(., "\\.|\\_") %>%
    unlist(.) %>%
    .[. != "mgt" & . != "csv"]

  mgt_cnop <- list()
  fraction <- data.frame(NAME = numeric(),
                         FRAC = numeric())

  for(i in name){
    mgt <- read.csv(file = mgt_pth%/%"mgt"%_%i%.%"csv",
                     header = TRUE, sep = ",", stringsAsFactors = FALSE)
    cnop <- read.csv(file = mgt_pth%/%"cnop"%_%i%.%"csv",
                     header = TRUE, sep = ",", stringsAsFactors = FALSE)
    fraction <- rbind(fraction,
                      data.frame(NAME = name,
                                 FRAC = as.numeric(
                                   mgt$OP_TYPE[mgt$OPERATION == "Fraction"]),
                                 stringsAsFactors = FALSE))
    mgt <- mgt[!mgt$OPERATION == "Fraction",]
    mgt_cnop[["mgt"%_%i]] <- mgt
    mgt_cnop[["cnop"%_%i]] <- cnop
  }
  fraction$FRAC <- cumsum(fraction$FRAC) / sum(fraction$FRAC)
  mgt_cnop[["fraction"]] <- fraction
  return(mgt_cnop)
}
