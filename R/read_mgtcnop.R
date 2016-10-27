## read_mgtcnop(mgt_pth) -------------------------------------------------
read_mgtcnop <- function(mgt_pth){

  mgt_files <- list.files(mgt_pth, pattern = "mgt")
  name <- mgt_files %>%
    strsplit(., "\\.|\\_") %>%
    unlist(.) %>%
    .[. != "mgt" & . != "csv"]

  cnop_use = TRUE
  if(any(!file.exists(mgt_pth%//%"cnop"%_%name))){
    warning("At least one CNOP file does not exist! No CNOP values considered for this SWAT project!")
    cnop_use = FALSE
  }

  mgt_cnop <- list()
  fraction <- data.frame(NAME = numeric(),
                         FRAC = numeric())

  for(i in name){
    mgt <- read.csv(file = mgt_pth%//%"mgt"%_%i%.%"csv",
                     header = TRUE, sep = ",", stringsAsFactors = FALSE)
    if(cnop_use){
      cnop <- read.csv(file = mgt_pth%//%"cnop"%_%i%.%"csv",
                       header = TRUE, sep = ",", stringsAsFactors = FALSE)
    }
    fraction <- rbind(fraction,
                      data.frame(NAME = i,
                                 FRAC = as.numeric(
                                   mgt$MGT_1[mgt$OPERATION == "Fraction"]),
                                 stringsAsFactors = FALSE))
    mgt <- mgt[!mgt$OPERATION == "Fraction",]
    mgt_cnop[["mgt"%_%i]] <- mgt
    if(cnop_use){
      mgt_cnop[["cnop"%_%i]] <- cnop
    }
  }
  fraction$FRAC <- cumsum(fraction$FRAC) / sum(fraction$FRAC)
  mgt_cnop[["fraction"]] <- fraction
  return(mgt_cnop)
}
