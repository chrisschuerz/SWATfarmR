# Libraries ---------------------------------------------------------------
require(purrr)
require(reshape2)
require(ggplot2)
require(lubridate)
require(dplyr)
require(magrittr)
require(data.table)
require(tidyr)


# functions ---------------------------------------------------------------
## read.weather_input(file_str, format_int) -------------------------------
## Function reads the wheater data given in ascii format. The delimiter
## positions are given by the format integers: (length date, length values,
## variables per station).
read.weather_input <- function(file_name, val_format) {

  sub_num <- function(df, g1, g2){
    substr(df, g1 , g2) %>%
      as.numeric
  }

  dec_pos <- gregexpr("\\.", val_format)
  shift   <- c(dec_pos[[1]]-1, nchar(val_format) - dec_pos[[1]])
  date_strt <- 1
  date_end  <- 7
  df <- fread(file_name, skip = 4, sep = "\n", header = FALSE)
  g <- gregexpr("\\.", df[1])[[1]]
  g1 <- c(date_strt, g - shift[1])
  g2 <- c(date_end, g + shift[2])
  df %<>%
    mapply(sub_num,., g1, g2) %>%
    as.data.frame
  return(df)
}
## paste functions --------------------------------------------------------
'%_%' <- function(a, b) paste(a, b, sep = "_")
'%.%' <- function(a, b) paste(a, b, sep = ".")
'%/%' <- function(a, b) paste(a, b, sep = "/")
'%-%' <- function(a, b) paste(a, b, sep = "-")
## rename.col(df, lbl) ----------------------------------------------------
## Function takes data.frame and labels the columns.
rename.col <- function(df, lbl){
  colnames(df) <- lbl
  return(df)
}

# Read TMP data -----------------------------------------------------------
txtIO_pth <- "D:/Projects_R/Altm_2016/Scenarios/Default/TxtInOut"
lookup <- read.lookup_tables(txtIO_pth)
tmp_dat <- read.weather_input(txtIO_pth%/%"Tmp1.Tmp", "XXX.X")


# Mutate TMP data ---------------------------------------------------------
tmp_mean <- cbind(tmp_dat[,1],
                  (tmp_dat[,seq(2, dim(tmp_dat)[2], 2)] +
                   tmp_dat[,seq(3, dim(tmp_dat)[2], 2)])/2) %>%
  rename.col(., lbl = c("YYYYJDN", "stat"%_%seq(1,12))) %>%
  mutate(date = as.Date(as.character(YYYYJDN), "%Y%j"),
         year = substr(as.character(YYYYJDN), 1, 4)) %>%
  select(-YYYYJDN) %>%
  group_by(year) %>%
  mutate_each(., funs(ifelse(. < 0, 0, .)), starts_with("stat")) %>%
  mutate_each(., funs(cumsum), starts_with("stat")) %>%
  mutate(day = substr(date, 9, 10),
         mon = substr(date, 6, 7),
         date = as.Date("2016"%-%mon%-%day)) %>%
  select(-mon, -day)

phu_tot <- tmp_mean %>%
  summarise_each(., funs(max), starts_with("stat")) %>%
  ungroup %>%
  select(starts_with("stat")) %>%
  colMeans() %>%
  mean



# Plot PHU0 ---------------------------------------------------------------
p_acc <- ggplot(tmp_mean%>%
                  melt(id = c("date", "year"))) +
  geom_line(aes(x = date, y = value, col = year, lty = variable)) +
  scale_color_grey() +
  theme(legend.position="none") +
  scale_x_date(date_breaks = "1 month",date_labels = "%b %d")






