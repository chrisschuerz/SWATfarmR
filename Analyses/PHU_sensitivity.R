# Libraries ---------------------------------------------------------------
require(purrr)
require(reshape2)
require(ggplot2)
require(lubridate)
require(dplyr)
require(magrittr)
require(data.table)
require(tidyr)
require(gridExtra)


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
tmp_dat <- read.weather_input(txtIO_pth%/%"Tmp1.Tmp", "XXX.X")


# Mutate TMP data ---------------------------------------------------------
tmp_mean <- cbind(tmp_dat[,1],
                  (tmp_dat[,seq(2, dim(tmp_dat)[2], 2)] +
                   tmp_dat[,seq(3, dim(tmp_dat)[2], 2)])/2) %>%
  rename.col(., lbl = c("YYYYJDN", "stat"%_%seq(1,12))) %>%
  mutate(date = as.Date(as.character(YYYYJDN), "%Y%j"),
         year = substr(as.character(YYYYJDN), 1, 4)) %>%
  select(-YYYYJDN)

phu0_acc <- tmp_mean %>%
  group_by(year) %>%
  mutate_each(., funs(ifelse(. < 0, 0, .)), starts_with("stat")) %>%
  mutate_each(., funs(cumsum), starts_with("stat")) %>%
  mutate(day = substr(date, 9, 10),
         mon = substr(date, 6, 7),
         date = as.Date("2016"%-%mon%-%day)) %>%
  select(-mon, -day) %>%
  melt(id = c("date", "year"))

phu_tot <- phu0_acc %>%
  ungroup %>%
  group_by(year, variable) %>%
  summarise(value = max(value)) %>%
  ungroup %>%
  summarise(mean(value))

phu0_fract <- phu0_acc  %>%
  mutate(phu0 = value/phu_tot$`mean(value)`) %>%
  select(-value)

phu_corn <- 900
tbase_corn <- 8

f_plant <- 0.078

phu <- tmp_mean %>%
  group_by(year) %>%
  mutate_each(., funs(. - tbase_corn), starts_with("stat")) %>%
  mutate_each(., funs(ifelse(. < 0, 0, .)), starts_with("stat")) %>%
  mutate(day = substr(date, 9, 10),
         mon = substr(date, 6, 7),
         date = as.Date("2016"%-%mon%-%day)) %>%
  select(-mon, -day) %>%
  melt(id = c("date", "year"))

phu_fract <- phu  %>%
  mutate(phu = value/phu_corn) %>%
  select(-value)

phu_corn <- phu0_fract %>%
  mutate(phu_plant = ifelse(phu0 <= f_plant, phu0, NA)) %>%
  cbind.data.frame(., phu = phu_fract$phu) %>%
  mutate(phu_grow = ifelse(is.na(phu_plant), phu, 0)) %>%
  group_by(year, variable) %>%
  mutate(phu_grow = cumsum(phu_grow),
         phu_grow = ifelse(phu_grow < 1.15, phu_grow, NA),
         stat = variable) %>%
  ungroup %>%
  mutate(
    # phu_fall = ifelse(is.na(phu_grow), phu0, NA),
         phu_grow = ifelse(phu_grow == 0, NA, phu_grow)) %>%
  select(-phu0, -phu, -variable) %>%
  melt(id = c("date", "year", "stat")) %>%
  mutate(col_ind = variable%_%year)



# Plot PHU0 ---------------------------------------------------------------
p_acc <- ggplot(phu0_acc) +
  geom_line(aes(x = date, y = value, col = year, lty = variable), lwd = 0.1) +
  scale_color_grey() +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme_bw(base_size = 8) +
  theme(legend.position="none") +
  xlab("Date") + ylab("accumulated PHU / °C")

# Plot PHU corn------------------------------------------------------------
col_fallow <- colorRampPalette(c("lightblue4", "lightblue1"))(45)
col_grow <- colorRampPalette(c("coral4", "coral1"))(45)
col_pal <- c(col_grow, col_fallow)

p_corn <- ggplot(phu_corn) +
  geom_line(aes(x = date, y = value, lty = stat, col = col_ind), lwd = 0.1) +
  scale_color_manual(values = col_pal) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme_bw(base_size = 8) +
  theme(legend.position="none") +
  xlab("Date") + ylab("PHU fraction count in SWAT/ -")

phu_plot <- grid.arrange(p_acc, p_corn, ncol = 2)

ggsave(filename = "phu_plot.svg", plot = phu_plot, device = "svg",
       width = 150, height = 70, units = "mm", scale = 1.3)


