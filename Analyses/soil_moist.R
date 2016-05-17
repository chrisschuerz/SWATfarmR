require(ggplot2)
require(plotly)
require(dygraphs)
require(xts)
require(nls2)
require(magrittr)


setwd("D:/Projects_R/Altm_2016/Scenarios/Default/TxtInOut")

hru_out <- read.fortran(file = "output.hru", format = c("A4", "I5", "I10", "3I5", "F10", "F9"), skip = 9)
colnames(hru_out) <- c("Luse", "HRU", "GIS", "Sub", "MGT", "JDN", "Area", "SW")

hru_out$Date <- seq(as.Date("2003-01-01"), as.Date("2005-12-31"), by = "day")
hru_out$Ind <- seq(1, dim(hru_out)[1])

hru_out %<>% filter(Date >= as.Date("2003-01-01"), Date <= as.Date("2005-12-31"))

spln_fun <- smooth.spline(x = hru_out$Ind, y = hru_out$SW, tol = 5)
smth_fun <- nls(y ~ A*cos(omega*t+phi)+C, data=data.frame(t = hru_out$Ind,y = hru_out$SW), start=list(A=20,omega=pi/20,phi=0.1,C=550))
pred_smth <- predict(smth_fun, 1:365)
pred_spln <- predict(spln_fun, 1:365)


plot(hru_out$Ind, hru_out$SW, type = "l")
lines(pred_smth)
lines(pred_spln)


SW_short <- data.frame(Date = hru_out$Date,
                       SW = hru_out$SW - 550)

hru_dat <- pcp_dat %>%
  mutate(Date = as.Date(paste(YEAR, MON, DAY, sep = "-")),
         Precip = SUB_017) %>%
  select(Date, Precip) %>%
  filter(Date >= as.Date("2003-01-01"), Date <= as.Date("2005-12-31")) %>%
  mutate(Precip = ifelse(Precip < 2, 0, Precip)) %>%
  left_join(SW_short) %>%
  mutate(SW = SW)



plot <-
  ggplot(data = hru_dat) +
  geom_bar(aes(Date, Precip), stat = "Identity", col = "royalblue") +
  geom_line(aes(Date, SW), col = "red")

ggplotly(plot)

dygraph(SW_xts)
