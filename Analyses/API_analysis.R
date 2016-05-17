require(reshape2)
start_date <- as.Date("1998-01-01")
end_date <- as.Date("2000-12-31")

pcp <- pcp_dat %>%
  mutate(Date = as.Date(paste(YEAR,"-", MON, "-", DAY, sep = "")),
         pcp = SUB_001) %>%
  select(Date, pcp) %>%
  filter(Date >= start_date,
         Date <= end_date)


amc_dat <- apply(pcp_et_bal, 2, AMC.estimate, c(0.8,0.83,0.86, 0.9), 5)

for(i in 1:lookup$n_subbasin){
  amc_dat[[i]] <- cbind(pcp_dat[,1:4],amc_dat[[i]])
}

amc_5 <- amc_dat$SUB_001 %>%
  mutate(Date = as.Date(paste(YEAR,"-", MON, "-", DAY, sep = ""))) %>%
  select(Date, A, B, C, D) %>%
  filter(Date >= start_date,
         Date <= end_date) %>%
  melt(id = "Date")

amc_dat <- apply(pcp_et_bal, 2, AMC.estimate, c(0.8,0.83,0.86, 0.9), 10)

for(i in 1:lookup$n_subbasin){
  amc_dat[[i]] <- cbind(pcp_dat[,1:4],amc_dat[[i]])
}

amc_10 <- amc_dat$SUB_001 %>%
  mutate(Date = as.Date(paste(YEAR,"-", MON, "-", DAY, sep = ""))) %>%
  select(Date, A, B, C, D) %>%
  filter(Date >= start_date,
         Date <= end_date) %>%
  melt(id = "Date")

amc_dat <- apply(pcp_et_bal, 2, AMC.estimate, c(0.8,0.85,0.9, 0.95), 5)

for(i in 1:lookup$n_subbasin){
  amc_dat[[i]] <- cbind(pcp_dat[,1:4],amc_dat[[i]])
}
amc_5_95 <- amc_dat$SUB_001 %>%
  mutate(Date = as.Date(paste(YEAR,"-", MON, "-", DAY, sep = ""))) %>%
  select(Date, A, B, C, D) %>%
  filter(Date >= start_date,
         Date <= end_date) %>%
  melt(id = "Date")

amc_plot <- ggplot() +
  geom_bar(data = pcp, aes(x = Date, y = pcp), stat = "Identity") +
  geom_line(data = amc_5, aes(x = Date, y = value, col = variable)) +
  geom_line(data = amc_10, aes(x = Date, y = value, col = variable), lty = 2)

ggplotly(amc_plot)
