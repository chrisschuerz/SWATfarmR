mgt_pth <- "C:/SWAT_IL/mgt_cnop"
txt_pth <- "C:/SWAT_IL/TxtInOut"

input_il <- prepare_inputdata(txtIO_pth = txt_pth,
                              mgt_pth = mgt_pth,
                              ant_days = 5)

write_mgtfiles(input_il, txt_pth, precip_thrs = c(2,10,25), days_random = c(3,10),
               day_ssp = 3, select_type = "norm5")
