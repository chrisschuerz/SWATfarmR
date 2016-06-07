## format_mgtschedule(sdl_df, meta_data, n_yrs) -----------------------------
## Function takes the management schedule file and prepares it for writing the
## the operations to the management file.
format_mgtschedule <- function(input_lst, meta_lst, mgtcnop_sel) {

  sdl <- input_lst$mgt_cnop[["mgt"%_%mgtcnop_sel]] %>%
    filter(CROP == meta_lst$LUSE)
  sdl[sdl == ""] <- NA
  n_op <- dim(sdl)[1]

  sdl <- sdl[rep(seq(1, n_op), input_lst$lookup$n_years),]
  sdl %<>%  mutate(.,
                        YEAR = seq(input_lst$lookup$bound_yrs[1],
                                   input_lst$lookup$bound_yrs[2]) %>%
                          rep(. , each = n_op),
                        JDN1 = paste(YEAR,
                                     MON_1 %>% sprintf("%02d", .),
                                     DAY_1 %>% sprintf("%02d", .),
                                     sep = "") %>%
                          as.Date(.,"%Y%m%d") %>%
                          yday(.),
                        JDN2 = paste(YEAR,
                                     MON_2 %>% sprintf("%02d", .),
                                     DAY_2 %>% sprintf("%02d", .),
                                     sep = "") %>%
                          as.Date(.,"%Y%m%d") %>%
                          yday(.)) %>%
    select(., YEAR, JDN1, JDN2, MON_1, MON_2,
           starts_with("OP"))

  sdl <- rbind(filter(sdl, OPERATION == "Initial crop")[1,],
                    filter(sdl, OPERATION != "Initial crop"))
  sdl <- filter(sdl, !is.na(OPERATION))

  return(sdl)
}
