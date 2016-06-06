## format_mgtschedule(sdl_df, meta_data, n_yrs) -----------------------------
## Function takes the management schedule file and prepares it for writing the
## the operations to the management file.
format_mgtschedule <- function(sdl_df, meta_data, lookup) {

  temp_sdl  <- filter(sdl_df, CROP == meta_data$LUSE)
  temp_sdl[temp_sdl == ""] <- NA
  n_op <- dim(temp_sdl)[1]

  temp_sdl <- temp_sdl[rep(seq(1, n_op), lookup$n_years),]
  temp_sdl %<>%  mutate(.,
                        YEAR = seq(lookup$bound_yrs[1],
                                   lookup$bound_yrs[2]) %>%
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

  temp_sdl <- rbind(filter(temp_sdl, OPERATION == "Initial crop")[1,],
                    filter(temp_sdl, OPERATION != "Initial crop"))
  temp_sdl <- filter(temp_sdl, !is.na(OPERATION))

  return(temp_sdl)
}
