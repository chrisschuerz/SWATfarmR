## format_mgtschedule(sdl_df, meta_data, n_yrs) -----------------------------
## Function takes the management schedule file and prepares it for writing the
## the operations to the management file.
format_mgtschedule <- function(input_lst, meta_lst, mgtcnop_sel) {

  sdl <- input_lst$mgt_cnop[["mgt"%_%mgtcnop_sel]] %>%
    filter(ROTATION == meta_lst$LUSE)
  sdl[sdl == ""] <- NA
  n_yrs <- sdl %>%
    filter(OPERATION == "End of year" | OPERATION == "Skip") %>%
    nrow
  n_times <- ceiling(input_lst$lookup$n_years/n_yrs)
  sdl <- do.call("rbind", replicate(n_times, sdl, simplify = FALSE))
  end_pos <- which(sdl$OPERATION == "End of year" |
                   sdl$OPERATION == "Skip") %>%
    .[1:input_lst$lookup$n_years]
  sdl <- sdl[1:end_pos[input_lst$lookup$n_years],]

  sdl %<>%  mutate(.,
                        YEAR = seq(input_lst$lookup$bound_yrs[1],
                                   input_lst$lookup$bound_yrs[2]) %>%
                          rep(. , c(end_pos[1], diff(end_pos))),
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
    select(., CROP, YEAR, JDN1, JDN2, MON_1, MON_2, DATE_RULE,
           OPERATION, starts_with("MGT"))

  sdl <- rbind(filter(sdl, OPERATION == "Initial crop")[1,],
                    filter(sdl, OPERATION != "Initial crop"))
  sdl <- filter(sdl, !is.na(OPERATION))

  return(sdl)
}
