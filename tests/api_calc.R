library(purrr)
library(tidyverse)

decay_cumsum <- function(data, decay_coeff, n_steps) {
  if (n_steps < 0) {
    w <- decay_coeff^seq(-(n_steps + 1), 0)
    idx <- seq(-n_steps + 1, length(data))
    dec_cum <- map_dbl(idx, ~ sum(data[(.x+n_steps):(.x - 1)]*w))
    dec_cum <- c(rep(0 , - n_steps), dec_cum)
  } else {
    w <- decay_coeff^seq(0, n_steps - 1)
    idx <- seq(1, length(data) - n_steps)
    dec_cum <- map_dbl(idx, ~ sum(data[(.x + 1):(.x + n_steps)]*w))
    dec_cum <- c(dec_cum, rep(0 , n_steps))
  }
  return(dec_cum)
}

api7 <- raab$.data$weather$pcp %>%
  select(starts_with("pcp")) %>%
  apply(., 2, decay_cumsum, decay_coeff = 0.8, n_steps = -7)

#idea of add_variable.... implementation in R6 object
