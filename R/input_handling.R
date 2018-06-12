#' Read a weather input file
#'
#' @param file Path string to weather input file.
#' @param var  Character vector that defines the variables
#' @param skip Integer to skip the header
#' @param digit_var Integer, number of digits per variable
#' @param digit_date Vector of length 2 Integers give digits of year and jdn

#' @importFrom readr cols fwf_widths read_fwf
#' @importFrom dplyr mutate select everything
#' @importFrom lubridate month day
#' @importFrom magrittr %>%
#'
#' @return Generates a new farmr_project in the working environment (as an R6
#'   object) and saves the project the TxtInOut folder.

read_weather <- function(file, var, skip, digit_var, digit_date) {
  n_var <-  (nchar(readLines(file, n = (skip + 1))[(skip + 1)]) -
               sum(digit_date)) / digit_var

  cols <- fwf_widths(c(digit_date[1],digit_date[2], rep(digit_var,n_var)),
                     col_names = c("year", "jdn", rep(var, n_var/length(var))%_%
                                   rep(1:(n_var/length(var)), each = length(var))))

  read_fwf(file = file, col_positions = cols,
           col_types = cols(.default = "d", year = "i", jdn = "i"),
           skip = 4) %>%
    mutate(date  = as.Date(jdn%//%year, "%j/%Y"),
           month = month(date),
           day   = day(date)) %>%
    select(-date) %>%
    select(year, month, day, jdn, everything())
}
