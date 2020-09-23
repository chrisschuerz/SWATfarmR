#' Logistic function to calculate weight transition between boundaries.
#'
#' @param x Numeric value to calculate weight from
#' @param lwr Numeric value for lower boundary. If \code{x = lwr} the function returns ~0.01 as a weight.
#' @param upr Numeric value for upper boundary. If \code{x = upr} the function returns ~0.99 as a weight.
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom purrr map map_df
#' @importFrom readr read_lines
#' @importFrom stringr str_remove
#' @importFrom tibble add_column
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # All examples below must be included in the dynamic
#' # rules in the management csv file to be effective.
#'
#' # Low weight for days below 0 degC with a
#' # logistic transition from 5 degC to 0 degC:
#' tmp <- -5:10
#' weight <- logistic(tmp, 0, 5)
#' plot(tmp, weight)
#'
#' # Low weight for days with high precipitation
#' # (transition range: 2 to 10 mm)
#' pcp <- 0:12
#' weight <- (1 - logistic(pcp, 0, 5))
#' plot(pcp, weight)
#'
#' # Harvesting crop in optimum PHU fraction range 1 to 1.1
#' hu_fr <- seq(0.75, 1.4, 0.01)
#' weight <- logistic(hu_fr, 0.9, 1) * (1 - logistic(hu_fr, 1.1, 1.2))
#' }
#'
w_log <- function(x, lwr, upr) {
  k   <- 9.19024 / (upr - lwr)
  x_0 <- upr/2 + lwr/2
  1 / (1 + exp(-k*(x - x_0)))
}


#' Linear function to calculate weight transition between boundaries.
#'
#' @param x Numeric value to calculate weight from
#' @param lwr Numeric value for lower boundary. If \code{x <= lwr} the function returns 0 as a weight.
#' @param upr Numeric value for upper boundary. If \code{x >= upr} the function returns 1 as a weight.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # All examples below must be included in the dynamic
#' # rules in the management csv file to be effective.
#'
#' # Low weight for days below 0 degC with a
#' # linear transition from 5 degC to 0 degC:
#' tmp <- -5:10
#' weight <- linear(tmp, 0, 5)
#' plot(tmp, weight)
#'
#' # Low weight for days with high precipitation
#' # (transition range: 2 to 10 mm)
#' pcp <- 0:12
#' weight <- (1 - linear(pcp, 0, 5))
#' plot(pcp, weight)
#'
#' # Harvesting crop in optimum PHU fraction range 1 to 1.1
#' hu_fr <- seq(0.75, 1.4, 0.01)
#' weight <- linear(hu_fr, 0.9, 1) * (1 - linear(hu_fr, 1.1, 1.2))
#' }
#'
w_lin <- function(x, lw_bd, up_bd) {
  (x >= up_bd) + (x < up_bd & x > lw_bd) * (x - lw_bd) / (up_bd - lw_bd)
}

#' Cumulative exponential decay.
#'
#' This function can be used to calculate new farmR variables that account for
#' events occurring \code{n_step} to 0 time steps before, or 0 to \code{n_step}
#' time steps after a certain reference point in time (\code{t_0}).
#' Decreasing weights are assigned to the considered time steps before or after
#' the reference point using an exponential decay function with a certain
#' \code{decay_rate}.
#'
#' Application examples can be the consideration of antecedent precipitation,
#' where recent events are more relevant than events that are several days ago
#' (can act as a proxy for soil moisture).
#'
#' @param variable Tibble of input variable were each columns is a subbasin and
#'   each row is a time step. The variables from the farmR object can also be used as is.
#' @param n_steps Numeric value for the number of times steps before or after
#'   the reference point. Negative values consider time steps before and positive
#'   values after.
#' @param decay_rate Numeric value that controls the rate of decay for the values
#' of the time steps before or after.
#'
#' @importFrom purrr map_df
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Unit input, varying decay rates:
#' x <- data.frame(x_1 = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0))
#' y1<- variable_decay(x, -10, 1)
#' y2<- variable_decay(x, -10, 0.8)
#' y3<- variable_decay(x, -10, 0.25)
#'
#' plot(y1$x_1, type = "l", col = 1)
#' lines(y2$x_1, col = 2)
#' lines(y3$x_1, col = 3)
#'
#' # Antecedent precipitation index with 5 time steps and a decay rate of 0.8:
#' pcp <- data.frame(pcp_1 = c(0,3,7,10,2,0,0,5,2,0))
#' time_steps <- -5
#' rate <- 0.8
#' api <- variable_decay(pcp, time_steps, rate)
#' plot(pcp$pcp_1, ylim = c(0,15), pch = 3)
#' lines(api$pcp_1)
#' }
#'
variable_decay <- function(variable, n_steps, decay_rate) {
  if(all(names(variable)[1:4] == c("year", "month", "day", "jdn"))) {
    variable <- variable[,5:ncol(variable)]
  }
  map_df(variable, ~exp_decay(.x, n_steps, decay_rate))
}

#' Cumulative exponential for a vector x.
#'
#' @param x Input vector fro which cummulative decay is calculated.
#' @param n Numeric value for the number of steps before or after the reference point.
#' @param l Numeric value for the decay rate.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map_dbl
#'
#' @keywords internal
#'
exp_decay <- function(x, n, l) {
  if(n < 0 ) {
    n_section <- map(1:length(x), ~ x[max((.x + n),0):.x])
    n_decay <- decay_i(seq(-n, 0, -1), l)
    x_decay <- map(n_section, ~ .x*n_decay[((-n) + 2 - length(.x)):((-n) + 1)]) %>%
      map_dbl(., sum)
  } else {
    n_section <- map(1:length(x), ~ x[.x:(.x + n)])
    n_decay <- decay_i(seq(0, n, 1), l)
    x_decay <- map(n_section, ~ .x*n_decay) %>%
      map_dbl(., ~ sum(.x, na.rm = TRUE))
  }
  return(x_decay)
}

#' Calculate decay weights.
#'
#' @param n Numeric value for the number of steps before or after the reference point.
#' @param l Numeric value for the decay rate.
#'
#' @keywords internal
#'
decay_i <- function(n, l) {
  exp(-n*l)
}
