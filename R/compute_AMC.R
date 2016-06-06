# compute_AMC(wtr_bal, k_ret) -------------------------------------------
# Function takes time series of water balance (P - ET0) and estimates the
# antecedent soil moisture (ACM) for each subbasin and respective hydrologic
# soil group (A to D).
compute_AMC <- function (wtr_bal, k_ret, n_prevday) {

  if(length(k_ret) != 4) {stop("Length of k_ret not equal to 4!")}

  ant_moist <- list()

  ant.days <- function(ind, var_ts, n_days){
    var_fill <- NULL
    if(ind <= n_days){
      var_fill   <- rep(0, (n_days+1) - ind)
      n_days <- ind - 1
    }
    var_ant <- c(var_fill,var_ts[(ind - n_days):ind])
    return(var_ant)
  }

  ant.bal.sum <- function(ant_bal_i, k){
    n_day <- seq(length(ant_bal_i) -1, 0, -1)
    k_day <- k^n_day
    sum(ant_bal_i*k_day)
  }

  for (i in 1:4){
    ant_bal_i <- t(sapply(1:length(wtr_bal), ant.days,
                          wtr_bal, n_prevday, simplify = TRUE))
    ant_moist_i <- apply(ant_bal_i, 1, ant.bal.sum, k = k_ret[i])
    ant_moist_i[ant_moist_i < 0] <- 0
    ant_moist[[LETTERS[i]]] <- ant_moist_i
  }
  return(ant_moist)
}

# compute_ET0Hargreaves(t_min, t_max, sub_info) ---------------------------
compute_ET0Hargreaves <- function (t_min, t_max, lookup) {
  # Calculate ET values applying FAO-56 Hargreaves (Allen et al., 1998)
  # Estimate Ra according to Allen at al., 1998
  # (http://www.fao.org/docrep/x0490e/x0490e07.htm#calculation%20procedures)
  G_sc <- 0.0820
  delta <- 0.409*sin(2*pi*t_min$JDN/365 - 1.39)
  d_r <- 1 + 0.033*cos(2*pi*t_min$JDN/365)

  t_min  %<>% select(., starts_with("SUB"))
  t_max  %<>% select(., starts_with("SUB"))

  R_a <- data.frame(matrix(data = NA,
                           nrow = dim(t_min)[1],
                           ncol = lookup$n_subbasin))
  for(i in 1:lookup$n_subbasin){
    phi <- lookup$station$I_LAT [i]*pi/180
    w_s <- acos(-tan(phi)*tan(delta))
    R_a[i] <- 24*60/pi*0.408*G_sc*d_r*(w_s*sin(phi)*sin(delta) +
                                         cos(phi)*cos(delta)*sin(w_s))
  }

  # Estimate ET0 according to Allen at al., 1998
  # http://www.fao.org/docrep/x0490e/x0490e07.htm#an%20alternative%20equation
  # %20for%20eto%20when%20weather%20data%20are%20missing

  et0 <- 0.0023*((t_min + t_max)/2 + 17.8)* (t_max - t_min)^0.5 * R_a

  et0 <- rename.col(et0, "SUB"%_%sprintf("%03d",1:lookup$n_subbasin))
  return(et0)
}
