#' Identify the SWAT version of the project path
#'
#' @param project_path Text string path to the project TxtInOut folder
#'
#' @importFrom readr read_lines
#' @importFrom stringr str_detect
#'
#' @keywords internal
#'
check_version <- function(project_path) {
  file_cio <- read_lines(project_path%//%'file.cio', lazy = FALSE)
  swat_version <- c(0,0)
  if (length(file_cio > 70)) {
    swat_version[1] <- swat_version[1] + 1
  } else {
    swat_version[2] <- swat_version[2] + 1
  }
  if (any(str_detect(file_cio, 'General Information/Watershed Configuration'))) {
    swat_version[1] <- swat_version[1] + 1
  } else {
    swat_version[2] <- swat_version[2] + 1
  }
  if (any(str_detect(file_cio, 'Reach output variables:'))) {
    swat_version[1] <- swat_version[1] + 1
  } else {
    swat_version[2] <- swat_version[2] + 1
  }
  if (any(str_detect(file_cio, 'simulation'))) {
    swat_version[2] <- swat_version[2] + 1
  } else {
    swat_version[1] <- swat_version[1] + 1
  }
  if (any(str_detect(file_cio, 'connect'))) {
    swat_version[2] <- swat_version[2] + 1
  } else {
    swat_version[1] <- swat_version[1] + 1
  }
  if (any(str_detect(file_cio, 'routing_unit'))) {
    swat_version[2] <- swat_version[2] + 1
  } else {
    swat_version[1] <- swat_version[1] + 1
  }
  if(swat_version[1] > 4) {
    return('2012')
  } else if (swat_version[2] > 4) {
    return('plus')
  } else {
    stop('Cannot identify the SWAT version of the project')
  }
}


#' Display the progress if iterative processes
#'
#' @param n Iteration step
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now seconds
#' @keywords internal
#'
display_progress_hru <- function(id, n, nmax, t0){
  t1 <- now()
  time_elaps  <- interval(t0,t1) %>%
    round(.) %>%
    as.period(.)
  time_remain <- (as.numeric(time_elaps, "seconds")*(nmax-n)/n) %>%
    round(.) %>%
    seconds(.) %>%
    as.period(., unit = "days")

  cat("\r", "HRU", id, paste0("(", n, " of ", nmax, ')'),
      "  Time elapsed:", as.character(time_elaps),
      "  Time remaining:", as.character(time_remain),
      "   ")
}

#' Display the progress if iterative processes as percentage value
#'
#' @param n Iteration step
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now seconds
#' @keywords internal
#'
display_progress_pct <- function(n, nmax, t0){
  t1 <- now()
  time_elaps  <- interval(t0,t1) %>%
    round(.) %>%
    as.period(.)
  time_remain <- (as.numeric(time_elaps, "seconds")*(nmax-n)/n) %>%
    round(.) %>%
    seconds(.) %>%
    as.period(., unit = "days")
  prog <- paste0(round(100*n/nmax), "%")
  cat("\r", "Progress:", prog,
      "  Time elapsed:", as.character(time_elaps),
      "  Time remaining:", as.character(time_remain),
      "   ")
}

#' Print message for completed process
#'
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now
#' @keywords internal
#'
finish_progress <- function(nmax, t0, word1, word2) {
  cat("\r", paste0(rep(" ", 75), collapse = ""))
  interval(t0,now()) %>%
    round(.) %>%
    as.period(.) %>%
    as.character(.) %>%
    cat("\r",word1, nmax, word2%&%plural(nmax), "in", ., "\n")
}

#' Add plural 's' to the written message if multiple operations done
#'
#' @param n Interger number of operations
#' @keywords internal
#'
plural <- function(n) {
  ifelse(n <= 1, "", "s")
}

#' Concatenate with an underscore
#'
#' \%_\% pastes two strings by "_".
#' @keywords internal
"%_%" <- function(a, b) paste(a, b, sep = "_")

#' Concatenate with a hyphen
#'
#' \%-\% pastes two strings by "-".
#' @keywords internal
'%-%' <- function(a, b) paste(a, b, sep = "-")

#' Concatenate with a dot
#'
#' \%.\% pastes two strings by ".".
#' @keywords internal
'%.%' <- function(a, b) paste(a, b, sep = ".")

#' Paste slash function
#'
#' \%//\% pastes two strings by "/".
#' @keywords internal
'%//%' <- function(a, b) paste(a, b, sep = "/")

#' Concatenate without separator
#'
#' \%&\% pastes two strings by "".
#' @keywords internal
'%&%' <- function(a, b) paste0(a, b)

#' Concatenate with space
#'
#' \%&&\% pastes two strings by " ".
#' @keywords internal
'%&&%' <- function(a, b) paste(a, b, sep = " ")

#' Set colnames of a data frame or matrix
#'
#' @param x Data frame or matrix
#' @param nm Character names vector
#'
#' @keywords internal
#'
set_colnames <- function(x, nm) {
  colnames(x) <- nm
  return(x)
}
