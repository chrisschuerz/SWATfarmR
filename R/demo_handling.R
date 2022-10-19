#' Loading SWAT demo data for testing \code{SWATfarmR}
#'
#' With this function you can load data sets for SWAT2012 and SWAT+ demo projects.
#' The provided data includes demo projects, and management schedule .csv files.
#'
#' @param dataset Character string to define data set to load. Valid inputs are
#'   \code{dataset = c('project', 'schedule')}.
#'   \code{dataset = 'project'} loads a SWAT demo project in the defined
#'   \code{path} location and returns the final project path as a text string in
#'   R. The definition of the \code{version} is required.
#'   \code{dataset = 'schedule'} writes a demo management schedule .csv file for the
#'   demo model setup in the \code{path} location and returns the file path as a text string.
#' @param path Character string that defines the path where to copy the SWAT demo
#'   project or the demo managment schedule.
#' @param version Character string that defines the SWAT version. Options
#'   are \code{version = c('2012', 'plus')}. This argument is required to
#'   load SWAT projects and shape files.
#'
#' @section Examples:
#'   To learn how to load the different demo data sets with \code{SWATfarmR} see the
#'   section \href{https://chrisschuerz.github.io/SWATfarmR/articles/SWATfarmR.html#loading-the-demo-data}{Loading the demo data}
#'   on the package's *Get Started* page.
#'
#' @importFrom stringr str_sub
#'
#' @export

load_demo <- function(dataset, path = NULL, version = NULL) {

  if(!("SWATdata" %in% installed.packages()[,1])) {
    choice <- select.list(choices = c("install", "cancel"),
      title = paste("Loading demo data requires the R package 'SWATdata'.",
                    "Should the package now be installed?",
                    "Type '1' to install or '2' to cancel:"))
    if(choice == "install") {
      if(!("devtools" %in% installed.packages()[,1])){
        install.packages("devtools")
      }
      suppressWarnings(remotes::install_github("chrisschuerz/SWATdata"))
    }
  }
  sdat_path <- system.file(package = "SWATdata")
  frmr_path <- system.file(package = "SWATfarmR")

  dataset <- tolower(dataset) %>% str_sub(., 1, 3)
  if(!(dataset %in% c("pro", "sch"))) {
    stop("Invalid selection for dataset!")
  }

  if(is.null(version)) {
    stop("Loading a dataset = '"%&%dataset%&%"' requires a SWAT 'version'.")
  }

  version <- tolower(version)
  if(version == "+")  version <- "plus"
  if(version == 2012) version <- "2012"

  if(!(version %in% c("2012", "plus"))) {
    stop("Invalid value for 'version'. Must be one of: '2012', 'plus', '+'.")
  }

  if(dataset == "pro") {

    os <- get_os()
    demo_files <- list.files(sdat_path%//%"extdata")

    revision <- demo_files %>%
      .[grepl(version, .)] %>%
      .[grepl(os, .)] %>%
      str_sub(., 6, nchar(.)) %>%
      gsub("[^[:digit:]]", "",.) %>%
      as.numeric(.) %>%
      max(.)

    swat_exe <- version%_%"rev"%&%revision%_%os%.%"zip"
    swat_project <- version%_%"rev"%&%revision%_%"project"%.%"zip"

    if(!(swat_exe %in% demo_files)) {
      stop("There is no SWAT"%&%version%&&%"Rev."%&%revision%&&%
           "project available for '"%&%os%&%"'.")
    }

    if(is.null(path)) {
      stop("Loading a SWAT demo project requires a 'path'.")
    }
    path <- gsub("\\/$", "", path)
    demo_path <- path%//%"swat"%&%version%_%"rev"%&%revision%_%"demo"

    if(dir.exists(demo_path)) {
      warning("Demo already exists in provided 'path'."%&&%
              "To work with the existing demo project use the returned path.")
      return(demo_path)
    } else {
      add_slash <- ifelse(grepl("\\:$", path), "/", "")
      unzip(zipfile = sdat_path%//%"extdata"%//%swat_project,
            exdir = path%&%add_slash)
      unzip(zipfile = sdat_path%//%"extdata"%//%swat_exe,
            exdir = demo_path)
      if(os == "unix"){
        system("chmod -R 777"%&&%demo_path%//%"swat"%&%version%_%"rev"%&%revision)
      }
      return(demo_path)
    }
  }

  if(dataset == "sch") {
    if (version == '2012') {
      stop('No demo schedule file for SWAT2012 included yet!')
    }

    file.copy(frmr_path%//%"extdata"%//%dataset%_%version%.%"csv",
              path%//%'demo_schedule'%_%version%.%'csv')

    return(path%//%'demo_schedule'%_%version%.%'csv')
  }
}

#' Identify the operating system.
#' The function was written by Will Lowe and was copied from here:
#' https://conjugateprior.org/2015/06/identifying-the-os-from-r/
#'
#' @keywords internal
#'
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin') {
      os <- "osx"
    }
    if (os == 'Windows') {
      os <- "win"
    }
    # If rare case occurs that Sys.info() is NULL
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}

