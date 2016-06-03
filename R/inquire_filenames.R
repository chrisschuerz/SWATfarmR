## inquire_filenames(pat_str) ----------------------------------------------------
## Function lists all the names of files in the TxtIO directory with a specific
## string pattern. It removes all output files from the list.
inquire_filenames <- function(file_pattern, file_path) {
  filenames_list <- list.files(path = file_path, pattern = file_pattern)
  drop_elements  <- grep("output", file_list)
  filenames_list <- file_list[-drop_elements]
}
