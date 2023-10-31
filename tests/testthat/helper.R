os_type <- function() {
  tolower(.Platform$OS.type)
}

os_name <- function() {
  tolower(Sys.info()[["sysname"]])
}

os_arch <- function() {
  tolower(paste0(os_name(), "-", R.Version()$arch))
}
