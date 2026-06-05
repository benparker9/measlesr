.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "measlesr ", utils::packageVersion("measlesr"), "\n",
    "Time series WHO measles data and analysis. Use read_me() to get started."
  )
}
