#' Provides context on the functions within the package for easy terminal use.     
#'
#' Lists the functions available within the package.     
#'
#' @return A text paragraph of each function within the package. 
#' @export
#'
#' @examples
#' read_me()

read_me <- function() {
  cat("Welcome to measlesr!", "\n", 
      "This package provides time series measles/rubella data across the world from 2012-2025.", "\n",
      "See https://github.com/benparker9/measlesr/tree/master/vignettes for function use cases.", "\n",
      "We've provided 6 functions for users:", "\n",
      "1. load_data() -> loads the dataset provided by the WHO", "\n",
      "2. measles_snapshot() -> returns the top/bottom 10 countries per year by total measles cases", "\n",
      "3. lab_coverage() -> returns the top/bottom 10 countries per year by lab confirmed to total measles cases"

  )
}
