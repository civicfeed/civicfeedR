#' NULL to NA
#'
#' This is a helper function that converts NULL to NA.
#'
#' @keywords internal
toNA = function(x) {
  if (is.null(x)) {
    x = NA
  }
  x
}
