##' Define the initial continuous state vector
##'
##' The continuous state vector 'v' contains the environmental
##' contamination in each pen and the clean state.
##'
##' @param farms List of farms.
##' @return \code{matrix} with the initial continuous state vector per
##'     pen.
##' @noRd
create_v0 <- function(farms) {
    matrix(data = 0,
           nrow = 2,
           ncol = number_of_pens(farms),
           dimnames = list(c("phi", "clean"), NULL))
}
