##' Define initial population vectors
##'
##' @description \code{create_u0}  Define initial health state of each sector depending on farm type
##' @usage create_u0(farms)
##'
##' @param farms List of farms including farm type
##' @return A matrix with the number of individuals per health state
##'     per pen.
##' @noRd
create_u0 <- function(farms) {
    compartments <- c("S1","E1","I1","R11","R12","R13","R14","R15","R16","M2",
                      "S2","E2","I2","R21","R22","R23","R24","R25","R26")

    npens <- number_of_pens(farms)

    ## Initialize the count in each compartment to zero (data is
    ## recycled).
    u0 <- matrix(data = 0, nrow = length(compartments), ncol = npens,
                 dimnames = list(compartments, NULL))

    offset <- 0
    for (i in seq_len(length(farms))) {
        farm <- farms[[i]]

        ## Add sows in each pen in the gestation sector.
        if (farm$farm_type != "SL") {
            j <- seq_len(farm$size_ges_sector * farm$size_ges_room)
            if (length(j) > 0)
                u0["S1", j + offset] <- farm$size_ges_pen
        }

        offset <- offset + number_of_pens_in_farm(farm)
    }

    u0
}
