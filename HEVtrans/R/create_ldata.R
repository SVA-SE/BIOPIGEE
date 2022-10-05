##' Determine the number of pens in a farm
##' @noRd
number_of_pens_in_farm <- function(farm) {
    if (farm$farm_type == "SL")
        return(1)

    sum(farm$size_ges_sector * farm$size_ges_room,
        farm$size_fa_sector  * farm$size_fa_room,
        farm$size_pw_sector  * farm$size_pw_room,
        farm$size_fi_sector  * farm$size_fi_room)
}

##' Count the total number of pens in all farms
##' @noRd
number_of_pens <- function(farms) {
    sum(vapply(farms, number_of_pens_in_farm, numeric(1)))
}

##' Create ldata
##'
##' Create ldata representing the node's neighbours used for the
##' environmental and the parameters specific to the farm type
##'
##' @param farms List of farms
##' @return a matrix with left, right, lat, beta, betaEWP, betaEBAP, cleaning_rate, 
##'     Dinf, shedRate 
##' @noRd
create_ldata <- function(farms) {
    sectors <- c("ges", "fa", "pw", "fi")

    variables <- c("left", "right", "lat", "beta", "betaEWP",
                   "betaEBAP", "cleanRate","Dinf", "shedRate", "renewalrate", "extBioSec", "farm",
                   "slaughterhouse")

    npens <- number_of_pens(farms)

    ## Initialize ldata to zero (data is recycled).
    ldata <- matrix(data = 0, nrow = length(variables), ncol = npens,
                    dimnames = list(variables, NULL))

    offset <- 0
    for (i in seq_len(length(farms))) {
        farm <- farms[[i]]

        ## Add sows in each pen in the gestation sector.
        if (farm$farm_type == "SL") {
            l <- seq_len(number_of_pens_in_farm(farm))
            ldata["farm", l + offset] <- farm$farm_id
            ldata["slaughterhouse", l + offset] <- 1
            offset <- offset + length(l)
        } else {
            
            ## For each sector
            for (j in seq_len(length(sectors))) {
                sector <- sectors[j]
                
                ## For each room
                for (k in seq_len(farm[[paste0("size_", sector, "_sector")]])) {
                    
                    ## List pens
                    l <- seq_len(farm[[paste0("size_", sector, "_room")]])
                    
                    ## If no empty
                    if (length(l) > 0) {
                        ## All pens except the first one have an
                        ## adjacent pen on the left.
                        ldata["left", l + offset] <- 1
                        ldata["left", 1 + offset] <- 0

                        ## All pens except the last one have an
                        ## adjacent pen on the right.
                        ldata["right", l + offset] <- 1
                        ldata["right", length(l) + offset] <- 0

                        ldata["lat", l + offset] <- 1 / farm$Dlat
                        ldata["beta", l + offset] <- farm[[paste0("beta_D_", sector, "_sector")]]
                        ldata["betaEWP", l + offset] <- farm[[paste0("beta_EWP_", sector, "_sector")]]
                        ldata["betaEBAP", l + offset] <- farm[[paste0("beta_EBAP_", sector, "_sector")]]
                        ldata["cleanRate", l + offset] <- farm[[paste0("cleaning_rate_", sector)]]
                        ldata["Dinf", l + offset] <- 1 / farm$Dinf
                        ldata["shedRate", l + offset] <- farm$shedrate
                        ldata["renewalrate", l + offset] <- farm$renewalrate
                        ldata["extBioSec", l + offset] <- farm[[paste0("ext_BioSec_", sector, "_sector")]]
                        ldata["farm", l + offset] <- farm$farm_id

                        offset <- offset + length(l)
                    }
                }
            }
        }
    }

    ldata
}
